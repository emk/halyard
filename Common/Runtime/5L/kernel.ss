(module 5L-Kernel (lib "lispish.ss" "5L")

  ;; Import %call-5l-prim from the engine.
  (require #%fivel-engine)

  ;; Get begin/var, and re-export it.
  (require (lib "begin-var.ss" "5L"))
  (provide begin/var)
  (provide define/var)
  
  ;; Get hooks, and re-export them.
  (require (lib "hook.ss" "5L"))
  (provide (all-from (lib "hook.ss" "5L")))

  ;; Get format-result-values.
  (require (lib "trace.ss" "5L"))


  ;;=======================================================================
  ;;  Built-in Types
  ;;=======================================================================
  ;;  These methods implement various "built-in" types that are known to
  ;;  the 5L engine.  They should *never* raise errors, because they're
  ;;  called directly from C++ code that isn't prepared to cope with Scheme
  ;;  errors.

  (provide (rename point <point>) (rename make-point point) point?
           point-x set-point-x! point-y set-point-y!

           (rename rect <rect>) (rename make-rect rect) rect?
           rect-left set-rect-left! rect-top set-rect-top!
           rect-right set-rect-right! rect-bottom set-rect-bottom!

           (rename color <color>) (rename make-color-opt-alpha color) color?
           color-red set-color-red! color-green set-color-green!
           color-blue set-color-blue! color-alpha set-color-alpha!

           (rename percent <percent>) (rename make-percent percent)
           percent? percent-value)
  
  (define-struct point (x y) (make-inspector))

  (define-struct rect (left top right bottom) (make-inspector))

  (define-struct color (red green blue alpha) (make-inspector))

  (define (make-color-opt-alpha r g b &opt (a 0))
    (make-color r g b a))

  (define-struct percent (value) (make-inspector))
  

  ;;=======================================================================
  ;;  Assertions
  ;;=======================================================================

  (provide assert)

  (define (%kernel-assert label value)
    (when (not value)
      (fatal-error (cat "Assertion failure: " label))))
  
  (define-syntax assert
    (syntax-rules ()
      [(assert cond)
       (%kernel-assert 'cond cond)]))


  ;;=======================================================================
  ;;  Utility Functions
  ;;=======================================================================

  (provide member? value->string cat label with-errors-blocked)
  
  (define (member? item list)
    (if (null? list)
        #f
        (if (equal? item (car list))
            #t
            (member? item (cdr list)))))
  
  (define (value->string value)
    (if (string? value)
        value
        (let ((str-port (open-output-string)))
          (write value str-port)
          (get-output-string str-port))))
  
  (define (cat . values)
    (if (not (null? values))
        (string-append (value->string (car values)) (apply cat (cdr values)))
        ""))

  (define-syntax label
    (syntax-rules ()
      [(label name body ...)
       (call-with-escape-continuation (lambda (name)
                                        (begin/var body ...)))]))

  (define (call-with-errors-blocked report-func thunk)
    (let* ((result (with-handlers ([void (lambda (exn) (cons #f exn))])
                     (cons #t (thunk))))
           (good? (car result))
           (exn-or-value (cdr result)))
      (if good?
          exn-or-value
          (begin
            (report-func (exn-message exn-or-value))
            #f))))

  (define-syntax with-errors-blocked
    (syntax-rules ()
      [(with-errors-blocked (report-func) body ...)
       (call-with-errors-blocked report-func
                                 (lambda () (begin/var body ...)))]))

  (define-syntax with-values
    (syntax-rules ()
      [(with-values values expr body ...)
       (call-with-values (lambda () expr) (lambda values body ...))]))
             

  ;;=======================================================================
  ;;  Standard Hooks
  ;;=======================================================================
  
  (provide *enter-card-hook* *exit-card-hook*
           *card-body-finished-hook* *before-draw-hook*)

  ;; Called before running each card.
  (define *enter-card-hook* (make-hook 'enter-card))

  ;; Called immediately before moving to a new card.
  (define *exit-card-hook* (make-hook 'exit-card))

  ;; Called after running the body of a each card, if the body exits
  ;; normally (not by jumping, etc.).
  (define *card-body-finished-hook* (make-hook 'card-body-finished))

  ;; Called before *most* screen redraws.
  (define *before-draw-hook* (make-hook 'before-draw))


  ;;=======================================================================
  ;;  Internal State
  ;;=======================================================================
  ;;  When the interpreter returns from a primitive call (including a
  ;;  primitive call to run the idle loop), it can be in one of a number of
  ;;  states:
  ;;
  ;;    NORMAL:   The interpreter is running code normally, or has no code
  ;;              to run.  The default.
  ;;    STOPPING: The kernel is about to stop.  We use a two-phase stop
  ;;              system--STOPPING indicates we should escape to the
  ;;              top-level loop, and STOPPED indicates that we should
  ;;              efficiently busy-wait at the top-level.
  ;;    STOPPED:  The kernel has been stopped.
  ;;    PAUSED:   The interpreter should pause the current card until it is
  ;;              told to wake back up.
  ;;    JUMPING:  The interpreter should execute a jump.
  ;;    NAPPING:  The interpreter should pause the current card for the
  ;;              specified number of milliseconds.
  ;;    CARD-KILLED: The interpreter should stop executing the current
  ;;               card, and return to the top-level loop.
  ;;    INTERPRETER-KILLED: The interpreter should exit.
  ;;
  ;;  Callbacks are slightly special, however--see the code for details.
  
  (provide call-at-safe-time)

  (define *%kernel-running-callback?* #f)
  (define *%kernel-state* 'NORMAL)
  
  (define *%kernel-jump-card* #f)
  (define *%kernel-timeout* #f)
  (define *%kernel-timeout-thunk* #f)
  (define *%kernel-nap-time* #f)

  (define *%kernel-running-deferred-thunks?* #f)
  (define *%kernel-deferred-thunk-queue* '())
  
  (define *%kernel-exit-interpreter-func* #f)
  (define *%kernel-exit-to-top-func* #f)
  
  (define (%kernel-die-if-callback name)
    (if *%kernel-running-callback?*
        (throw (cat "Cannot call " name " from within callback."))))

  (define (%kernel-clear-timeout)
    (set! *%kernel-timeout* #f)
    (set! *%kernel-timeout-thunk* #f))

  (define (%kernel-set-timeout time thunk)
    (when *%kernel-timeout*
      (debug-caution "Installing new timeout over previously active one."))
    (set! *%kernel-timeout* time)
    (set! *%kernel-timeout-thunk* thunk))
  
  (define (%kernel-check-timeout)
    (if (%kernel-stopped?)
        (%kernel-clear-timeout)
        (when (and *%kernel-timeout*
                   (>= (current-milliseconds) *%kernel-timeout*))
          (let ((thunk *%kernel-timeout-thunk*))
            (%kernel-clear-timeout)
            (thunk)))))

  (define (%kernel-safe-to-run-deferred-thunks?)
    ;; Would now be a good time to run deferred thunks?  Wait until
    ;; nothing exciting is happening.
    (and (not *%kernel-running-callback?*)
         (not *%kernel-running-deferred-thunks?*)
         (member? *%kernel-state* '(NORMAL PAUSED NAPPING))))

  (define (call-at-safe-time thunk)
    ;; Make sure we run 'thunk' at the earliest safe time, but not
    ;; in a callback.  This can be used to defer calls to video, input,
    ;; and other functions which can't be called from a callback.
    (if (%kernel-safe-to-run-deferred-thunks?)
        (thunk)
        (set! *%kernel-deferred-thunk-queue*
              (cons thunk *%kernel-deferred-thunk-queue*)))
    #f)
    
  (define (%kernel-check-deferred)
    ;; If the interpreter has stopped, cancel any deferred thunks.
    (when (%kernel-stopped?)
      (set! *%kernel-deferred-thunk-queue* '()))   

    ;; Run any deferred thunks.
    (unless (or (null? *%kernel-deferred-thunk-queue*)
                (not (%kernel-safe-to-run-deferred-thunks?)))

      ;; Make a copy of the old queue and clear the global variable
      ;; (which may be updated behind our backs.
      (let [[items (reverse *%kernel-deferred-thunk-queue*)]]
        (set! *%kernel-deferred-thunk-queue* '())

        ;; Run every thunk in the queue, in order.
        (fluid-let [[*%kernel-running-deferred-thunks?* #t]]
          (let loop [[items items]]
            (unless (null? items)
              ((car items))
              (loop (cdr items))))))

      ;; Check to see if any new items appeared in the queue while we
      ;; were running the first batch.
      (%kernel-check-deferred)))

  (define (%kernel-maybe-clear-state)
    (case *%kernel-state*
      [[STOPPING]
       (set! *%kernel-state* 'STOPPED)]
      [[STOPPED]
       #f]
      [else
       (set! *%kernel-state* 'NORMAL)])
    (set! *%kernel-jump-card* #f))
  
  (define (%kernel-run-as-callback thunk error-handler)
    (assert (not *%kernel-running-callback?*))
    (if (eq? *%kernel-state* 'INTERPRETER-KILLED)
        (5l-log "Skipping callback because interpreter is being shut down")
        (let [[saved-kernel-state *%kernel-state*]]
          (set! *%kernel-state* 'NORMAL)
          (label exit-callback
            ;; TODO - Can we have better error handling?
            (with-errors-blocked (error-handler)
              (fluid-let [[*%kernel-exit-to-top-func* exit-callback]
                          [*%kernel-exit-interpreter-func* exit-callback]
                          [*%kernel-running-callback?* #t]]
                (thunk))))
          (if (eq? *%kernel-state* 'NORMAL)
              (set! *%kernel-state* saved-kernel-state)))))

  (define (%kernel-set-state state)
    (set! *%kernel-state* state))

  (define (%kernel-check-state)
    (%kernel-check-deferred) ; Should be the first thing we do.
    (unless *%kernel-running-callback?*
      (%kernel-check-timeout))
    (case *%kernel-state*
      [[NORMAL STOPPED]
       #f]
      [[STOPPING]
       (when *%kernel-exit-to-top-func*
             (*%kernel-exit-to-top-func* #f))]
      [[PAUSED]
       (%call-5l-prim 'schemeidle)
       (%kernel-check-state)]
      [[NAPPING]
       (if (< (current-milliseconds) *%kernel-nap-time*)
           (begin
             (%call-5l-prim 'schemeidle)
             (%kernel-check-state))
           (%kernel-maybe-clear-state))]
      [[JUMPING]
       (when *%kernel-exit-to-top-func*
             (*%kernel-exit-to-top-func* #f))]
      [[CARD-KILLED]
       (when *%kernel-exit-to-top-func*
             (*%kernel-exit-to-top-func* #f))]
      [[INTERPRETER-KILLED]
       (*%kernel-exit-interpreter-func* #f)]
      [else
       (fatal-error "Unknown interpreter state")]))


  ;;=======================================================================
  ;;  Core 5L API
  ;;=======================================================================
  ;;  We only declare a small number of primitives here, typically those
  ;;  which are needed by the kernel or which intimately depend on the
  ;;  kernel's inner workings.  The rest of these functions can be found
  ;;  in the 5L-API module.
  
  (provide call-5l-prim have-5l-prim? idle 5l-log debug-log
           caution debug-caution non-fatal-error fatal-error
           engine-var set-engine-var! engine-var-exists?
           throw exit-script jump refresh)

  (define *32-bit-signed-min* -2147483648)
  (define *32-bit-signed-max* 2147483647)
  (define *32-bit-unsigned-min* 0)
  (define *32-bit-unsigned-max* 4294967295)

  (define (call-5l-prim . args)
    (let ((result (apply %call-5l-prim args)))
      (%kernel-check-state)
      result))
  
  (define (have-5l-prim? name)
    (call-5l-prim 'haveprimitive name))

  (define (idle)
    (%kernel-die-if-callback 'idle)
    (call-5l-prim 'schemeidle))
  
  (define (5l-log msg)
    (%call-5l-prim 'log '5L msg 'log))
  
  (define (debug-log msg)
    (%call-5l-prim 'log 'Debug msg 'log))
  
  (define (caution msg)
    (%call-5l-prim 'log '5L msg 'caution))
  
  (define (debug-caution msg)
    (%call-5l-prim 'log 'Debug msg 'caution))
  
  (define (non-fatal-error msg)
    (%call-5l-prim 'log '5L msg 'error))
  
  (define (fatal-error msg)
    (%call-5l-prim 'log '5L msg 'fatalerror))
  
  (define (engine-var name)
    (call-5l-prim 'get (if (string? name) (string->symbol name) name)))
  
  (define (set-engine-var! name value)
    (let [[namesym (if (string? name) (string->symbol name) name)]
          [type
           (cond
            [(void? value) 'NULL]
            [(string? value) 'STRING]
            [(symbol? value) 'SYMBOL]
            [(and (integer? value) (exact? value))
             (cond
              [(<= *32-bit-signed-min* value *32-bit-signed-max*) 'LONG]
              [(<= *32-bit-unsigned-min* value *32-bit-unsigned-max*) 'ULONG]
              [else (throw (cat "Cannot store " value " in " name
                                " because it does fall between "
                                *32-bit-signed-min* " and "
                                *32-bit-unsigned-max* "."))])]
            [(number? value) 'DOUBLE]
            [(or (eq? value #t) (eq? value #f)) 'BOOLEAN]
            [else (throw (cat "Cannot store " value " in " name "."))])]]
      (if (eq? type 'NULL)
          (call-5l-prim 'settyped namesym type)
          (call-5l-prim 'settyped namesym type value))))

  (define (engine-var-exists? name)
    (call-5l-prim 'VariableInitialized name))
  
  (define (throw msg)
    ;; TODO - More elaborate error support.
    (non-fatal-error msg)
    (error msg))
  
  (define (exit-script)
    (call-5l-prim 'schemeexit))
  
  (define (jump-to-card card)
    (if (have-5l-prim? 'jump)
        (call-5l-prim 'jump (card-name card))
        (begin
          ;; If we don't have a JUMP primitive, fake it by hand.
          (set! *%kernel-jump-card* (%kernel-find-card card))
          (%kernel-set-state 'JUMPING)
          (%kernel-check-state))))

  (define (refresh)
    (call-hook-functions *before-draw-hook*)
    (if (have-5l-prim? 'unlock)
        (call-5l-prim 'unlock)))


  ;;=======================================================================
  ;;  Object Model
  ;;=======================================================================

  ;;-----------------------------------------
  ;; Nodes

  (provide <node> node? node-name node-full-name node-parent find-node @)

  (defclass <node> ()
    name
    parent
    init-thunk)

  (defmethod (initialize (node <node>) initargs)
    (call-next-method)
    (when (node-parent node)
      (group-add-child! (node-parent node) node)))

  (define (node-full-name node)
    ;; Join together local names with "/" characters.
    (let [[parent (node-parent node)]]
      (if (and parent (not (eq? parent $root-node)))
        (string->symbol (cat (node-full-name (node-parent node))
                             "/" (node-name node)))
        (node-name node))))

  (define *node-table* (make-hash-table))

  (define (register-node node)
    (let [[name (node-full-name node)]]
      (when (hash-table-get *node-table* name (lambda () #f))
        (error (cat "Duplicate copies of node " node)))
      (hash-table-put! *node-table* name node)))

  (define (find-node name)
    (hash-table-get *node-table* name (lambda () #f)))

  (define (find-node-relative base name)
    (if (eq? base $root-node)
        (find-node name)
        (let* [[base-name (node-full-name base)]
               [candidate (string->symbol (cat base-name "/" name))]
               [found (find-node candidate)]]
          (or found (find-node-relative (node-parent base) name)))))

  (define-syntax @
    (syntax-rules ()
      [(@ name)
       (find-node-relative (node-parent (current-card)) 'name)]))

  (define (analyze-node-name name)
    ;; Given a name of the form '/', 'foo' or 'bar/baz', return the
    ;; node's parent and the "local" portion of the name (excluding the
    ;; parent).  A "/" character separates different levels of nesting.
    (if (eq? name |/|)
        (values #f |/|) ; Handle the root node.
        (let* [[str (symbol->string name)]
               [matches (regexp-match "^(([^/].*)/)?([^/]+)$" str)]]
          (cond
           [(not matches)
            (error (cat "Illegal node name: " name))]
           [(not (cadr matches))
            (values $root-node name)]
           [else
            (let [[parent (find-node (string->symbol (caddr matches)))]]
              (unless parent
                (error (cat "Parent of " name " does not exist.")))
              (values parent
                      (string->symbol (cadddr matches))))]))))

  ;;-----------------------------------------
  ;; Groups

  (provide <group> group? group-children)

  (defclass <group> (<node>)
    children)

  (defgeneric (group-add-child! (group <group>) (child <node>)))

  (defmethod (group-add-child! (group <group>) (child <node>))
    (set! (group-children group)
          (append (group-children group) (list child))))
    

  ;;-----------------------------------------
  ;; Jumpables (things which support jump)

  (provide <jumpable> jumpable?)

  (defclass <jumpable> (<node>))

  (defgeneric (jump (target <jumpable>)))

  ;;-----------------------------------------
  ;; Groups of Cards

  (provide $root-node)

  (define (card-or-card-group? node)
    (or (card? node) (card-group? node)))

  (defclass <card-group> (<group>)
    active?)

  (defgeneric (card-group-find-next (group <card-group>) (child <jumpable>)))
  (defgeneric (card-group-find-prev (group <card-group>) (child <jumpable>)))

  (defmethod (card-group-find-next (group <card-group>) (child <jumpable>))
    #f)

  (defmethod (card-group-find-prev (group <card-group>) (child <jumpable>))
    #f)

  (defmethod (group-add-child! (group <card-group>) (child <node>))
    (assert (card-or-card-group? child))
    (call-next-method))

  (define $root-node
    (make <card-group>
      :name |/| :parent #f :active? #t :init-thunk #f
      :children '()))
  
  ;;-----------------------------------------
  ;; Sequences of Cards (like groups, but ordered)

  (defclass <card-sequence> (<jumpable> <card-group>))

  (defmethod (jump (target <card-sequence>))
    (if (null? (group-children target))
        (error (cat "Can't jump to sequence " (node-full-name target)
                    " because it contains no cards."))
        (jump (car (group-children target)))))

  (defmethod (card-group-find-next (group <card-sequence>) (child <jumpable>))
    ;; Find the node *after* child.
    (let [[remainder (memq child (group-children group))]]
      (assert (not (null? remainder)))
      (if (null? (cdr remainder))
          #f
          (cadr remainder))))

  (defmethod (card-group-find-prev (group <card-group>) (child <jumpable>))
    ;; Find the node *before* child.
    (let search [[children (group-children group)] [candidate #f]]
      (assert (not (null? children) ))
      (if (eq? (car children) child)
          candidate
          (search (cdr children) (car children)))))

  ;;-----------------------------------------
  ;; Cards

  (provide <card> card? card-next card-prev jump-next jump-prev)

  (defclass <card>          (<jumpable> <group>))

  (defmethod (jump (target <card>))
    (jump-to-card target))

  (define (card-next)
    (card-group-find-next (node-parent (current-card)) (current-card)))

  (define (card-prev)
    (card-group-find-prev (node-parent (current-card)) (current-card)))

  (define (jump-helper find-card str)
    (let [[c (find-card)]]
      (if c
          (jump c)
          (error (cat "No card " str " " (node-full-name (current-card))
                      " in sequence.")))))
      
  (define (jump-next) (jump-helper card-next "after"))
  (define (jump-prev) (jump-helper card-prev "before"))

  ;;-----------------------------------------
  ;; Elements (currently unused)

  ;;(defclass <element>       (<node>))


  ;;=======================================================================
  ;;  Cards
  ;;=======================================================================

  (provide card-exists? current-card card-name group sequence card)

  ;; TODO - A different meaning of "previous" from the one above.  Rename.
  (define *%kernel-current-card* #f)
  (define *%kernel-previous-card* #f)
  
  ;; TODO - This glue makes <card> look like the old %kernel-card.  Remove.
  ;; TODO - We need to start creating sequences, etc., to contain cards.
  ;;(define-struct %kernel-card (name thunk) (make-inspector))
  (define (make-%kernel-card name thunk)
    (make <card> :name name :init-thunk thunk :parent $root-node
          :children '()))
  (define %kernel-card? card?)
  (define %kernel-card-name node-full-name)
  (define %kernel-card-thunk node-init-thunk)

  ;; TODO - Redundant with *node-table*.  Remove.
  (define *%kernel-card-table* (make-hash-table))
  
  (define (%kernel-register-card card)
    (let ((name (%kernel-card-name card)))
      (if (hash-table-get *%kernel-card-table* name (lambda () #f))
          (non-fatal-error (cat "Duplicate card: " name))
          (begin
            (if (have-5l-prim? 'RegisterCard)
                (call-5l-prim 'RegisterCard name))
            (hash-table-put! *%kernel-card-table* name card)))))

  (define (card-exists? card-name)
    (if (hash-table-get *%kernel-card-table* (string->symbol card-name)
                        (lambda () #f))
        #t
        #f))
  
  (define (%kernel-run-card card)
    (%kernel-clear-timeout)

    ;; Finish exiting our previous card.
    (when *%kernel-current-card*
      (call-hook-functions *exit-card-hook* *%kernel-current-card*)
      (when (have-5l-prim? 'notifyexitcard)
        (call-5l-prim 'notifyexitcard)))

    ;; Update our global variables.
    (set! *%kernel-previous-card* *%kernel-current-card*)
    (set! *%kernel-current-card* card)
    (when (have-5l-prim? 'notifyentercard)
      (call-5l-prim 'notifyentercard))

    ;; Actually run the card.
    (debug-log (cat "Begin card: <" (%kernel-card-name card) ">"))
    (call-hook-functions *enter-card-hook* *%kernel-current-card*)
    (with-errors-blocked (non-fatal-error)
      (call-5l-prim 'resetorigin)
      ((%kernel-card-thunk card))
      (call-hook-functions *card-body-finished-hook* card)
      (refresh)))

  (define (%kernel-find-card card-or-name)
    (cond
     [(%kernel-card? card-or-name)
      card-or-name]
     [(symbol? card-or-name)
      (let ((card (hash-table-get *%kernel-card-table*
                                  card-or-name
                                  (lambda () #f))))
        (or card (throw (cat "Unknown card: " card-or-name))))]
     [(string? card-or-name)
      (%kernel-find-card (string->symbol card-or-name))]
     [#t
      (throw (cat "Not a card: " card-or-name))]))

  (define (current-card)
    *%kernel-current-card*)    

  (define (card-name card-or-name)
    (cond
     [(%kernel-card? card-or-name)
      (%kernel-card-name card-or-name)]
     [(symbol? card-or-name)
      (symbol->string card-or-name)]
     [(string? card-or-name)
      card-or-name]
     [#t
      (throw (cat "Not a card: " card-or-name))]))

  (define-syntax define-node
    (syntax-rules ()
      [(define-node name [parent local-name] init-expr)
       (begin
         (define name
           (with-values [parent local-name] (analyze-node-name 'name)
             init-expr))
         (register-node name))]))

  (define-syntax group
    (syntax-rules ()
      [(group name)
       (define-node name [parent local-name]
         (make <card-group>
           :parent parent
           :name local-name
           :children '()))]))

  (define-syntax sequence
    (syntax-rules ()
      [(group name)
       (define-node name [parent local-name]
         (make <card-sequence>
           :parent parent
           :name local-name
           :children '()))]))

  (define-syntax card
    (syntax-rules ()
      [(card name body ...)
       (begin
         (define-node name [parent local-name]
           (make <card>
             :parent parent
             :name local-name
             :init-thunk (lambda () (begin/var body ...))
             :children '()))
         (%kernel-register-card name))]))


  ;;=======================================================================
  ;;  Kernel Entry Points
  ;;=======================================================================
  ;;  The '%kernel-' methods are called directly by the 5L engine.  They
  ;;  shouldn't raise errors, because they're called directly from C++ code
  ;;  that doesn't want to catch them (and will, in fact, quit the
  ;;  program).

  (define (%kernel-run)
    (with-errors-blocked (fatal-error)
      (label exit-interpreter
        (fluid-let ((*%kernel-exit-interpreter-func* exit-interpreter))
          (let ((jump-card #f))
            (let loop ()
              (label exit-to-top
                (with-errors-blocked (non-fatal-error)
                  (fluid-let ((*%kernel-exit-to-top-func* exit-to-top))
                    (idle)
                    (cond
                     [jump-card
                      (%kernel-run-card (%kernel-find-card jump-card))]
                     [#t
                      ;; Highly optimized do-nothing loop. :-)  This
                      ;; is a GC optimization designed to prevent the
                      ;; interpreter from allocating memory like a crazed
                      ;; maniac while the user's doing nothing.  If we
                      ;; removed this block, we'd have to perform a lot
                      ;; of LABEL and FLUID-LET statements, which are
                      ;; extremely expensive in quantities of 1,000.
                      (let idle-loop ()
                        (unless (eq? *%kernel-state* 'JUMPING)
                          (idle)
                          (idle-loop)))]))))
              (set! jump-card #f)
              (when (eq? *%kernel-state* 'JUMPING)
                (set! jump-card *%kernel-jump-card*))
              (%kernel-maybe-clear-state)
              (loop)))))
      (%kernel-maybe-clear-state)
      (%kernel-clear-timeout)))

  (define (%kernel-kill-interpreter)
    (%kernel-set-state 'INTERPRETER-KILLED))

  (define (%kernel-stop)
    (%kernel-set-state 'STOPPING))

  (define (%kernel-go)
    (when (%kernel-stopped?)
      (%kernel-set-state 'NORMAL)))

  (define (%kernel-stopped?)
    (or (eq? *%kernel-state* 'STOPPING)
        (eq? *%kernel-state* 'STOPPED)))
  
  (define (%kernel-pause)
    (%kernel-die-if-callback '%kernel-pause)
    (%kernel-set-state 'PAUSED))

  (define (%kernel-wake-up)
    (%kernel-die-if-callback '%kernel-wake-up)
    (when (%kernel-paused?)
      (%kernel-maybe-clear-state)))

  (define (%kernel-paused?)
    (eq? *%kernel-state* 'PAUSED))

  (define (%kernel-timeout card-name seconds)
    (%kernel-die-if-callback '%kernel-timeout)
    (%kernel-set-timeout (+ (current-milliseconds) (* seconds 1000))
                         (lambda () (jump card-name))))

  (define (%kernel-nap tenths-of-seconds)
    (%kernel-die-if-callback '%kernel-nap)
    (set! *%kernel-nap-time* (+ (current-milliseconds)
                                (* tenths-of-seconds 100)))
    (%kernel-set-state 'NAPPING))

  (define (%kernel-napping?)
    (eq? *%kernel-state* 'NAPPING))

  (define (%kernel-kill-nap)
    (%kernel-die-if-callback '%kernel-kill-nap)
    (when (%kernel-napping?)
      (%kernel-maybe-clear-state)))

  (define (%kernel-kill-current-card)
    (%kernel-set-state 'CARD-KILLED))

  (define (%kernel-jump-to-card-by-name card-name)
    (set! *%kernel-jump-card* card-name)
    (%kernel-set-state 'JUMPING))

  (define (%kernel-current-card-name)
    (if *%kernel-current-card*
        (value->string (%kernel-card-name *%kernel-current-card*))
        ""))

  (define (%kernel-previous-card-name)
    (if *%kernel-previous-card*
        (value->string (%kernel-card-name *%kernel-previous-card*))
        ""))

  (define (%kernel-valid-card? card-name)
    (card-exists? card-name))

  (define (%kernel-eval expression)
    (let [[ok? #t] [result "#<did not return from jump>"]]

      ;; Jam everything inside a callback so JUMP, etc., work
      ;; as the user expects.  Do some fancy footwork to store the
      ;; return value(s) and return them correctly.  This code is ugly
      ;; because I'm too lazy to redesign the callback architecture
      ;; to make it pretty.
      (%kernel-run-as-callback
       ;; Our callback.
       (lambda ()
         (call-with-values
          (lambda () (eval (read (open-input-string expression))))
          (lambda r (set! result (apply format-result-values r)))))

       ;; Our error handler.
       (lambda (error-msg)
         (set! ok? #f)
         (set! result error-msg)))
      
      ;; Return the result.
      (cons ok? result)))

  (define (%kernel-run-callback function args)
    (%kernel-die-if-callback '%kernel-run-callback)
    (%kernel-run-as-callback (lambda () (apply function args))
                             non-fatal-error))

  (define (%kernel-reverse! l)
    (reverse! l))

  ) ; end module
