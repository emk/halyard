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

  ;; Already provided by Swindle, I think.
  ;;(define (keyword? value)
  ;;  (and (symbol? value)
  ;;       (let [[str (symbol->string value)]]
  ;;         (and (> 0 (string-length str))
  ;;              (eq? (string-ref str 0) #\:)))))

  (define (keyword-name value)
    (assert (keyword? value))
    (let [[str (symbol->string value)]]
      (string->symbol (substring str 1 (string-length str)))))

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
  ;;  Kernel Entry Points
  ;;=======================================================================
  ;;  The '%kernel-' methods are called directly by the 5L engine.  They
  ;;  shouldn't raise errors, because they're called directly from C++ code
  ;;  that doesn't want to catch them (and will, in fact, quit the
  ;;  program).
  ;;
  ;;  The theory behind these functions is documented in detail in
  ;;  TInterpreter.h.

  (define (%kernel-run)
    ;; The workhorse function.  We get called to manage the main event
    ;; loop, and we provide support code for handling jumps, STOPPING
    ;; the interpreter, idling after the end of each card, and quiting
    ;; the interpreter.  This code needs to be understood in
    ;; conjuction with *%kernel-state*, the functions that manipulate
    ;; it, and the callback system.  Yes, it's ugly--but it lets us
    ;; get the semantics we want without writing an entire interpreter
    ;; from scratch.
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
                      (run-card (find-card jump-card))]
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
                          (if (%kernel-stopped?)
                              (blocking-idle)
                              (idle))
                          (idle-loop)))]))))
              (set! jump-card #f)
              (when (eq? *%kernel-state* 'JUMPING)
                ;; Handle a jump by setting jump-card for our next goaround.
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
      (%kernel-clear-state)))

  (define (%kernel-paused?)
    (eq? *%kernel-state* 'PAUSED))

  (define (%kernel-timeout card-name seconds)
    (%kernel-die-if-callback '%kernel-timeout)
    (%kernel-set-timeout (+ (current-milliseconds) (* seconds 1000))
                         (lambda () (jump (find-card card-name)))))

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
      (%kernel-clear-state)))

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
  ;;              told to wake back up.  This is used for modal text
  ;;              entry fields, (wait ...), and similar things.
  ;;    JUMPING:  The interpreter should execute a jump.
  ;;    NAPPING:  The interpreter should pause the current card for the
  ;;              specified number of milliseconds.
  ;;    CARD-KILLED: The interpreter should stop executing the current
  ;;               card, and return to the top-level loop.
  ;;    INTERPRETER-KILLED: The interpreter should exit.
  ;;
  ;;  Callbacks are slightly special, however--see the code for details.
  
  (provide call-at-safe-time)

  ;; The most important global state variables.
  (define *%kernel-running-callback?* #f)
  (define *%kernel-state* 'NORMAL)

  ;; Some slightly less important global state variables.  See the
  ;; functions which define and use them for details.
  (define *%kernel-jump-card* #f)
  (define *%kernel-timeout* #f)
  (define *%kernel-timeout-thunk* #f)
  (define *%kernel-nap-time* #f)

  ;; Deferred thunks are used to implement (deferred-callback () ...).
  ;; See call-at-safe-time for details.
  (define *%kernel-running-deferred-thunks?* #f)
  (define *%kernel-deferred-thunk-queue* '())
  
  ;; These are bound at the top level in %kernel-run, and get
  ;; temporarily rebound during callbacks.  We use
  ;; *%kernel-exit-interpreter-func* to help shut down the
  ;; interpreter, and *%kernel-exit-to-top-func* to help implement
  ;; anything that needs to bail out to the top-level loop (usually
  ;; after setting up some complex state).  See the functions which define
  ;; and call these functions for more detail.
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
    ;; If we have a timeout to run, and it's a safe time to run it, do so.
    (if (%kernel-stopped?)
        (%kernel-clear-timeout)
        (unless *%kernel-running-callback?*
          (when (and *%kernel-timeout*
                     (>= (current-milliseconds) *%kernel-timeout*))
            (let ((thunk *%kernel-timeout-thunk*))
              (%kernel-clear-timeout)
              (thunk))))))

  (define (%kernel-safe-to-run-deferred-thunks?)
    ;; Would now be a good time to run deferred thunks?  Wait until
    ;; nothing exciting is happening.  See call-at-safe-time.
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
    ;; If the interpreter has stopped, cancel any deferred thunks.  (See
    ;; call-at-safe-time.)  This function won't call any deferred thunks
    ;; unless it is safe to do so.
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

  (define (%kernel-clear-state)
    ;; This is the version that we want to call from most places to get the
    ;; current state set back to normal.
    (assert (not (or (eq? *%kernel-state* 'STOPPING) 
                     (eq? *%kernel-state* 'STOPPED))))
    (set! *%kernel-state* 'NORMAL)
    (set! *%kernel-jump-card* #f))

  (define (%kernel-maybe-clear-state)
    ;; This should only ever be called from the main loop, because then
    ;; stopping has finished and the interpreter is in a stopped state. 
    ;; Everyone else should only ever call %kernel-clear-state
    (case *%kernel-state*
      [[STOPPING]
       (set! *%kernel-state* 'STOPPED)]
      [[STOPPED]
       #f]
      [else
       (set! *%kernel-state* 'NORMAL)])
    (set! *%kernel-jump-card* #f))
  
  (define (%kernel-run-as-callback thunk error-handler)
    ;; This function is in charge of running callbacks from C++ into
    ;; Scheme.  These include simple callbacks and anything evaled from
    ;; C++.  When we're in a callback, we need to install special values
    ;; of *%kernel-exit-to-top-func* and *%kernel-exit-interpreter-func*,
    ;; because the values installed by %kernel-run can't be invoked without
    ;; "throwing" across C++ code, which isn't allowed (and which would
    ;; be disasterous).  Furthermore, we must trap all errors, because
    ;; our C++-based callers don't want to deal with Scheme exceptions.
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
    ;; This function is called immediately after returning from any
    ;; primitive call (including idle).  It's job is to check flags
    ;; set by the engine, handle any deferred callbacks, check
    ;; timeouts, and generally bring the Scheme world back into sync
    ;; with everybody else.
    ;; 
    ;; Since this is called after idle, it needs to be *extremely*
    ;; careful about allocating memory.  See %kernel-run for more
    ;; discussion about consing in the idle loop (hint: it's bad).
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
       (%call-5l-prim 'schemeidle #f)
       (%kernel-check-state)]       ; Tail-call self without consing.
      [[NAPPING]
       (if (< (current-milliseconds) *%kernel-nap-time*)
           (begin
             (%call-5l-prim 'schemeidle #f)
             (%kernel-check-state)) ; Tail call self without consing.
           (%kernel-clear-state))]
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
  
  (provide call-5l-prim have-5l-prim? idle blocking-idle 5l-log
           debug-log caution debug-caution non-fatal-error fatal-error 
           engine-var set-engine-var! engine-var-exists?  throw 
           exit-script jump refresh)

  ;; C++ can't handle very large or small integers.  Here are the
  ;; typical limits on any modern platform.
  (define *32-bit-signed-min* -2147483648)
  (define *32-bit-signed-max* 2147483647)
  (define *32-bit-unsigned-min* 0)
  (define *32-bit-unsigned-max* 4294967295)

  (define (call-5l-prim . args)
    ;; Our high-level wrapper for %call-5l-prim (which is defined by
    ;; the engine).  You should almost always call this instead of
    ;; %call-5l-prim, because this function calls %kernel-check-state
    ;; to figure out what happened while we were in C++.
    (let ((result (apply %call-5l-prim args)))
      (%kernel-check-state)
      result))
  
  (define (have-5l-prim? name)
    (call-5l-prim 'haveprimitive name))
  
  (define (blocking-idle)
    (%kernel-die-if-callback 'idle)
    (call-5l-prim 'schemeidle #t))

  (define (idle)
    (%kernel-die-if-callback 'idle)
    (call-5l-prim 'schemeidle #f))
  
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
    ;; Set an engine variable.  This is a pain, because we have to play
    ;; along with the engine's lame type system.
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
          (set! *%kernel-jump-card* card)
          (%kernel-set-state 'JUMPING)
          (%kernel-check-state))))

  (define (refresh &key (transition 'none) (ms 500))
    ;; Refresh the screen by blitting dirty areas of our offscreen buffer
    ;; to the display.
    (call-hook-functions *before-draw-hook*)
    (if (have-5l-prim? 'refresh)
        (call-5l-prim 'refresh transition ms)))


  ;;=======================================================================
  ;;  Object Model
  ;;=======================================================================

  ;;-----------------------------------------------------------------------
  ;;  Templates
  ;;-----------------------------------------------------------------------
  
  ;; This object is distinct from every other object, so we use it as
  ;; a unique value.
  (define $no-default (list 'no 'default))

  (defclass <template-parameter> ()
    (name      :type <symbol>)
    (type      :type <class>       :initvalue <object>)
    (label     :type <string>      :initvalue "")
    (default   :type <object>      :initvalue $no-default))

  (defclass <template> ()
    (group      :type <symbol>     :initvalue 'none)
    (extends                       :initvalue #f)
    (parameters :type <list>       :initvalue '())
    (bindings   :type <hash-table> :initvalue (make-hash-table))
    (init-thunk :type <function>   :initvalue (lambda () #f)))

  (defmethod (initialize (template <template>) initargs)
    (call-next-method)
    (assert (or (not (template-extends template))
                (eq? (template-group template)
                     (template-group (template-extends template))))))

  (define (bindings->hash-table bindings)
    ;; Turns a keyword argument list into a hash table.
    (let [[result (make-hash-table)]]
      (let recursive [[b bindings]]
        (cond
         [(null? b)
          #f]
         [(null? (cdr b))
          (error "Bindings list contains an odd number of values:" bindings)]
         [(not (keyword? (car b)))
          (error "Expected keyword in bindings list, found:" (car b) bindings)]
         [else
          (hash-table-put! result (keyword-name (car b)) (cadr b))
          (recursive (cddr b))]))
      result))
  
  (define-syntax define-template
    (syntax-rules (:extends)
      [(define-template name group (:extends extended bindings ...)
         (parameters ...) body ...)
       (define name (make <template>
                      :group      group
                      :extends    extended
                      :bindings   (bindings->hash-table (list bindings ...))
                      :parameters (expand-parameters parameters ...)
                      :init-thunk (lambda () (begin/var body ...))))]
      [(define-template name group (bindings ...) rest ...)
       (define-template name group (:extends #f bindings ...) rest ...)]))

  (define-syntax define-template-definer
    (syntax-rules ()
      [(define-template-definer definer-name group)
       (define-syntax definer-name
         (syntax-rules ()
           [(definer-name name . rest)
            (define-template name 'group . rest)]))]))

  ;;-----------------------------------------------------------------------
  ;;  Nodes
  ;;-----------------------------------------------------------------------
  ;;  A program is (among other things) a tree of nodes.  The root node
  ;;  contains <card>s and <card-groups>.  <card>s contain <element>s.

  (provide <node> node? node-name node-full-name node-parent find-node @)

  (defclass <node> (<template>)
    name
    parent
    (handlers :type <hash-table> :initvalue (make-hash-table))
    )

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

  (defgeneric (register-node (node <node>)))

  (defmethod (register-node (node <node>))
    (let [[name (node-full-name node)]]
      (when (hash-table-get *node-table* name (lambda () #f))
        (error (cat "Duplicate copies of node " node)))
      (hash-table-put! *node-table* name node)))

  (define (find-node name)
    (hash-table-get *node-table* name (lambda () #f)))

  (define (find-node-relative base name)
    ;; Treat 'name' as a relative path.  If 'name' can be found relative
    ;; to 'base', return it.  If not, try the parent of base if it
    ;; exists.  If all fails, return #f.
    (if (eq? base $root-node)
        (find-node name)
        (let* [[base-name (node-full-name base)]
               [candidate (string->symbol (cat base-name "/" name))]
               [found (find-node candidate)]]
          (or found (find-node-relative (node-parent base) name)))))

  (define (@-helper name)
    (if (current-card)
        (find-node-relative (node-parent (current-card)) name)
        (error (cat "Can't write (@ " name ") outside of a card"))))    

  (define-syntax @
    ;; Syntactic sugar for find-node-relative.
    (syntax-rules ()
      [(@ name)
       (@-helper 'name)]))

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

  (define-syntax define-node
    (syntax-rules (:extends)
      [(define-node name group node-class (:extends extended bindings ...)
         body ...)
       (begin
         (define name
           (with-values [parent local-name] (analyze-node-name 'name)
             (make node-class
               :group      'group
               :extends    extended
               :bindings   (bindings->hash-table (list bindings ...))
               :init-thunk (lambda () (begin/var body ...))
               :parent     parent
               :name       local-name)))
         (register-node name))]
      [(define-node name group node-class (bindings ...) body ...)
       (define-node name group node-class (:extends #f bindings ...) body ...)]
      [(define-node name group node-class)
       (define-node name group node-class (:extends #f))]))

  (define-syntax define-node-definer
    (syntax-rules ()
      [(define-node-definer definer-name group node-class)
       (define-syntax definer-name
         (syntax-rules ()
           [(definer-name name . rest)
            (define-node name group node-class . rest)]))]))

  ;;-----------------------------------------------------------------------
  ;;  Groups
  ;;-----------------------------------------------------------------------
  ;;  A group contains other nodes.

  (provide <group> group? group-children)

  (defclass <group> (<node>)
    (children :type <list> :initvalue '()))

  (defgeneric (group-add-child! (group <group>) (child <node>)))

  (defmethod (group-add-child! (group <group>) (child <node>))
    (set! (group-children group)
          (append (group-children group) (list child))))
    
  ;;-----------------------------------------------------------------------
  ;;  Jumpable
  ;;-----------------------------------------------------------------------
  ;;  The abstract superclass of all nodes which support 'jump'.  In
  ;;  general, these are either cards or <card-sequence>s.

  (provide <jumpable> jumpable?)

  (defclass <jumpable> (<node>))

  (defgeneric (jump (target <jumpable>)))

  ;;-----------------------------------------------------------------------
  ;;  Groups of Cards
  ;;-----------------------------------------------------------------------
  ;;  By default, groups of cards are not assumed to be in any particular
  ;;  linear order, at least for purposes of program navigation.

  (provide $root-node define-group-template group)

  (define (card-or-card-group? node)
    (or (card? node) (card-group? node)))

  (defclass <card-group> (<group>)
    (active? :type <boolean> :initvalue #f))

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
      :name |/| :parent #f :active? #t))

  (define-template-definer define-group-template <card-group>)
  (define-node-definer group <card-group> <card-group>)

  ;;-----------------------------------------------------------------------
  ;;  Sequences of Cards
  ;;-----------------------------------------------------------------------
  ;;  Like groups, but ordered.

  (provide sequence)

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
          (card-group-find-next (node-parent group) group)
          (cadr remainder))))

  (defmethod (card-group-find-prev (group <card-sequence>) (child <jumpable>))
    ;; Find the node *before* child.
    (let search [[children (group-children group)]
                 [candidate-func 
                  (lambda ()
                    (card-group-find-prev (node-parent group) group))]]
      (assert (not (null? children)))
      (if (eq? (car children) child)
          (candidate-func)
          (search (cdr children) (lambda () (car children))))))

  (define-node-definer sequence <card-group> <card-sequence>)

  ;;-----------------------------------------------------------------------
  ;;  Cards
  ;;-----------------------------------------------------------------------
  ;;  More functions are defined in the next section below.

  (provide <card> card? card-next card-prev jump-next jump-prev
           define-card-template card)

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

  (defmethod (register-node (c <card>))
    (call-next-method)
    (%kernel-register-card c))

  (define-template-definer define-card-template <card>)
  (define-node-definer card <card> <card>)

  ;;-----------------------------------------------------------------------
  ;; Elements
  ;;-----------------------------------------------------------------------

  (provide <element> element? define-element-template element create)

  (defclass <element>       (<node>)
    (temporary? :type <boolean> :initvalue #f))

  (define-template-definer define-element-template <element>)
  (define-node-definer element <element> <element>)
  
  (define (create template &key (name (gensym)) &rest-keys bindings)
    ;; Temporarily create an <element> node belonging to the current card,
    ;; using 'template' as our template.  This node will be deleted when we
    ;; leave the card.
    ;;
    ;; Someday I expect this to be handled by a transactional rollback of the
    ;; C++ document model.  But this will simulate things well enough for
    ;; now, I think.
    (assert (current-card))
    (assert (eq? (template-group template) '<element>))
    (let [[e (make <element>
               :group      'element
               :extends    template
               :bindings   (bindings->hash-table bindings)
               :parent     (current-card)
               :name       name
               :temporary? #t)]]
      ((template-init-thunk e))
      e))

  ;;=======================================================================
  ;;  Changing Cards
  ;;=======================================================================

  ;; We call these function whenever we enter or exit a node.  They never
  ;; recurse up or down the node hierarchy; that work is done by other
  ;; functions below.
  (defgeneric (exit-node (node <node>)))
  (defgeneric (enter-node (node <node>)))

  (defmethod (exit-node (node <node>))
    ;; Clear our handler list.
    (set! (node-handlers node) (make-hash-table)))

  (defmethod (enter-node (node <node>))
    ;; TODO - Make sure all our template parameters are bound.
    ;; Initialize our templates one at a time.
    (let recurse [[template node]]
      (when template
        (recurse (template-extends template))
        ((template-init-thunk template)))))

  (defmethod (exit-node (group <card-group>))
    (set! (card-group-active? group) #f)
    (call-next-method))

  (defmethod (enter-node (group <card-group>))
    (call-next-method)
    (set! (card-group-active? group) #t))

  (defmethod (exit-node (card <card>))
    ;; We have some extra hooks and primitives to call here.
    (call-hook-functions *exit-card-hook* card)
    (when (have-5l-prim? 'notifyexitcard)
      (call-5l-prim 'notifyexitcard))
    (call-next-method))

  (defmethod (enter-node (card <card>))
    ;; We have some extra hooks and primitives to call here.
    (when (have-5l-prim? 'notifyentercard)
      (call-5l-prim 'notifyentercard))
    (call-hook-functions *enter-card-hook* card)
    (call-next-method)
    (call-hook-functions *card-body-finished-hook* card))

  (define (find-active-parent new-card)
    ;; Walk back up the node hierarchy from new-card until we find the
    ;; nearest active parent.  The root node is always active, so this
    ;; recursive call terminates.
    (let recurse [[group (node-parent new-card)]]
      (if (card-group-active? group)
          group
          (recurse (node-parent group)))))

  (define (exit-card-group-recursively group stop-before)
    ;; Recursively exit nested card groups starting with 'group', but
    ;; ending before 'stop-before'.
    (let recurse [[group group]]
      (unless (eq? group stop-before)
        (enter-node group)
        (recurse (node-parent group)))))

  (define (enter-card-group-recursively group)
    ;; Recursively enter nested card groups until we have a chain of
    ;; active groups from the root to 'group'.
    (let recurse  [[group group]]
      (unless (card-group-active? group)
        (recurse (node-parent group))
        (exit-node group))))

  (define (exit-card old-card new-card)
    ;; Exit all our child elements.
    (let loop [[children (group-children old-card)]]
      (unless (null? children)
        (exit-node (car children))
        (loop (cdr children))))
    ;; TODO - Delete temporary elements.
    ;; Exit old-card.
    (exit-node old-card)
    ;; Exit as many enclosing card groups as necessary.
    (exit-card-group-recursively (node-parent old-card)
                                 (find-active-parent new-card)))

  (define (enter-card new-card)
    ;; Clear our handler list.  We also do this in exit-node; this
    ;; invocation is basically defensive, in case something went
    ;; wrong during the node exiting process.
    (set! (node-handlers node) (make-hash-table))
    ;; Enter as many enclosing card groups as necessary.
    (enter-card-group-recursively (node-parent new-card))
    ;; Enter all our child elements.  Notice we do this first, so
    ;; all the elements are available by the time we run the card body.
    ;; Unfortunately, this means that "new element" events generated
    ;; during element initialization can't be caught by the card.
    ;; Weird, but this is what the users voted for.
    (let loop [[children (group-children new-card)]]
      (unless (null? children)
        (enter-node (car children))
        (loop (cdr children))))
    ;; Enter new-card.
    (enter-node new-card))

  (define (run-card card)
    (%kernel-clear-timeout)

    ;; Finish exiting our previous card.
    (when *%kernel-current-card*
      (exit-card *%kernel-current-card* card))

    ;; Reset the origin to 0,0.
    (call-5l-prim 'resetorigin)

    ;; Update our global variables.
    (set! *%kernel-previous-card* *%kernel-current-card*)
    (set! *%kernel-current-card* card)

    ;; Actually run the card.
    (debug-log (cat "Begin card: <" (%kernel-card-name card) ">"))
    (with-errors-blocked (non-fatal-error)
      (enter-card card)
      (refresh)))


  ;;=======================================================================
  ;;  Cards
  ;;=======================================================================
  ;;  Older support code for cards.  Some of this should probably be
  ;;  refactored to an appropriate place above.

  (provide card-exists? find-card current-card card-name)

  ;; TODO - A different meaning of "previous" from the one above.  Rename.
  (define *%kernel-current-card* #f)
  (define *%kernel-previous-card* #f)
  
  ;; TODO - This glue makes <card> look like the old %kernel-card.  Remove.
  ;; TODO - We need to start creating sequences, etc., to contain cards.
  ;;(define-struct %kernel-card (name thunk) (make-inspector))
  (define %kernel-card? card?)
  (define %kernel-card-name node-full-name)
  (define %kernel-card-thunk template-init-thunk)

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

  (define (find-card card-or-name)
    (cond
     [(%kernel-card? card-or-name)
      card-or-name]
     [(symbol? card-or-name)
      (let ((card (hash-table-get *%kernel-card-table*
                                  card-or-name
                                  (lambda () #f))))
        (or card (throw (cat "Unknown card: " card-or-name))))]
     [(string? card-or-name)
      (find-card (string->symbol card-or-name))]
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

  ) ; end module
