(module nodes (lib "language.ss" "5L")

  ;; Various support code and declarations refactored out of the kernel.
  (require (lib "types.ss" "5L"))
  (provide (all-from (lib "types.ss" "5L")))
  (require (lib "util.ss" "5L"))
  (provide (all-from (lib "util.ss" "5L")))
  (require-for-syntax (lib "util.ss" "5L"))
  (require (lib "hook.ss" "5L"))

  ;; Get begin/var.
  (require (lib "begin-var.ss" "5L"))

  ;; Require our macro-related helpers.
  (require-for-syntax (lib "capture.ss" "5L"))
  (require (lib "indent.ss" "5L"))

  ;; Our shiny new node path system.
  (require (lib "paths.ss" "5L"))
  (provide %node-path% node-path?)
  
  ;;=======================================================================
  ;;  Stuff Callable by the Kernel
  ;;=======================================================================
  ;;  This should not be exported any further than the kernel, unless
  ;;  otherwise specified.

  (provide run-card jump
           delete-element-internal
           dispatch-event-to-current-group-member
           current-group-member
           current-card
           *running-on-exit-handler-for-node*)

  ;; XXX - This, along with the other provides here, needs to be
  ;; refactored into the right place...
  (provide last-card-visited)
  
  ;;=======================================================================
  ;;  Engine Interface
  ;;=======================================================================

  (provide *engine* set-engine! %engine% static-root-node running-root-node)

  (define-class %engine% ()
    (attr static-root-node %root-node%)
    (attr running-root-node (%root-node% .new))
    (attr static-node-table (make-hash-table))
    (attr running-node-table (make-hash-table))
    (attr default-element-parent #f :writable? #t)
    (attr current-group-member (.running-root-node) :writable? #t)
    (attr last-card-visited #f :writable? #t)

    (def (current-card)
      (let [[result (.current-group-member)]]
        (if (card? result)
            result
            #f)))

    ;; Private helper funciton.
    (define (must-override)
      (error "Must override %engine% method"))

    (def (set-event-handled?! handled?) (must-override))
    (def (set-event-vetoed?! vetoed?) (must-override))
    (def (jump-to-card target) (must-override))
    (def (register-card card) (must-override))
    (def (enable-expensive-events enable?) (must-override))
    (def (notify-exit-card card) (must-override))
    (def (notify-enter-card card) (must-override))
    (def (notify-card-body-finished card) (must-override))
    (def (delete-element elem) (must-override))
    (def (exit-node node) (must-override))
    )

  (define *engine* #f)

  (define (set-engine! engine)
    (set! *engine* engine))

  (define (static-root-node)
    (*engine* .static-root-node))

  (define (running-root-node)
    (*engine* .running-root-node))

  (define (root-node? node)
    (eq? (node .parent) #f))

  (define (current-group-member)
    (let [[result (*engine* .current-group-member)]]
      (unless result
        (error "Can't get current group member during script startup"))
      result))

  (define (current-static-group-member)
    ((current-group-member) .static-node))

  (define (current-card)
    (let [[result (*engine* .current-card)]]
      (unless result
        (error "Can't get current card when no card is active"))
      result))

  (define (current-static-card)
    ((current-card) .static-node))
  
  (define (last-card-visited)
    (*engine* .last-card-visited))
  
  
  ;;=======================================================================
  ;;  Object Model
  ;;=======================================================================

  ;;-----------------------------------------------------------------------
  ;;  Events
  ;;-----------------------------------------------------------------------

  (provide on send send*
           <event> event? event-stale?
           <vetoable-event> veto-event! event-vetoed?
           <idle-event> idle-event?
           <update-ui-event> update-ui-event? event-command
           <char-event> char-event? event-character event-modifiers
           <mouse-event> mouse-event? event-position event-double-click?
           <url-event> url-event? event-url
           <text-event> text-event? event-text
           <browser-navigate-event> browser-navigate-event?
           <progress-changed-event> event-progress-done? event-progress-value
           make-node-event-dispatcher ; semi-private
           <media-event> <media-finished-event>
           <media-caption-event> event-caption
           )

  (define-syntax (on stx)
    (syntax-case stx ()
      [(_ name args . body)
       ;; Create a capture variable NEXT-HANDLER which will be visible in
       ;; BODY.  It's exceptionally evil to capture using BODY's context
       ;; instead of EXPAND-ON's context, but that's what we want.
       (with-syntax [[self (make-self #'body)]
                     [call-next-handler
                      (make-capture-var/ellipsis #'body 'call-next-handler)]]
         (quasisyntax/loc
          stx
          (register-event-handler self 'name
                                  (lambda (call-next-handler . args)
                                    (begin/var . body)))))]))

  (define (register-event-handler node name handler)
    (%assert (node .instance-of? %node%))

    ;; Keep track of whether we're handling expensive events.  We call
    ;; ENABLE-EXPENSIVE-EVENTS here, which is sufficient for %card% and
    ;; %element% nodes.  But since %card-group%s and %card-sequence%s stay
    ;; alive longer than a single card, we need to set
    ;; NODE-HAS-EXPENSIVE-HANDLERS?, which is used by
    ;; MAYBE-ENABLE-EXPENSIVE-EVENTS-FOR-CARD (on behalf of RUN-CARD) to do
    ;; the rest of our bookkeeping.
    (when (expensive-event? name)
      (set! (node-has-expensive-handlers? node) #t)
      (*engine* .enable-expensive-events #t))

    ;; If we're registering a handler for a mouse event, and the node
    ;; appears to care, let it know.
    ;;
    ;; TODO - This is a bit more informal than our EXPENSIVE-EVENT?
    ;; handling; perhaps we should unify the two?
    (when (and (mouse-event-name? name)
               (eq? (prop node wants-cursor?) 'auto))
      (set! (prop node wants-cursor?) #t))

    ;; Update our handler table.
    (let* [[table (node-handlers node)]
           [old-handler (hash-table-get table name (lambda () #f))]]
      (if old-handler
          (hash-table-put! table name
                           ;; This is tricky--we need to replace the old
                           ;; handler with our new one.  To do this, we
                           ;; create a glue function to Do The Right Thing
                           ;; with the NEXT-HANDLER argument.
                           ;;
                           ;; TODO - We don't catch duplicate handlers
                           ;; within a single node or template (or at the
                           ;; top level).  This would be a Good Thing<tm>
                           ;; to do correctly.
                           (lambda (call-next-handler . args)
                             (apply handler
                                    (lambda ()
                                      (apply old-handler
                                             call-next-handler args))
                                    args)))
          (hash-table-put! table name handler))))

  (define (send/nonrecursive* call-next-handler node name . args)
    (let [[handler (hash-table-get (node-handlers node) name (lambda () #f))]]
      (if handler
          (apply handler call-next-handler args)
          (call-next-handler))))

  (define (send/recursive* call-next-handler node name . args)
    (let recurse [[node node]]
      (if (not node)
          (call-next-handler)
          (let [[new-call-next-handler
                 (lambda () (recurse (node-parent node)))]]
            (apply send/nonrecursive* new-call-next-handler node name args)))))

  (define (send* node name
                 &key (arguments '()) (recursive? #t) (ignorable? #f))
    (define (error-handler)
      (error (cat "No handler for " name " on " (node-full-name node))))
    (define (ignore-handler)
      #f)
    (apply (if recursive? send/recursive* send/nonrecursive*)
           (if ignorable? ignore-handler error-handler)
           node name arguments))

  (define-syntax send
    (syntax-rules ()
      [(send node name . args)
       (send* node 'name :arguments (list . args))]))
  (define-syntax-indent send 2)

  (defclass <event> ()
    (stale? :accessor event-stale? :initvalue #f))

  (defclass <vetoable-event> (<event>)
    (vetoed? :accessor event-vetoed? :initvalue #f))

  (defclass <idle-event> (<event>))

  (defclass <update-ui-event> (<event>)
    (command :accessor event-command))

  (defclass <char-event> (<event>)
    (character :accessor event-character)
    (modifiers :accessor event-modifiers))

  (defclass <mouse-event> (<event>)
    (position :accessor event-position)
    (double-click? :accessor event-double-click? :initvalue #f))

  (defclass <edit-box-event> (<event>))

  (defclass <url-event> (<event>)
    (url :accessor event-url))

  (defclass <text-event> (<event>)
    (text :accessor event-text))

  (defclass <browser-navigate-event> (<url-event> <vetoable-event>))

  (defclass <progress-changed-event> (<event>)
    (done? :accessor event-progress-done?)
    ;; value is 0.0 to 1.0, inclusive.
    (value :accessor event-progress-value))

  (defclass <media-event> (<event>))
  (defclass <media-finished-event> (<media-event>))
  (defclass <media-caption-event> (<media-event>)
    (caption :accessor event-caption))

  (defclass <cursor-event> (<event>))

  (define (veto-event! event)
    (set! (event-vetoed? event) #t))

  ;; A local helper.
  (defgeneric (was-vetoed? (event <event>)))
  (defmethod (was-vetoed? (event <event>))
    #f)
  (defmethod (was-vetoed? (event <vetoable-event>))
    (event-vetoed? event))

  (define (dispatch-event-to-node node name args)
    (debug-log (cat (node-full-name node) ": " name " event: " args))
    (let [[unhandled? #f]
          [event (case name
                   [[update-ui]
                    (make <update-ui-event> :command (car args))]
                   [[char]
                    (make <char-event>
                      :character (string-ref (car args) 0)
                      :modifiers (cadr args)
                      :stale? (caddr args))]
                   [[mouse-down]
                    (make <mouse-event>
                      :position (point (car args) (cadr args))
                      :double-click? (caddr args)
                      :stale? (cadddr args))]
                   [[mouse-up mouse-enter mouse-leave mouse-moved]
                    (make <mouse-event>
                      :position (point (car args) (cadr args))
                      :stale? (cadr args))]
                   [[text-changed text-enter]
                    (make <edit-box-event>)]
                   [[browser-navigate]
                    (make <browser-navigate-event> :url (car args))]
                   [[browser-page-changed]
                    (make <url-event> :url (car args))]
                   [[browser-title-changed]
                    (make <text-event> :text (car args))]
                   [[status-text-changed]
                    (make <text-event> :text (car args))]
                   [[progress-changed]
                    (make <progress-changed-event>
                      :done? (car args)
                      :value (cadr args))]
                   [[media-finished]
                    (make <media-finished-event>)]
                   [[media-local-error media-network-error
                     media-network-timeout playback-timer]
                    (make <media-event>)]
                   [[media-caption]
                    (make <media-caption-event> :caption (car args))]
                   [[cursor-moved]
                    (make <mouse-event>
                      :position (point (car args) (cadr args))
                      :stale? (cadr args))]
                   [[cursor-shown cursor-hidden]
                    (make <cursor-event>)]
                   [else
                    (non-fatal-error (cat "Unsupported event type: " name))])]]
      (define (no-handler)
        (set! unhandled? #t))
      (send/recursive* no-handler node name event)
      (set! (*engine* .event-vetoed?) (was-vetoed? event))
      (set! (*engine* .event-handled?) (not unhandled?))))

  (define (dispatch-idle-event-to-active-nodes)
    (define event (make <idle-event>))
    (define (no-handler)
      (void))
    (define (send-idle node)
      (send/nonrecursive* no-handler node 'idle event))
    (let loop [[node (current-group-member)]]
      (when node
        (send-idle node)
        (foreach [elem (node-elements node)]
          (send-idle elem))
        (loop (node-parent node)))))

  (define (dispatch-event-to-current-group-member name . args)
    (when (*engine* .current-group-member)
      (if (eq? name 'idle)
          (dispatch-idle-event-to-active-nodes)
          (dispatch-event-to-node (current-group-member) name args))))

  (define (make-node-event-dispatcher node)
    (lambda (name . args)
      (dispatch-event-to-node node name args)))

  (define (expensive-event? name)
    ;; Some events are sent almost constantly, and cause us to allocate
    ;; memory too quickly.  This causes a performance loss.  To avoid
    ;; this performance loss, we only enable the sending of these events
    ;; if we believe there is a handler to receive them.
    (case name
      [[idle mouse-moved] #t]
      [else #f]))

  (define (mouse-event-name? name)
    (case name
      [[mouse-moved mouse-down mouse-up mouse-enter mouse-leave] #t]
      [else #f]))


  ;;-----------------------------------------------------------------------
  ;;  Templates
  ;;-----------------------------------------------------------------------
  
  (provide prop* set-prop*! prop *node-defined-hook*)

  ;; These objects are distinct from every other object, so we use them as
  ;; unique values.
  (define $no-default (list 'no 'default))
  (define $no-such-key (list 'no 'such 'key))

  (define-class %node% ()
    (with-instance (.class)
      ;; Override ATTR and add backwards-compatibility wrapper for old
      ;; ON PROP-CHANGE protocol.
      (def (attr name &key (label #f) (type #f) (writable? #f) &rest keys)
        (super)
        (unless writable?
          (.define-method (symcat "set-" name "!")
            (method (value)
              ;; TODO - Can we refactor out code shared with ruby-objects.ss?
              (when (and type (not (instance-of? value type)))
                (error (cat "Attr " name " has type " type
                            ", tried to assign " value)))
              (if (not (.initialized?))
                (set! (slot name) value)
                (.send '%maybe-set-property! (list name value)))))))

      ;; Backwards-compatibility glue for old :DEFAULT semantics.
      (def (attr/glue name &key (default $no-default) &rest-keys keys)
        (if (eq? default $no-default)
          (.send 'attr (list* name keys))
          (.send 'attr (list* name :default (method () default) keys))))
      )

    ;;; Run the "body" function of this node, where elements are created,
    ;;; media is played, etc.
    (def (run-body)
      (void))

    ;;; Backwards-compatibility glue for the old ON PROP-CHANGE protocol.
    (def (%maybe-set-property! name value)
      ;; We allow the scriptor to set properties on a node.  However, we
      ;; must notify the node of the change.  The node may choose to veto
      ;; the change--in which case, we undo the change.
      (define (veto &opt reason)
        (define msg (cat "Cannot set property '" name "' on node '"
                         (node-full-name self) "' to " value))
        (if reason
            (error (cat msg ": " reason))
            (error msg)))
      (define (no-handler)
        (veto "Property is read-only."))
      (let [[accepted? #f]
            [previous #f]]
        (dynamic-wind
            (lambda ()
              ;; Store our previous property value, and set the
              ;; new one.
              (set! previous (slot name))
              (set! (slot name) value))
            (lambda ()
              ;; Notify the node that the property has changed.
              ;; If the node vetos the change, send/nonrecursive*
              ;; will raise an error, and we'll never set
              ;; accepted? to #t.
              (send/nonrecursive* no-handler self 'prop-change name
                                  value previous veto)
              (set! accepted? #t))
            (lambda ()
              ;; If we didn't accept the change, revert it.
              (unless accepted?
                (set! (slot name) previous))))))
      )

  (define (prop* node name)
    (node .send name '()))

  (define (set-prop*! node name value)
    ;; TODO - Performance?
    (node .send
          (string->symbol (string-append "set-" (symbol->string name) "!"))
          (list value)))

  (define-syntax prop
    (syntax-rules ()
      [(prop node name)
       (prop* node 'name)]))
  (define-syntax-indent prop function)

  (define-syntax expand-prop-decls
    (syntax-rules (:new-default)
      [(expand-prop-decls self)
       (void)]
      [(expand-prop-decls self (name :new-default default) rest ...)
       (begin (self .attr-initializer 'name (method () default) #t)
              (expand-prop-decls self rest ...))]
      [(expand-prop-decls self (name keywords ...) rest ...)
       (begin (self .attr/glue 'name keywords ...)
              (expand-prop-decls self rest ...))]
      [(expand-prop-decls self name rest ...)
       (begin (self .attr 'name)
              (expand-prop-decls self rest ...))]))

  (define-syntax (expand-method-with-prop-names stx)
    (syntax-case stx ()
      ;; CTX is the syntax context in which to create SELF.
      [(_ ctx prop-decls . body)
       (with-syntax [[self (make-self #'ctx)]
                     [super (make-capture-var/ellipsis #'ctx 'super)]]

         ;; Bind each template property NAME to a call to (prop self
         ;; 'name), so it's convenient to access.  We don't need to use
         ;; capture variables for this, because the programmer supplied the
         ;; names--which means they're already in the right context.
         (define (make-prop-bindings prop-decls-stx)
           (datum->syntax-object
            stx
            (map (lambda (prop-stx)
                   (syntax-case prop-stx ()
                     [(name . rest)
                      (syntax/loc prop-stx [name (prop self name)])]
                     [name
                      (syntax/loc prop-stx [name (prop self name)])]))
                 (syntax->list prop-decls-stx))))

         ;; We introduce a number of "capture" variables in BODY.  These
         ;; will be bound automagically within BODY without further
         ;; declaration.  See the PLT203 mzscheme manual for details.
         (quasisyntax/loc
          stx
          (lambda (self super)
            (letsubst #,(make-prop-bindings #'prop-decls)
              (begin/var . body)))))]))

  (define-syntax expand-run-body
    (syntax-rules ()
      [(_ prop-decls . body)
       (method ()
         (super)
         (instance-exec self
          (expand-method-with-prop-names body prop-decls . body)))]))
  
  (define-syntax expand-bindings
    (syntax-rules ()
      [(_ self prop-decls)
       (void)]
      [(_ self prop-decls keyword value . rest)
       (begin (self .attr-initializer (keyword-name keyword)
                    (expand-method-with-prop-names keyword prop-decls value)
                    #f)
              (expand-bindings self prop-decls . rest))]))

  (define-syntax define-template
    (syntax-rules ()
      [(_ name prop-decls (extended . bindings) . body)
       (define-class name (extended)
         (expand-prop-decls name . prop-decls)
         (expand-bindings name prop-decls . bindings)
         (.define-method 'run-body (expand-run-body prop-decls . body)))]))

  ;; TODO - Backwards compatibility glue that should go away.
  (define-syntax define-template-definer
    (syntax-rules ()
      [(_ definer-name default-superclass)
       (begin
         (define-syntax definer-name
           (syntax-rules ()
             [(definer-name name prop-decls () . body)
              (define-template name prop-decls (default-superclass) . body)]
             [(definer-name name . rest)
              (define-template name . rest)]))
         (define-syntax-indent definer-name 3))]))

  ;; Called when a node is defined.
  (define *node-defined-hook* (make-hook 'node-defined-hook))

  (define-syntax define-node
    (syntax-rules ()
      [(define-node name (extended . bindings) . body)
       (begin
         (define-class name (extended)
           ;; HACK - Hygiene problems with SELF, so we use the explicit name
           ;; everywhere.
           (with-values [[parent local-name] (analyze-node-name 'name)]
             (check-node-name local-name)
             (set! (name .name) local-name)
             (set! (name .parent) parent)
             (expand-bindings name [] . bindings)
             (.define-method 'run-body (expand-run-body [] . body))
             ))
         (name .register)
         (call-hook-functions *node-defined-hook* name))]))

  (define-syntax define-node-definer
    (syntax-rules ()
      ;; TODO - Should we check that any supplied superclass is actually
      ;; a subclass of DEFAULT-SUPERCLASS?
      [(define-node-definer definer-name default-superclass)
       (begin
         (define-syntax definer-name
           (syntax-rules ()
             [(definer-name name)
              (define-node name (default-superclass))]
             [(definer-name name () . body)
              (define-node name (default-superclass) . body)]
             [(definer-name name . rest)
              (define-node name . rest)]))
         (define-syntax-indent definer-name 2))]))

  (define (create klass
                  &key (name (gensym)) (parent (default-element-parent))
                  &rest-keys bindings)
    ;; Create an instance of KLASS attached to PARENT. This element will be
    ;; deleted when we leave the card.
    (unless (klass .subclass-of? %element%)
      (error (cat "Can only CREATE subclasses of %ELEMENT%, not " klass)))
    (unless parent
      (error "Can't create element without a parent node"))
    (unless (symbol? name)
      (error (cat "create " klass ": name must be a <symbol>; given " name)))
    (check-node-name name)
    (let [[inst (klass .send 'new
                  (list* :parent     parent
                         :name       name
                         bindings))]]
      (inst .enter-node)
      inst))

  ;;-----------------------------------------------------------------------
  ;;  Nodes
  ;;-----------------------------------------------------------------------
  ;;  A program is (among other things) a tree of nodes.  The root node
  ;;  contains %card%s and %card-group%s.  %card-group%s contain %card%s
  ;;  and other %card-group%s.  Any node may contain %element%s.

  (provide %node% node? node-name node-full-name extends-template? node-parent
           node-elements find-node find-running-node find-static-node
           resolve @* @)

  (define (add-shared-node-members! inst)
    (with-instance inst
      (def (register-in table)
        (let [[name (node-full-name self)]]
          (when (hash-table-has-key? table name)
            (error (cat "Duplicate copies of node " name)))
          (hash-table-put! table name self)))

      (def (unregister-from table)
        (let [[name (node-full-name self)]]
          (%assert (hash-table-has-key? table name))
          (hash-table-remove! table name)))
      ))
      

  (with-instance %node%
    ;; We replicate several members at both the class and the instance
    ;; level, becase they are meaningful for both running and non-running
    ;; nodes.
    (add-shared-node-members! self)
    (add-shared-node-members! (.class))

    (attr name :type <symbol>)
    (attr parent)

    ;; May be ENTERING, ACTIVE, EXITING, EXITED.
    (attr node-state 'ENTERING :type <symbol> :writable? #t)

    (attr elements '() :type <list> :writable? #t)
    (attr has-expensive-handlers?  #f :type <boolean> :writable? #t)
    (attr handlers (make-hash-table) :type <hash-table> :writable? #t) 

    ;; Attributes shared by all nodes.
    (attr wants-cursor? #f :label "Wants cursor?")

    (def (active?)
      (eq? (.node-state) 'ACTIVE))

    (def (to-string)
      (check-for-initialization self 'to-string)
      (cat "#<inst " (node-full-name self) ">"))

    (def (register)
      (.register-in (*engine* .running-node-table)))

    (def (unregister)
      (.unregister-from (*engine* .running-node-table)))

    (with-instance (.class)
      (attr name #f :writable? #t)   ; May be #f if class not in hierarchy.
      (attr parent #f :writable? #t)

      (def (to-string)
        (if (.name)
          (cat "#<class " (node-full-name self) ">")
          (super)))

      ;;; Returns #t if this node is part of the hierarchy of named nodes,
      ;;; and can be run directly.
      (def (can-be-run?)
        (and (.name) #t))

      (def (register)
        (.register-in (*engine* .static-node-table)))
      
      ;; Corresponds to the old %jumpable% class.
      ;; TODO - Decide if we want a general .implements-interface? method.
      (def (jumpable?)
        #f)
      
      ;; Must be overriden by all jumpable nodes.
      (def (find-first-card)
        #f)
      
      ;; Must be overriden by all jumpable nodes.
      (def (find-last-card)
        #f)
      )
    )

  ;; HACK - Nasty backwards compatibility hack from before we distintiguished
  ;; between running and non-running nodes.  This should go away.
  (define (make-node-type-predicate klass)
    (lambda (obj)
      (and (ruby-object? obj)
           (or (obj .instance-of? klass)
               (and (obj .instance-of? %class%)
                    (obj .subclass-of? klass)
                    (obj .name))))))

  ;; TODO - Get rid of these wrapper functions.
  ;; TODO - Some setters will be needed.
  (define node? (make-node-type-predicate %node%))
  (define (node-name node) (node .name))
  (define (node-parent node) (node .parent))
  (define (node-elements node) (node .elements))
  (define (set-node-elements! node val) (set! (node .elements) val))
  (define (node-has-expensive-handlers? node) (node .has-expensive-handlers?))
  (define (set-node-has-expensive-handlers?! node val)
    (set! (node .has-expensive-handlers?) val))
  (define (node-handlers node) (node .handlers))
  (define (set-node-handlers! node val) (set! (node .handlers) val))
  (define (find-first-card node) (node .find-first-card))
  (define (find-last-card node) (node .find-last-card))

  (define (node-full-name node-or-path)
    ;; XXX - Resolve either a running or a static node here, depending
    ;; on what we find.  This is an odd special case that really only
    ;; affects NODE-FULL-NAME, because it's one of the few non-trivial
    ;; functions which can be called on either static or running nodes,
    ;; and possibly the only one that really cares about the difference.
    (define (not-found-fn)
      (error (cat "Cannot find " node-or-path "; "
                  "If referring to a static node, please resolve it first.")))
    (define node 
      (cond
       [(node-path? node-or-path)
        (node-or-path .resolve-path :running? #t :if-not-found not-found-fn)]
       [(node? node-or-path)
        node-or-path]
       [else
        (error (cat "node-full-name: expecting node or node-path, given " 
                    node-or-path "."))]))
    ;; Join together local names with "/" characters.
    (let [[parent (node-parent node)]]
      (if (and parent (not (root-node? parent)))
        (string->symbol (cat (node-full-name (node-parent node))
                             "/" (node-name node)))
        (node-name node))))

  (define (extends-template? node template)
    (if (ruby-class? node)
        (node .subclass-of? template)
        (node .instance-of? template)))
  
  (define (check-for-duplicate-nodes node-list node)
    (let recurse [[node-list node-list]]
      (cond
       [(null? node-list)
        #f]
       [(eq? (node-name node) (node-name (car node-list)))
        (error (cat "Duplicate node: " (node-full-name node)))]
       [#t
        (recurse (cdr node-list))])))

  (define (node-add-element! node elem)
    ;; We need to check for duplicates before adding or we violate
    ;; some pretty obvious invariants.
    (check-for-duplicate-nodes (node-elements node) elem)
    (set! (node-elements node)
          (append (node-elements node) (list elem))))

  ;; This is an internal API that should probably not be used by script code.
  (define (find-node name running?)
    (define table (if running? 
                    (*engine* .running-node-table) 
                    (*engine* .static-node-table)))
    (hash-table-get table name (lambda () #f)))

  ;;; Look up a running node by name.
  (define (find-running-node name-as-string)
    (find-node (string->symbol name-as-string) #t))
  
  ;;; Look up a static node by name.
  (define (find-static-node name-as-string)
    (find-node (string->symbol name-as-string) #f))
  
  (define (find-node-relative base name running?)
    ;; Treat 'name' as a relative path.  If 'name' can be found relative
    ;; to 'base', return it.  If not, try the parent of base if it
    ;; exists.  If all fails, return #f.
    (unless base
      (error (cat "Can't find relative path '@" name "' outside of a card")))
    (if (root-node? base)
        (find-node name running?)
        (let* [[base-name (node-full-name base)]
               [candidate (string->symbol (cat base-name "/" name))]
               [found (find-node candidate running?)]]
          (or found (find-node-relative (node-parent base) name running?)))))

  ;;; Given either a %node-path% or a node, return a node.
  (define (resolve path-or-node &key (running? #t))
    (if (node-path? path-or-node)
        (path-or-node .resolve-path :running? running?)
        path-or-node))

  ;; We need to add some methods to %node-path%.
  (with-instance %node-path%
    ;;; Redirect any method calls we don't understand to our associated node.
    (def (method-missing name . args)
      ((.resolve-path) .send name args))

    ;; XXX - Nasty hack since we haven't decided what to do about interfaces
    ;; yet.  This will need more thought.
    ;; TODO - Decide if we want a general .implements-interface? method.
    (def (instance-of? klass)
      (or (super)
          ((.resolve-path) .instance-of? klass)))

    ;;; Resolve a path.
    (def (resolve-path
          &key (running? #t)
               (if-not-found
                (lambda ()
                  (error (cat "Can't find relative path: " self)))))
      (or (find-node-relative (if running?
                                  (current-group-member)
                                  (current-static-group-member))
                              (.to-symbol) running?)
          (if-not-found)))
    )

  (define-syntax @
    ;; Syntactic sugar for creating a path.
    (syntax-rules ()
      [(@ name)
       ;; TODO - Make our caller pass us a string instead.
       (@* (symbol->string 'name))]))
  (define-syntax-indent @ function)

  (define (check-node-name name)
    (unless (regexp-match "^[a-z][-_a-z0-9]*$" (symbol->string name))
      (error (cat "Bad node name: " name ". Must begin with a letter, and "
                  "contain only letters, numbers, hyphens and underscores."))))

  (define (analyze-node-name name)
    ;; Given a name of the form '/', 'foo' or 'bar/baz', return the
    ;; node's parent and the "local" portion of the name (excluding the
    ;; parent).  A "/" character separates different levels of nesting.
    (if (eq? name '|/|)
        (values #f '|/|) ; Handle the root node.
        (let* [[str (symbol->string name)]
               [matches (regexp-match "^(([^/].*)/)?([^/]+)$" str)]]
          (cond
           [(not matches)
            (error (cat "Illegal node name: " name))]
           [(not (cadr matches))
            (values (static-root-node) name)]
           [else
            (let [[parent (find-node (string->symbol (caddr matches)) #f)]]
              (unless parent
                (error (cat "Parent of " name " does not exist.")))
              (values parent
                      (string->symbol (cadddr matches))))]))))


  ;;-----------------------------------------------------------------------
  ;;  Group Member
  ;;-----------------------------------------------------------------------
  ;;  A %group-member% may be stored in a %card-group%.

  (define-class %group-member% (%node%)
    (with-instance (.class)
      (def (register)
        (super)
        (when (node-parent self)
          (group-add-member! (node-parent self) self))))

    ;;; Return the static version of this node.
    (def (static-node)
      (define result (.class))
      (%assert (result .can-be-run?))
      result)
    )

  ;;-----------------------------------------------------------------------
  ;;  Jumpable
  ;;-----------------------------------------------------------------------
  ;;  This used to be an abstract superclass for jumpable objects  In
  ;;  general, these are either cards or %card-sequence%s.

  ;; TODO - Get rid of wrapper.
  (define (jumpable? node)
    (and (ruby-object? node) ((resolve node :running? #f) .jumpable?)))
  (define (jump node)
    ((resolve node :running? #f) .jump))

  ;;-----------------------------------------------------------------------
  ;;  Groups of Cards
  ;;-----------------------------------------------------------------------
  ;;  By default, groups of cards are not assumed to be in any particular
  ;;  linear order, at least for purposes of program navigation.

  (provide define-group-template group %card-group% group-members card-group?)

  (define (card-or-card-group? node)
    (or (card? node) (card-group? node)))

  (define-class %card-group% (%group-member%)
    (with-instance (.class)
      (attr members '() :type <list> :writable? #t)
      
      (def (find-next member)
        (assert (member .jumpable?))
        #f)
      
      (def (find-prev member)
        (assert (member .jumpable?))
        #f)
      )
    )

  ;; TODO - Eliminate these wrappers.
  (define card-group? (make-node-type-predicate %card-group%))
  (define (card-group-members grp) (grp .members))
  (define (set-card-group-members! grp val) (set! (grp .members) val))
  (define (card-group-active? grp) (grp .active?))
  (define (card-group-find-next grp member) (grp .find-next member))
  (define (card-group-find-prev grp member) (grp .find-prev member))

  (define group-members card-group-members)
  (define set-group-members! set-card-group-members!)

  (define (group-add-member! group member)
    ;; We need to check for duplicates before adding or we violate
    ;; some pretty obvious invariants.
    (check-for-duplicate-nodes (group-members group) member)
    (set! (group-members group)
          (append (group-members group) (list member))))

  (define-template-definer define-group-template %card-group%)
  (define-node-definer group %card-group%)

  ;;-----------------------------------------------------------------------
  ;;  The root node
  ;;-----------------------------------------------------------------------

  (define-class %root-node% (%card-group%)
    (set! (.name) '|/|)
    (attr-value name (%root-node% .name)) ; Copy class name attr to instance.
    (attr-value parent #f))

  ;;-----------------------------------------------------------------------
  ;;  Sequences of Cards
  ;;-----------------------------------------------------------------------
  ;;  Like groups, but ordered.

  (provide sequence %card-sequence% card-sequence? define-sequence-template)

  (define-class %card-sequence% (%card-group%)
    (with-instance (.class)
      (def (jumpable?) #t)
      
      (def (jump)
        (if (null? (group-members self))
            (error (cat "Can't jump to sequence " (node-full-name self)
                        " because it contains no cards."))
            (jump (car (group-members self)))))
      
      (def (find-next member)
        (assert (member .jumpable?))
        ;; Find the node *after* member.
        (let [[remainder (memq member (group-members self))]]
          (%assert (not (null? remainder)))
          (if (null? (cdr remainder))
              (card-group-find-next (node-parent self) self)
              ;; Walk recursively through any sequences to find the first card.
              (find-first-card (cadr remainder)))))
      
      (def (find-prev member)
        (assert (member .jumpable?))
        ;; Find the node *before* member.  Notice the two (2!) lambdas in
        ;; this function, which are used to implement a form of lazy
        ;; evaluation: They keep track of how to find the node we want,
        ;; assuming the current current node is MEMBER (which is one past the
        ;; node we want).
        (let search [[members (group-members self)]
                     [candidate-func 
                      (lambda ()
                        (card-group-find-prev (node-parent self) self))]]
          (%assert (not (null? members)))
          (if (eq? (car members) member)
              (candidate-func)
              (search (cdr members)
                      (lambda ()
                        ;; This is our actual base case: It's called when
                        ;; we've located MEMBER, and it recursively looks
                        ;; for the last card in the node *before* MEMBER.
                        ;; Got it?
                        (find-last-card (car members)))))))
      
      (def (find-first-card)
      (find-first-card (first (group-members self))))
      
      (def (find-last-card)
        (find-last-card (last (group-members self))))
      )
    )

  ;; TODO - Get rid of wrapper functions.
  (define card-sequence? (make-node-type-predicate %card-sequence%))

  (define-template-definer define-sequence-template %card-sequence%)
  (define-node-definer sequence %card-sequence%)

  ;;-----------------------------------------------------------------------
  ;;  Cards
  ;;-----------------------------------------------------------------------
  ;;  More functions are defined in the next section below.

  (provide %card% card? card-next card-prev jump-next jump-prev jump-current
           define-card-template card)

  (define-class %card% (%group-member%)
    (with-instance (.class)
      (def (register)
        (super)
        (*engine* .register-card self))
      
      (def (jumpable?) #t)
      
      (def (jump)
        (*engine* .jump-to-card self))
      
      (def (find-first-card) self)
      (def (find-last-card) self)
      )

    ;; This is a fairly easy mistake to make.
    (def (jump)
      (error (cat "Cannot jump to running card " self ", jump to static "
                  "counterpart " (self .class) " instead.")))
    )

  ;; TODO - Get rid of wrapper functions.  
  (define card? (make-node-type-predicate %card%))

  (define (card-next)
    (define static (current-static-card))
    (card-group-find-next (node-parent static) static))

  (define (card-prev)
    (define static (current-static-card))
    (card-group-find-prev (node-parent static) static))

  (define (jump-helper find-card str)
    (let [[c (find-card)]]
      (if c
          (jump c)
          (error (cat "No card " str " " (node-full-name (current-card))
                      " in sequence.")))))
      
  (define (jump-next) (jump-helper card-next "after"))
  (define (jump-prev) (jump-helper card-prev "before"))
  (define (jump-current)
    (jump (current-static-card)))
  
  (define-template-definer define-card-template %card%)
  (define-node-definer card %card%)

  ;;-----------------------------------------------------------------------
  ;; Elements
  ;;-----------------------------------------------------------------------

  (provide element? define-element-template ; %element%
           default-element-parent call-with-default-element-parent
           with-default-element-parent create)

  ;; TODO - Merge this element class with one in tamale.ss.  Should we
  ;; rename this one until we do?
  (define-class %element% (%node%)
    (def (initialize &rest keys)
      (super)
      (node-add-element! (node-parent self) self)))

  ;; TODO - Get rid of wrapper functions.  
  (define element? (make-node-type-predicate %element%))

  (define-template-definer define-element-template %element%)
  ;;(define-node-definer element %element% %element%)
  
  (define (default-element-parent)
    (or (*engine* .default-element-parent)
        (current-card)))

  (define (call-with-default-element-parent node thunk)
    (let [[old (*engine* .default-element-parent)]]
      (dynamic-wind
          (lambda () (set! (*engine* .default-element-parent) node))
          thunk
          (lambda () (set! (*engine* .default-element-parent) old)))))

  (define-syntax with-default-element-parent
    (syntax-rules ()
      [(with-default-element-parent node . body)
       (call-with-default-element-parent node (lambda () . body))]))
  (define-syntax-indent with-default-element-parent 1)

  (define (eq-with-gensym-hack? sym1 sym2)
    ;; STUPID BUG - We name anonymous elements with gensyms, for
    ;; which (eq? sym (string->symbol (symbol->string sym))) is never
    ;; true.  This is dumb, and is evidence of a fundamental misdesign
    ;; in element management responsibilities.  I need to fix this ASAP.
    ;; But for now, this will allow the engine to limp along.
    (equal? (symbol->string sym1) (symbol->string sym2)))

  (define (delete-element-internal elem)
    (if (node-path? elem)
        ;; If we've got a node path, resolve it first so we can use EQ? to
        ;; compare it against existing nodes.
        (delete-element-internal (elem .resolve-path))
        ;; We're the master node deletion routine--C++ is no longer in charge.
        ;; TODO - We're called repeatedly as nodes get deleted, resulting
        ;; in an O(n^2) time to delete n nodes.  Not good, but we can live with
        ;; it for the moment.
        (let [[parent (elem .parent)]]
          ;; Remove the node from its parent's NODE-ELEMENTS first, to
          ;; reduce the odds that a script can find the node mid-deletion.
          (set! (parent .elements)
                (filter (lambda (e) (not (eq? elem e)))
                        (parent .elements)))
          ;; Now it's safe to delete the node.
          (elem .exit-node)
          (*engine* .delete-element elem))))
  

  ;;=======================================================================
  ;;  Changing Cards
  ;;=======================================================================
  ;;  We call these function whenever we enter or exit a node.  They never
  ;;  recurse up or down the node hierarchy; that work is done by other
  ;;  functions below.

  (with-instance %node%
    (def (enter-node)
      ;; TODO - Make sure all our template properties are bound.
      ;; Mark this node as running so we can add event handlers and CREATE
      ;; elements.
      (%assert (eq? (.node-state) 'ENTERING))
      ;; Register this node in the table of running nodes.
      (.register)
      ;; Because we haven't been running, we shouldn't have any child
      ;; elements yet.
      (%assert (null? (node-elements self)))
      ;; Let the world know that we're starting to run this node.
      (.notify-enter)
      ;; Run our body function.  This may take quite a while, play media,
      ;; and so on--this is where "card" bodies actually get run.
      (.run-body)
      ;; Let the node know all initialization functions have been run.
      ;; (This allows "two-phase" construction, where templates can effectively
      ;; send messages to subtemplates.)
      (send/nonrecursive* (lambda () #f) self 'setup-finished)
      (.notify-body-finished)
      (set! (.node-state) 'ACTIVE))

    (def (notify-enter))
    (def (notify-body-finished))
    (def (notify-exit))

    (def (exit-node)
      (%assert (memq (.node-state) '(ENTERING ACTIVE)))
      (set! (.node-state) 'EXITING)
      (.notify-exit)
      ;; Exit all our child elements.
      ;; TRICKY - We call into the engine to do element deletion safely.
      ;; We work with a copy of (NODE-ELEMENTS SELF) the original
      ;; will be modified as we run.
      (foreach [elem (node-elements self)]
        (delete-element-internal elem))
      ;; Unregister our state-db listeners, if we have any.
      (*engine* .exit-node self)
      ;; Run any exit handler.
      (run-on-exit-handler self)
      ;; Unregister from the running node table.
      (.unregister)
      ;; Mark this node as no longer active, so nobody tries to call ON
      ;; or CREATE on it.
      (set! (.node-state) 'EXITED))
    )

  (with-instance %card%
    (def (notify-enter)
      (*engine* .notify-enter-card self))

    (def (notify-body-finished)
      (*engine* .notify-card-body-finished self))

    (def (notify-exit)
      (*engine* .notify-exit-card self)
      (set! (*engine* .last-card-visited) (self .static-node)))
    )

  ;; * Things we know
  ;;
  ;; .CURRENT-GROUP-MEMBER is the most-nested group member that we've
  ;; tried to enter.
  ;;
  ;; .ACTIVE? is true for any %CARD-GROUP% that has been fully set up.  A
  ;; node may be the .CURRENT-GROUP-MEMBER but not .ACTIVE?, either if it
  ;; is still being set up or if setup previously failed.
  ;;
  ;; * Notes on RUN-CARD
  ;;
  ;; If entering a node (group?) fails, we may still call ON EXIT on it.
  ;; This is not really a good idea.
  ;;
  ;; Jumping to the current card should exit and enter it, but leave its
  ;; parent alone.

  ;;; Set our current group member.
  (define (set-current-group-member! group-member)
    (set! (*engine* .current-group-member) group-member))

  ;; Recursively enter nested nodes starting with a child of 'trunk-node',
  ;; and ending with 'node'.
  (define (enter-node-recursively node-class trunk-node-inst)
    (unless (eq? node-class (trunk-node-inst .class))
      ;; We should never need to enter the root node.
      (%assert (not (root-node? node-class)))
      (enter-node-recursively (node-parent node-class) trunk-node-inst)
      (let [[node-inst (node-class .new
                         :parent (current-group-member)
                         :name (node-class .name))]]
        (set-current-group-member! node-inst)
        (node-inst .enter-node))))
  
  ;; Recursively exit nested nodes starting with 'node', but ending before
  ;; 'trunk-node'.
  (define (exit-node-recursively node-inst trunk-node-class)
    (unless (eq? (node-inst .class) trunk-node-class)
      ;; We should never exit the root node.
      (%assert (not (root-node? node-inst)))
      (node-inst .exit-node)
      (set-current-group-member! (node-parent node-inst))
      (exit-node-recursively (node-parent node-inst) trunk-node-class)))

  (define (node-or-elements-have-expensive-handlers? node)
    ;; See if NODE or any of its elements have expensive handlers.
    (or (node-has-expensive-handlers? node)
        (let recurse [[elements (node-elements node)]]
          (if (null? elements)
              #f
              (or (node-or-elements-have-expensive-handlers? (car elements))
                  (recurse (cdr elements)))))))
  
  (define (maybe-enable-expensive-events-for-node node)
    ;; REGISTER-EVENT-HANDLER attempts to turn on expensive events whenever
    ;; a matching handler is installed.  But we need to reset the
    ;; expensive event state when changing cards.  This means we need
    ;; to pay close attention to any nodes which live longer than a card.
    ;;
    ;; TODO - We could think of much better ways of handling this, I think.
    (let [[enable? #f]]
      (let recurse [[node node]]
        (when node
          (if (node-or-elements-have-expensive-handlers? node)
              (set! enable? #t)
              (recurse (node-parent node)))))
      (*engine* .enable-expensive-events enable?)))

  ;; Find the common "trunk" shared by the nodes OLD and NEW.  If OLD is
  ;; the root node, this returns the root node.  Otherwise, this will never
  ;; return OLD (the closest it will come is (NODE-PARENT OLD)).
  (define (find-trunk-node old new)
    (cond
     [(eq? old (static-root-node))
      (static-root-node)]
     [else
      (let [[old-nodes (make-hash-table)]]
        ;; Make a table of our old node's ancestors.  We really don't want
        ;; the old node itself in this table, because even if we're going
        ;; right back to it, we want to leave first.
        (let recurse [[node (node-parent old)]]
          (when node
            (hash-table-put! old-nodes node #t)
            (recurse (node-parent node))))
        
        ;; Walk up from new node until we hit something in our table.  We
        ;; know that OLD-NODES only contains groups, and that NEW is a card,
        ;; so we can optimize this code by starting with (NODE-PARENT NEW).
        ;; This recursion will always end, because OLD-NODES will contain
        ;; the root node.
        (%assert (card? new))
        (let recurse [[node (node-parent new)]]
          (if (hash-table-get old-nodes node (lambda () #f))
              node
              (recurse (node-parent node)))))]))
  
  (define (run-card new-card-class)
    ;; The node instance we're leaving.
    (define old-node-inst (*engine* .current-group-member))
    ;; The node we're going to leave running.
    (define trunk-node-class
      (find-trunk-node (old-node-inst .class) new-card-class))

    ;; Finish exiting whatever we're in.
    (exit-node-recursively old-node-inst trunk-node-class)
    (let [[trunk-node-inst (*engine* .current-group-member)]]

      ;; Update our expensive event state.
      (maybe-enable-expensive-events-for-node trunk-node-inst)

      ;; Actually run the card.
      (debug-log (cat "Begin card: <" (node-full-name new-card-class) ">"))
      (with-errors-blocked (non-fatal-error)
        (enter-node-recursively new-card-class trunk-node-inst))))

  (define *running-on-exit-handler-for-node* #f)

  (define (run-on-exit-handler node) ; FIXME - heh.
    ;; This is pretty simple--just send an EXIT message.  But we need to
    ;; trap any JUMP calls and quit immediately, because actually allowing
    ;; the jump will hopeless corrupt the data structures in this file.
    ;; Other errors we can simply trap and display.
    (define exited-safely? #f)
    (dynamic-wind
        (lambda () #f)
        (lambda ()
          (fluid-let [[*running-on-exit-handler-for-node* node]]
            (%assert *running-on-exit-handler-for-node*)
            (with-errors-blocked (non-fatal-error)
              (send/nonrecursive* (lambda () #f) node 'exit))
            (set! exited-safely? #t)))
        (lambda ()
          (unless exited-safely?
            (fatal-error (cat "Cannot JUMP in (on exit () ...) handler for "
                              (node-full-name node)))))))

  )
