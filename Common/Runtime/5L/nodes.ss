(module nodes (lib "lispish.ss" "5L")

  ;; Various support code and declarations refactored out of the kernel.
  (require (lib "types.ss" "5L"))
  (provide (all-from (lib "types.ss" "5L")))
  (require (lib "util.ss" "5L"))
  (provide (all-from (lib "util.ss" "5L")))

  ;; Get begin/var.
  (require (lib "begin-var.ss" "5L"))

  ;; Require our macro-related helpers.
  (require-for-syntax (lib "capture.ss" "5L"))


  ;;=======================================================================
  ;;  Stuff Callable by the Kernel
  ;;=======================================================================
  ;;  This should not be exported any further than the kernel, unless
  ;;  otherwise specified.

  (provide run-card jump
           delete-element-internal
           dispatch-event-to-current-group-member
           current-group-member
           current-card)


  ;;=======================================================================
  ;;  Engine Interface
  ;;=======================================================================

  (provide *engine* set-engine! <engine> root-node
           engine-current-card engine-last-card
           set-engine-event-handled?!
           set-engine-event-vetoed?!
           engine-jump-to-card
           engine-register-card
           engine-enable-expensive-events
           engine-notify-enter-card engine-notify-exit-card
           engine-notify-card-body-finished
           engine-delete-element
           engine-state-db-unregister-listeners)

  (defclass <engine> ()
    (root-node
     :initializer (lambda () (make <card-group>
                               :name '|/| :parent #f :active? #t
                               :running? #t)))
    (node-table :initializer make-hash-table)
    (default-element-parent :initvalue #f)
    (current-group-member :initvalue #f)
    (last-card :initvalue #f))

  (define *engine* #f)

  (define (set-engine! engine)
    (set! *engine* engine))

  (define (root-node)
    (engine-root-node *engine*))

  (define (node-table)
    (engine-node-table *engine*))

  (define (current-group-member)
    (let [[result (engine-current-group-member *engine*)]]
      (unless result
        (error "Can't get current group member during script startup"))
      result))

  (define (engine-current-card engine)
    (let [[result (engine-current-group-member *engine*)]]
      (if (instance-of? result <card>)
          result
          #f)))
  
  (define (current-card)
    (let [[result (engine-current-card *engine*)]]
      (unless result
        (error "Can't get current card when no card is active"))
      result))
  
  (defgeneric (set-engine-event-handled?! (eng <engine>) (handled? <boolean>)))
  (defgeneric (set-engine-event-vetoed?! (eng <engine>) (vetoed? <boolean>)))
  (defgeneric (engine-jump-to-card (engine <engine>) (target <card>)))
  (defgeneric (engine-register-card (engine <engine>) (card <card>)))
  (defgeneric (engine-enable-expensive-events (engine <engine>)
                                              (enable? <boolean>)))
  (defgeneric (engine-notify-exit-card (engine <engine>) (card <card>)))
  (defgeneric (engine-notify-enter-card (engine <engine>) (card <card>)))
  (defgeneric (engine-notify-card-body-finished (engine <engine>)
                                                (card <card>)))
  (defgeneric (engine-delete-element (engine <engine>) (elem <element>)))
  (defgeneric (engine-state-db-unregister-listeners (engine <engine>)
                                                    (node <node>)))


  ;;=======================================================================
  ;;  Object Model
  ;;=======================================================================

  ;;-----------------------------------------------------------------------
  ;;  Events
  ;;-----------------------------------------------------------------------

  (provide on send send*
           <event> event?
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
           <media-finished-event>
           )

  (define-syntax (expand-on stx)
    (syntax-case stx ()
      [(expand-on node name args . body)
       ;; Create a capture variable NEXT-HANDLER which will be visible in
       ;; BODY.  It's exceptionally evil to capture using BODY's context
       ;; instead of EXPAND-ON's context, but that's what we want.
       (with-syntax [[call-next-handler
                      (make-capture-var/ellipsis #'body 'call-next-handler)]]
         (quasisyntax/loc
          stx
          (register-event-handler node 'name
                                  (lambda (call-next-handler . args)
                                    . body))))]))

  (define-syntax on
    ;; This gets lexically overridden by expand-init-fn to refer
    ;; to nodes other than (root-node).
    (syntax-rules ()
      [(on . rest)
       (expand-on (root-node) . rest)]))

  (define (register-event-handler node name handler)
    (debug-log (cat "Registering handler: " name " in " (node-full-name node)))

    ;; Complain if this node is theoretically inactive.
    (unless (node-running? node)
      (error (cat "Cannot add handler '" name "' to inactive node "
                  (node-full-name node))))

    ;; Keep track of whether we're handling expensive events.  We call
    ;; ENABLE-EXPENSIVE-EVENTS here, which is sufficient for <card> and
    ;; <element> nodes.  But since <card-group>s and <card-sequence>s stay
    ;; alive longer than a single card, we need to set
    ;; NODE-HAS-EXPENSIVE-HANDLERS?, which is used by
    ;; MAYBE-ENABLE-EXPENSIVE-EVENTS-FOR-CARD (on behalf of RUN-CARD) to do
    ;; the rest of our bookkeeping.
    (when (expensive-event? name)
      (set! (node-has-expensive-handlers? node) #t)
      (engine-enable-expensive-events *engine* #t))

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

  (defclass <event> ())

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

  (defclass <url-event> (<event>)
    (url :accessor event-url))

  (defclass <text-event> (<event>)
    (text :accessor event-text))

  (defclass <browser-navigate-event> (<url-event> <vetoable-event>))

  (defclass <progress-changed-event> (<event>)
    (done? :accessor event-progress-done?)
    ;; value is 0.0 to 1.0, inclusive.
    (value :accessor event-progress-value))

  (defclass <media-finished-event> (<event>))

  (define (veto-event! event)
    (set! (event-vetoed? event) #t))

  ;; A local helper.
  (defgeneric (was-vetoed? (event <event>)))
  (defmethod (was-vetoed? (event <event>))
    #f)
  (defmethod (was-vetoed? (event <vetoable-event>))
    (event-vetoed? event))

  (define (dispatch-event-to-node node name args)
    (let [[unhandled? #f]
          [event (case name
                   [[update-ui]
                    (make <update-ui-event> :command (car args))]
                   [[char]
                    (make <char-event>
                      :character (string-ref (car args) 0)
                      :modifiers (cadr args))]
                   [[mouse-down]
                    (make <mouse-event>
                      :position (point (car args) (cadr args))
                      :double-click? (caddr args))]
                   [[mouse-up mouse-enter mouse-leave mouse-moved]
                    (make <mouse-event>
                      :position (point (car args) (cadr args)))]
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
                   [else
                    (non-fatal-error (cat "Unsupported event type: " name))])]]
      (define (no-handler)
        (set! unhandled? #t))
      (send/recursive* no-handler node name event)
      (set! (engine-event-vetoed? *engine*) (was-vetoed? event))
      (set! (engine-event-handled? *engine*) (not unhandled?))))

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
    (when (engine-current-group-member *engine*)
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


  ;;-----------------------------------------------------------------------
  ;;  Templates
  ;;-----------------------------------------------------------------------
  
  (provide prop* set-prop*! prop)

  ;; These objects are distinct from every other object, so we use them as
  ;; unique values.
  (define $no-default (list 'no 'default))
  (define $no-such-key (list 'no 'such 'key))

  (defclass <template-prop-decl> ()
    (name      :type <symbol>)
    (type      :type <class>       :initvalue <object>)
    (label     :type <string>      :initvalue "")
    (default                       :initvalue $no-default))

  (defclass <template> ()
    (group      :type <symbol>     :initvalue 'none)
    (extends                       :initvalue #f)
    (prop-decls :type <list>       :initvalue '())
    (bindings-eval-fn :type <function>)
    (init-fn :type <function>   :initvalue (lambda (node) #f)))

  (defmethod (initialize (template <template>) initargs)
    (call-next-method)
    ;; Make sure templates only extend other templates in their own group.
    (assert (or (not (template-extends template))
                (eq? (template-group template)
                     (template-group (template-extends template))))))

  (define (node-bind-value! node name value)
    (if (node-has-value? node name)
        (error (cat "Duplicate property " name " on node "
                    (node-full-name node)))
        (hash-table-put! (node-values node) name value)))

  (define (node-has-value? node name)
    (not (eq? (hash-table-get (node-values node) name
                              (lambda () $no-such-key))
              $no-such-key)))

  (define (node-maybe-default-property! node prop-decl)
    (unless (node-has-value? node (template-prop-decl-name prop-decl))
      (node-bind-value! node
                        (template-prop-decl-name prop-decl)
                        (template-prop-decl-default prop-decl))))

  (define (node-bind-property-values! node template)
    (foreach [decl (template-prop-decls template)]
      (hash-table-put! (node-allowed-values node)
                       (template-prop-decl-name decl)
                       #t)
      (node-maybe-default-property! node decl))
    (let [[bindings ((template-bindings-eval-fn template) node)]]
      (foreach [[k v] bindings]
        (node-bind-value! node k v))))

  (define (node-check-for-unexpected-properties node)
    (define allowed (node-allowed-values node))
    (foreach [[name value] (node-values node)]
      (unless (hash-table-get allowed name (lambda () #f))
        (error (cat "Unexpected property '" name "' on: "
                    (node-full-name node))))))

  (define (prop* node name)
    ;; This function controls how we search for property bindings.  If
    ;; you want to change search behavior, here's the place to do it.
    (let [[value (hash-table-get (node-values node)
                                 name (lambda () $no-such-key))]]
      (cond
       [(eq? value $no-such-key)
        (error (cat "No property named '" name "' on: "
                    (node-full-name node)))]
       [(eq? value $no-default)
        (error (cat "No value for property '" name "' on: "
                    (node-full-name node)))]
       [else
        value])))

  (define (set-prop*! node name value)
    ;; We allow the scriptor to set properties on a node.  However, we
    ;; must notify the node of the change.  The node may choose to veto
    ;; the change--in which case, we undo the change.
    (define (veto &opt reason)
      (define msg (cat "Cannot set property '" name "' on node '"
                       (node-full-name node) "'"))
      (if reason
          (error (cat msg ": " reason))
          (error msg)))
    (define (no-handler)
      (veto "Property is read-only."))
    (unless (node-has-value? node name)
      (veto "Property does not exist."))
    (let [[accepted? #f]
          [table (node-values node)]
          [previous #f]]
      (dynamic-wind
          (lambda ()
            ;; Store our previous property value, and set the new one.
            (set! previous (hash-table-get table name (lambda () #f)))
            (hash-table-put! table name value))
          (lambda ()
            ;; Notify the node that the property has changed.  If the node
            ;; vetos the change, send/nonrecursive* will raise an error, and
            ;; we'll never set accepted? to #t.
            (send/nonrecursive* no-handler node 'prop-change name value
                                previous veto)
            (set! accepted? #t))
          (lambda ()
            ;; If we didn't accept the change, revert it.
            (unless accepted?
              (hash-table-put! table name previous))))))

  (define-syntax prop
    (syntax-rules ()
      [(prop node name)
       (prop* node 'name)]))

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

  (define-syntax expand-prop-decls
    (syntax-rules (:new-default)
      [(expand-prop-decls)
       '()]
      [(expand-prop-decls (name :new-default default) rest ...)
       (cons (make <template-prop-decl> :name 'name :default default)
             (expand-prop-decls rest ...))]
      [(expand-prop-decls (name keywords ...) rest ...)
       (cons (make <template-prop-decl> :name 'name keywords ...)
             (expand-prop-decls rest ...))]
      [(expand-prop-decls name rest ...)
       (cons (make <template-prop-decl> :name 'name)
             (expand-prop-decls rest ...))]))

  (define-syntax (expand-fn-with-self-and-prop-names stx)
    (syntax-case stx ()
      [(expand-fn-with-self-and-prop-names self prop-decls . body)
       (begin

         ;; Bind each template property NAME to a call to (prop self
         ;; 'name), so it's convenient to access from with the init-fn.
         ;; We don't need to use capture variables for this, because
         ;; the programmer supplied the names--which means they're already
         ;; in the right context.
         (define (make-prop-bindings self-stx prop-decls-stx)
           (with-syntax [[self self-stx]]
             (datum->syntax-object
              stx
              (map (lambda (prop-stx)
                     (syntax-case prop-stx ()
                       [(name . rest)
                        (syntax/loc prop-stx [name (prop self name)])]
                       [name
                        (syntax/loc prop-stx [name (prop self name)])]))
                   (syntax->list prop-decls-stx)))))

         ;; We introduce a number of "capture" variables in BODY.  These
         ;; will be bound automagically within BODY without further
         ;; declaration.  See the PLT203 mzscheme manual for details.
         (quasisyntax/loc
          stx
          (lambda (self)
            (letsubst #,(make-prop-bindings #'self #'prop-decls)
              (begin/var . body)))))]))

  (define-syntax (expand-init-fn stx)
    (syntax-case stx ()
      [(expand-init-fn prop-decls . body)
         
       ;; We introduce a number of "capture" variables in BODY.  These
       ;; will be bound automagically within BODY without further
       ;; declaration.  See the PLT203 mzscheme manual for details.
       (with-syntax [[self (make-capture-var/ellipsis #'body 'self)]
                     [on   (make-capture-var/ellipsis #'body 'on)]]
         (quasisyntax/loc
          stx
          (expand-fn-with-self-and-prop-names self prop-decls
            (let-syntax [[on (syntax-rules ()
                               [(_ . rest)
                                (expand-on self . rest)])]]
              (begin/var . body)))))]))
  
  (define-syntax (expand-bindings-eval-fn stx)
    (syntax-case stx ()
      [(expand-bindings-eval-fn prop-decls . bindings)
       (with-syntax [[self (make-capture-var/ellipsis #'body 'self)]]
         (quasisyntax/loc
          stx
          (expand-fn-with-self-and-prop-names self prop-decls
            (bindings->hash-table (list . bindings)))))]))

  (define-syntax define-template
    (syntax-rules ()
      [(define-template group name prop-decls
                              (extended . bindings)
         . body)
       (define name (make <template>
                      :group      group
                      :extends    extended
                      :bindings-eval-fn (expand-bindings-eval-fn prop-decls 
                                                                 . bindings)
                      :prop-decls (expand-prop-decls . prop-decls)
                      :init-fn    (expand-init-fn prop-decls . body)))]
      [(define-template group name prop-decls bindings . body)
       (define-template group name prop-decls (#f . bindings)
         . body)]))

  (define-syntax define-template-definer
    (syntax-rules ()
      [(define-template-definer definer-name group)
       (define-syntax definer-name
         (syntax-rules ()
           [(definer-name . rest)
            (define-template 'group . rest)]))]))

  ;;-----------------------------------------------------------------------
  ;;  Nodes
  ;;-----------------------------------------------------------------------
  ;;  A program is (among other things) a tree of nodes.  The root node
  ;;  contains <card>s and <card-groups>.  <card-group>s contain <card>s
  ;; and other <card-group>s.  Any node may contain <element>s.

  (provide <node> node? node-name node-full-name node-parent
           node-elements find-node @* @ elem-or-name-hack)

  (defclass <node> (<template>)
    name
    parent
    (running? :type <boolean> :initvalue #f)
    (elements :type <list> :initvalue '())
    (has-expensive-handlers? :type <boolean> :initvalue #f)
    (handlers :type <hash-table> :initializer (lambda () (make-hash-table)))
    (allowed-values :type <hash-table>
                    :initializer (lambda () (make-hash-table)))
    (values   :type <hash-table> :initializer (lambda () (make-hash-table)))
    )

  (defmethod (initialize (node <node>) initargs)
    (call-next-method)
    (when (node-parent node)
      (if (element? node)
          (node-add-element! (node-parent node) node)
          (group-add-member! (node-parent node) node))))

  (define (node-full-name node)
    ;; Join together local names with "/" characters.
    (let [[parent (node-parent node)]]
      (if (and parent (not (eq? parent (root-node))))
        (string->symbol (cat (node-full-name (node-parent node))
                             "/" (node-name node)))
        (node-name node))))

  (define (node-add-element! node elem)
    (set! (node-elements node)
          (append (node-elements node) (list elem))))

  (defgeneric (node-children (node <node>)))
  (defmethod (node-children (node <node>))
    (node-elements node))

  (define (clear-node-state! node)
    (set! (node-has-expensive-handlers? node) #f)
    (set! (node-handlers node) (make-hash-table))
    (set! (node-allowed-values node) (make-hash-table))
    (set! (node-values node) (make-hash-table)))

  (defgeneric (register-node (node <node>)))

  (defmethod (register-node (node <node>))
    (let [[name (node-full-name node)]]
      (when (hash-table-get (node-table) name (lambda () #f))
        (error (cat "Duplicate copies of node " (node-full-name node))))
      (hash-table-put! (node-table) name node)))

  (define (unregister-node node)
    ;; This is only used to delete temporary <element> nodes, simulating
    ;; end-of-card rollback.
    (let [[name (node-full-name node)]]
      (assert (and (instance-of? node <element>)
                   (element-temporary? node)))
      (assert (eq? (hash-table-get (node-table) name (lambda () #f)) node))
      (hash-table-remove! (node-table) name)))

  (define (find-node name)
    (hash-table-get (node-table) name (lambda () #f)))

  (define (find-node-relative base name)
    ;; Treat 'name' as a relative path.  If 'name' can be found relative
    ;; to 'base', return it.  If not, try the parent of base if it
    ;; exists.  If all fails, return #f.
    (unless base
      (error (cat "Can't find relative path '@" name "' outside of a card")))
    (if (eq? base (root-node))
        (find-node name)
        (let* [[base-name (node-full-name base)]
               [candidate (string->symbol (cat base-name "/" name))]
               [found (find-node candidate)]]
          (or found (find-node-relative (node-parent base) name)))))

  (define (@* name &key (if-not-found
                         (lambda ()
                           (error (cat "Can't find relative path: @" name)))))
    (or (find-node-relative (current-group-member) name)
        (if-not-found)))

  (define-syntax @
    ;; Syntactic sugar for find-node-relative.
    (syntax-rules ()
      [(@ name)
       (@* 'name)]))

  (define (elem-or-name-hack elem-or-name)
    ;; Backwards compatibility glue for code which refers to elements
    ;; by name.  Used by functions such as WAIT.
    (assert (or (instance-of? elem-or-name <element>)
                (instance-of? elem-or-name <symbol>)))
    (node-full-name (if (element? elem-or-name)
                        elem-or-name
                        (begin
                          (debug-caution (cat "Change '" elem-or-name
                                              " to (@ " elem-or-name ")"))
                          (@* elem-or-name)))))

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
            (values (root-node) name)]
           [else
            (let [[parent (find-node (string->symbol (caddr matches)))]]
              (unless parent
                (error (cat "Parent of " name " does not exist.")))
              (values parent
                      (string->symbol (cadddr matches))))]))))

  (define-syntax define-node
    (syntax-rules ()
      [(define-node name group node-class (extended bindings ...)
         body ...)
       (begin
         (define name
           (with-values [[parent local-name] (analyze-node-name 'name)]
             (make node-class
               :group      'group
               :extends    extended
               :bindings-eval-fn (expand-bindings-eval-fn [] bindings ...)
               :init-fn    (expand-init-fn () body ...)
               :parent     parent
               :name       local-name)))
         (register-node name))]
      [(define-node name group node-class (bindings ...) body ...)
       (define-node name group node-class (#f bindings ...)
         body ...)]
      [(define-node name group node-class)
       (define-node name group node-class (#f))]))

  (define-syntax define-node-definer
    (syntax-rules ()
      [(define-node-definer definer-name group node-class)
       (define-syntax definer-name
         (syntax-rules ()
           [(definer-name name . rest)
            (define-node name group node-class . rest)]))]))

  ;;-----------------------------------------------------------------------
  ;;  Group Member
  ;;-----------------------------------------------------------------------
  ;;  A <group-member> may be stored in a <card-group>.

  (defclass <group-member> (<node>))

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

  (provide define-group-template group group-members)

  (define (card-or-card-group? node)
    (or (card? node) (card-group? node)))

  (defclass <card-group> (<group-member>)
    (members :accessor group-members :type <list> :initvalue '())
    (active? :type <boolean> :initvalue #f))

  (define (group-add-member! group member)
    (set! (group-members group)
          (append (group-members group) (list member))))

  (defmethod (node-children (group <card-group>))
    (append (group-members group) (call-next-method)))

  (defgeneric (card-group-find-next (group <card-group>) (member <jumpable>)))
  (defgeneric (card-group-find-prev (group <card-group>) (member <jumpable>)))

  (defmethod (card-group-find-next (group <card-group>) (member <jumpable>))
    #f)

  (defmethod (card-group-find-prev (group <card-group>) (member <jumpable>))
    #f)

  (define-template-definer define-group-template <card-group>)
  (define-node-definer group <card-group> <card-group>)

  ;;-----------------------------------------------------------------------
  ;;  Sequences of Cards
  ;;-----------------------------------------------------------------------
  ;;  Like groups, but ordered.

  (provide sequence)

  (defclass <card-sequence> (<jumpable> <card-group>))

  (defmethod (jump (target <card-sequence>))
    (if (null? (group-members target))
        (error (cat "Can't jump to sequence " (node-full-name target)
                    " because it contains no cards."))
        (jump (car (group-members target)))))

  (defmethod (card-group-find-next (group <card-sequence>) (member <jumpable>))
    ;; Find the node *after* member.
    (let [[remainder (memq member (group-members group))]]
      (assert (not (null? remainder)))
      (if (null? (cdr remainder))
          (card-group-find-next (node-parent group) group)
          (cadr remainder))))

  (defmethod (card-group-find-prev (group <card-sequence>) (member <jumpable>))
    ;; Find the node *before* member.
    (let search [[members (group-members group)]
                 [candidate-func 
                  (lambda ()
                    (card-group-find-prev (node-parent group) group))]]
      (assert (not (null? members)))
      (if (eq? (car members) member)
          (candidate-func)
          (search (cdr members) (lambda () (car members))))))

  (define-node-definer sequence <card-group> <card-sequence>)

  ;;-----------------------------------------------------------------------
  ;;  Cards
  ;;-----------------------------------------------------------------------
  ;;  More functions are defined in the next section below.

  (provide <card> card? card-next card-prev jump-next jump-prev
           define-card-template card)

  (defclass <card>          (<jumpable> <group-member>))

  (defmethod (jump (target <card>))
    (engine-jump-to-card *engine* target))

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
    (engine-register-card *engine* c))

  (define-template-definer define-card-template <card>)
  (define-node-definer card <card> <card>)

  ;;-----------------------------------------------------------------------
  ;; Elements
  ;;-----------------------------------------------------------------------

  (provide <element> element? define-element-template element
           default-element-parent call-with-default-element-parent
           with-default-element-parent create)

  (defclass <element>       (<node>)
    (temporary? :type <boolean> :initvalue #f))

  (define-template-definer define-element-template <element>)
  (define-node-definer element <element> <element>)
  
  (define (default-element-parent)
    (or (engine-default-element-parent *engine*)
        (current-card)))

  (define (call-with-default-element-parent node thunk)
    (let [[old (engine-default-element-parent *engine*)]]
      (dynamic-wind
          (lambda () (set! (engine-default-element-parent *engine*) node))
          thunk
          (lambda () (set! (engine-default-element-parent *engine*) old)))))

  (define-syntax with-default-element-parent
    (syntax-rules ()
      [(with-default-element-parent node . body)
       (call-with-default-element-parent node (lambda () . body))]))

  (define (create template
                  &key (name (gensym)) (parent (default-element-parent))
                  &rest-keys bindings)
    ;; Temporarily create an <element> node belonging to the current card,
    ;; using 'template' as our template.  This node will be deleted when we
    ;; leave the card.
    ;;
    ;; Someday I expect this to be handled by a transactional rollback of the
    ;; C++ document model.  But this will simulate things well enough for
    ;; now, I think.
    (define (bindings-eval-fn self)
      (bindings->hash-table bindings))
    (unless parent
      (error "Can't create element without a parent node"))
    (unless (node-running? parent)
      (error (cat "Cannot CREATE child on inactive node "
                  (node-full-name parent))))
    (assert (eq? (template-group template) '<element>))
    (let [[e (make <element>
               :group      '<element>
               :extends    template
               :bindings-eval-fn bindings-eval-fn
               :parent     parent
               :name       name
               :temporary? #t)]]
      (register-node e)
      (enter-node e)
      e))

  (define (eq-with-gensym-hack? sym1 sym2)
    ;; STUPID BUG - We name anonymous elements with gensyms, for
    ;; which (eq? sym (string->symbol (symbol->string sym))) is never
    ;; true.  This is dumb, and is evidence of a fundamental misdesign
    ;; in element management responsibilities.  I need to fix this ASAP.
    ;; But for now, this will allow the engine to limp along.
    (equal? (symbol->string sym1) (symbol->string sym2)))

  (define (delete-element-internal elem)
    ;; We're the master node deletion routine--C++ is no longer in charge.
    ;; TODO - We're called repeatedly as nodes get deleted, resulting
    ;; in an O(n^2) time to delete n nodes.  Not good, but we can live with
    ;; it for the moment.
    (let [[parent (node-parent elem)]]
      (set! (node-elements parent)
            (let recurse [[elements (node-elements parent)]]
              (cond
               [(null? elements) '()]
               [(eq? elem (car elements))
                ;; Delete this node, and exclude it from the new element list.
                (if (element-temporary? (car elements))
                    (begin
                      (exit-node (car elements))
                      (unregister-node (car elements)))
                    (debug-caution
                     (cat "Can't fully delete non-temporary element "
                          (node-full-name elem)
                          " in this version of the engine")))
                (engine-delete-element *engine* elem)
                (recurse (cdr elements))]
               [else
                ;; Keep this node.
                (cons (car elements) (recurse (cdr elements)))])))))
  

  ;;=======================================================================
  ;;  Changing Cards
  ;;=======================================================================

  ;; We call these function whenever we enter or exit a node.  They never
  ;; recurse up or down the node hierarchy; that work is done by other
  ;; functions below.
  (defgeneric (exit-node (node <node>)))
  (defgeneric (enter-node (node <node>)))

  (defmethod (exit-node (node <node>))
    ;; Exit all our child elements.
    ;; TRICKY - We call into the engine to do element deletion safely.
    ;; We work with a copy of (NODE-ELEMENTS NODE) the original
    ;; will be modified as we run.
    (foreach [elem (node-elements node)]
      (delete-element-internal elem))
    ;; Unregister our state-db listeners, if we have any.
    (engine-state-db-unregister-listeners *engine* node)
    ;; Run any exit handler.
    (run-on-exit-handler node)
    ;; Mark this node as no longer running, so nobody tries to call ON
    ;; or CREATE on it.
    (assert (node-running? node))
    (set! (node-running? node) #f)
    ;; Clear our handler list.
    (clear-node-state! node))

  (defmethod (enter-node (node <node>))
    ;; TODO - Make sure all our template properties are bound.
    ;; Mark this node as running so we can add event handlers and CREATE
    ;; children.
    (assert (not (node-running? node)))
    (set! (node-running? node) #t)
    ;; Enter all our child elements.  Notice we do this first, so
    ;; all the elements are available by the time we run the node body.
    ;; Unfortunately, this means that "new element" events generated
    ;; during element initialization can't be caught by the node.
    ;; Weird, but this is what the users voted for.
    (foreach [elem (node-elements node)]
      (enter-node elem))
    ;; Initialize our templates one at a time.
    (let recurse [[template node]]
      (when template
        ;; We bind property values on the way in, and run init-fns
        ;; on the way out.
        (node-bind-property-values! node template)
        (recurse (template-extends template))
        ;; Pass NODE to the init-fn so SELF refers to the right thing.
        ((template-init-fn template) node)))
    ;; Make sure all the properties of this node were declared somewhere.
    (node-check-for-unexpected-properties node)
    ;; Let the node know all initialization functions have been run.
    ;; (This allows "two-phase" construction, where templates can effectively
    ;; send messages to subtemplates.)
    (send/nonrecursive* (lambda () #f) node 'setup-finished))

  (defmethod (exit-node (group <group-member>))
    (call-next-method)
    ;; Reset our current group member.
    (set! (engine-current-group-member *engine*) (node-parent group)))

  (defmethod (enter-node (group <group-member>))
    ;; Set our current group member.
    (set! (engine-current-group-member *engine*) group)
    (call-next-method))

  (defmethod (exit-node (group <card-group>))
    (set! (card-group-active? group) #f)
    (call-next-method))

  (defmethod (enter-node (group <card-group>))
    (call-next-method)
    (set! (card-group-active? group) #t))

  (defmethod (exit-node (card <card>))
    (engine-notify-exit-card *engine* card)
    (set! (engine-last-card *engine*) card)
    (call-next-method))

  (defmethod (enter-node (card <card>))
    (engine-notify-enter-card *engine* card)
    (call-next-method)
    (engine-notify-card-body-finished *engine* card))

  (define (find-active-parent node)
    ;; Walk back up the node hierarchy from node until we find the
    ;; nearest active parent group.  The root node is always active, so
    ;; this recursive call terminates.
    (if (and (card-group? node) (card-group-active? node))
        node
        (find-active-parent (node-parent node))))

  (define (find-group card-or-group)
    ;; Walk back up our node hierarchy looking for the first group
    ;; we find.
    (if (card-group? card-or-group)
        card-or-group
        (find-group (node-parent card-or-group))))

  (define (exit-card-group-recursively group stop-before)
    ;; Recursively exit nested card groups starting with 'group', but
    ;; ending before 'stop-before'.
    (let recurse [[group group]]
      (unless (eq? group stop-before)
        (assert (node-parent group))
        (exit-node group)
        (recurse (node-parent group)))))

  (define (enter-card-group-recursively group)
    ;; Recursively enter nested card groups until we have a chain of
    ;; active groups from the root to 'group'.
    (let recurse [[group group]]
      (unless (card-group-active? group)
        (assert (node-parent group))
        (recurse (node-parent group))
        (enter-node group))))
  
  (define (node-or-elements-have-expensive-handlers? node)
    ;; See if NODE or any of its elements have expensive handlers.
    (or (node-has-expensive-handlers? node)
        (let recurse [[elements (node-elements node)]]
          (if (null? elements)
              #f
              (or (node-or-elements-have-expensive-handlers? (car elements))
                  (recurse (cdr elements)))))))
  
  (define (maybe-enable-expensive-events-for-card card)
    ;; REGISTER-EVENT-HANDLER attempts to turn on expensive events whenever
    ;; a matching handler is installed.  But we need to reset the
    ;; expensive event state when changing cards.  This means we need
    ;; to pay close attention to any nodes which live longer than a card.
    ;;
    ;; TODO - We could think of much better ways of handling this, I think.
    (let [[enable? #f]]
      (let recurse [[node card]]
        (when node
          (if (node-or-elements-have-expensive-handlers? node)
              (set! enable? #t)
              (recurse (node-parent node)))))
      (engine-enable-expensive-events *engine* enable?)))

  (define (run-card new-card)
    ;; Finish exiting whatever we're in.
    (let [[old-node (engine-current-group-member *engine*)]]
      (when old-node
        ;; If old-node is a card, exit it.
        (when (instance-of? old-node <card>)
          (exit-node old-node))
        ;; Exit as many enclosing card groups as necessary, stopping with
        ;; the nearest group which contains NEW-CARD.
        (exit-card-group-recursively (find-group old-node)
                                     (find-active-parent new-card))))
    
    ;; Update our expensive event state.
    (maybe-enable-expensive-events-for-card new-card)
    
    ;; Actually run the card.
    (debug-log (cat "Begin card: <" (node-full-name new-card) ">"))
    (with-errors-blocked (non-fatal-error)
      ;; Enter any enclosing card groups which we haven't entered yet.
      (enter-card-group-recursively (node-parent new-card))
      ;; Enter our card.
      (enter-node new-card)))
    
  (define (run-on-exit-handler node)
    ;; This is pretty simple--just send an EXIT message.  But we need to
    ;; trap any JUMP calls and quit immediately, because actually allowing
    ;; the jump will hopeless corrupt the data structures in this file.
    ;; Other errors we can simply trap and display.
    (define exited-normally? #f)
    (dynamic-wind
        (lambda () #f)
        (lambda ()
          (with-errors-blocked (non-fatal-error)
            (send/nonrecursive* (lambda () #f) node 'exit)
            (set! exited-normally? #t)))
        (lambda ()
          (unless exited-normally?
            (fatal-error (cat "Cannot JUMP in (on exit () ...) handler for "
                              (node-full-name node)))))))
  
  )
