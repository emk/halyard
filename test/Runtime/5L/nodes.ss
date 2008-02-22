(module nodes (lib "language.ss" "5L")

  ;; Various support code and declarations refactored out of the kernel.
  (require (lib "types.ss" "5L"))
  (provide (all-from (lib "types.ss" "5L")))
  (require (lib "util.ss" "5L"))
  (provide (all-from (lib "util.ss" "5L")))
  (require-for-syntax (lib "util.ss" "5L"))
  (require-for-syntax (lib "syntax-util.ss" "5L"))
  (require (lib "hook.ss" "5L"))

  ;; Get begin/var.
  (require (lib "begin-var.ss" "5L"))

  ;; Require our macro-related helpers.
  (require-for-syntax (lib "capture.ss" "5L"))
  (require (lib "indent.ss" "5L"))

  
  ;;=======================================================================
  ;;  Stuff Callable by the Kernel
  ;;=======================================================================
  ;;  This should not be exported any further than the kernel, unless
  ;;  otherwise specified.

  (provide run-card jump
           delete-element
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

  (define (current-card)
    (let [[result (*engine* .current-card)]]
      (unless result
        (error "Can't get current card when no card is active"))
      result))
  
  (define (last-card-visited)
    (*engine* .last-card-visited))
  
  
  ;;=======================================================================
  ;;  Object Model
  ;;=======================================================================

  ;;-----------------------------------------------------------------------
  ;;  Templates
  ;;-----------------------------------------------------------------------
  
  (provide setup run *node-defined-hook* define-node define-node-definer)

  ;; These objects are distinct from every other object, so we use them as
  ;; unique values.
  (define $no-default (list 'no 'default))
  (define $no-such-key (list 'no 'such 'key))

  (define-syntax define-def-and-super-abbrev
    (syntax-rules ()
      [(_ name)
       (define-syntax (name stx)
         (syntax-case stx ()
           [(_ . body)
            (quasisyntax/loc stx
              (#,(make-self #'body) .define-method 'name
               (method ()
                 (super)
                 (instance-exec self (method () . body)))))]))]))

  (define-def-and-super-abbrev setup)
  (define-def-and-super-abbrev run)

  (define-class %node% ()
    (with-instance (.class)

      ;;; Accept a :label argument (in addition to all the usual ones for
      ;;; ATTR), and ignore it.  This is theoretically a human-readable
      ;;; name for an attribute which we hope someday to use in the GUI.
      (def (attr name &key (label #f) &rest keys)
        (super))

      ;;; If the method NAME is not handled by a given object,
      ;;; automatically pass it to our parent object.  If the message tries
      ;;; to propagate out of the root object, raise an error.  To prevent
      ;;; the error, you can either provide a last-resort handler on the
      ;;; root node:
      ;;;
      ;;;   (with-instance (static-root-node)
      ;;;     (def (my-handler)
      ;;;       (do-last-resort-behavior)))
      ;;;
      ;;; ...or pass a METHOD to :if-not-handled.
      (def (always-propagate name &key if-not-handled)
        (%node% .define-method name
         (method args
           (if (.parent)
             ((.parent) .send name args)
             (if if-not-handled
               (apply instance-exec self if-not-handled args)
               (error (cat "." name " propagated to root node without "
                           "being handled")))))))
      
      ;;; Define a (basically) null method which sets a flag when it
      ;;; gets called.  This flag can be checked later to make sure that
      ;;; nobody forgot to call (SUPER).
      (def (define-method-with-mandatory-super name)
        (.attr (symcat "called-" name "?")
               :default (method () #f) :writable? #t)
        (.define-method name
          (method ()
            (.send (symcat "set-called-" name "?!") '(#t))
            (void))))

      (def (new &rest keys)
        (define obj (super))
        ;; Check to make sure that somebody actually set up all our slots
        ;; properly.  We check NAME because we know it should always be
        ;; there, but PARENT or any of several other slots would work as
        ;; well.  Note that we can't use CALL-METHOD-WITH-MANDATORY-SUPER
        ;; or anything like that because we don't have enough machinery set
        ;; up if nobody called .INITIALIZE.
        (unless (with-instance obj (has-slot? 'name))
          ;; Make sure we don't include OBJ anywhere in this error string,
          ;; becaue it's lying about being initialized, and will generally
          ;; make .to-string very sad.
          (error (cat "Called .initialize on " self
                      ", but someone forgot to call SUPER")))
        obj)

      )

    ;;; Call method .name if it exists, otherwise propagate to our parent
    ;;; recursively.
    (def (propagate name &rest args)
      (cond
       [(.responds-to? name) 
        (.send name args)]
       [(root-node? self) 
        (error (cat "." name " propagated to root node without "
                    "being handled"))]
       [else ((.parent) .send 'propagate (list* name args))]))

    ;; Call a "mandatory method" (see DEFINE-METHOD-WITH-MANDATORY-SUPER),
    ;; and make sure that (SUPER) was called by any methods which override
    ;; it.
    (def (call-method-with-mandatory-super name)
      (.send name '())
      (unless (.send (symcat "called-" name "?") '())
        (error (cat "Called " self " ." name
                    ", but someone forgot to call SUPER"))))

    ;;; Create the actual C++ representation of this node.  This should be
    ;;; overrridden by anything that needs to create a corresponding C++
    ;;; object.
    ;;;
    ;;; Note that currently only elements have any sort of representation
    ;;; in the C++ layer.  We'd like to fix this, so that the engine
    ;;; actually knows about all nodes.
    (def (create-engine-node)
      (void))

    ;;; Create any child elements, and do one-time initialization.  We
    ;;; assume that .SETUP will be called in both normal runtime mode, and
    ;;; in editing mode (assuming we ever get a GUI editor).
    ;;;
    ;;; TODO - We are hand-rolling most of
    ;;; define-method-with-mandatory-super here, so a little refactoring
    ;;; couldn't hurt.
    (attr called-setup? #f :writable? #t)
    (def (setup)
      (set! (.called-setup?) #t)
      (.instantiate-static-elements))

    ;;; A few parent classes in our system need to run small fragments
    ;;; of code *after* child classes finish their setup.  For now, we'll
    ;;; rely on .SETUP-FINSIHED to handle these cases.
    ;;; TODO - Can we get rid of .SETUP-FINISHED in our new system?
    (.define-method-with-mandatory-super 'setup-finished)

    ;;; Run the main body of this node.  This really only makes sense for 
    ;;; %GROUP-MEMBER%s, and we may remove it from elements in the future. 
    (.define-method-with-mandatory-super 'run)

    ;;; This method is called as a node is being exited (jumped away from 
    ;;; or deleted).
    (.define-method-with-mandatory-super 'exit)

    )

  ;; Called when a node is defined.
  (define *node-defined-hook* (make-hook 'node-defined-hook))

  (define-syntax (thunked-alist<-bindings stx)
    (syntax-case stx ()
      [(_) (quasisyntax/loc stx '())]
      [(_ key value . rest)
       (and (keyword? (syntax-object->datum #'key))
            (not (keyword? (syntax-object->datum #'value))))
       (quasisyntax/loc stx
         (cons (cons (keyword-name key) (method () value))
               (thunked-alist<-bindings . rest)))]
      [(_ . rest)
       (raise-syntax-error 'define-node "Malformed keyword list" #'rest)]))

  (define-syntax (define-node-internal stx)
    (syntax-case stx ()
      [(_ name (extended . bindings) . body)
       (begin
         (check-syntax-is-symbol 'define-node #'name "Name must be a symbol")
         (check-syntax-is-symbol 'define-node #'extended
                                 "Superclass must be a symbol")
         (quasisyntax/loc stx
           (define-class name (extended)
             ;; HACK - Hygiene problems with SELF, so we use the explicit
             ;; name everywhere.
             (name .process-static-node-name 'name)
             (name .values-with-instance-parent
                   (thunked-alist<-bindings . bindings))
             ;; This WITH-INSTANCE exists only to make sure that SELF gets
             ;; rebound in a way that's visible to BODY.  Yay hygienic
             ;; macros.
             (with-instance name . body)
             (name .register))))]))

  (define-syntax (define-node stx)
    (syntax-case stx ()
      ;; Some kinds of nodes may be lexically nested; others can't.  If
      ;; we find a SELF variable in our surrounding lexical context, we
      ;; pass it to register-with-lexical-parent and let the class sort
      ;; it out.
      [(_ name bindings . body)
       ;; Do we see a SELF in our scope?
       (identifier-binding (make-self #'name))
       (quasisyntax/loc stx
         ;; The outermost LET allows us to hide NAME from
         ;; everybody, so that we don't shadow uses of NAME in either
         ;; our calling context or in our BODY.  This is also why we
         ;; expand BODY using WITH-INSTANCE, and don't hand it over to
         ;; DEFINE-NODE-HELPER.
         ;;
         ;; The inner LET [] here gives us a form that allows initial
         ;; (begin (define ...) ...) forms, which are needed to expand
         ;; DEFINE-CLASS.
         (let [[name% (let [] (define-node-internal name bindings))]]
           (with-instance name% . body)
           (name% .register-with-lexical-parent #,(make-self #'name))))]
      ;; We don't have SELF, so expand in the ordinary fashion.
      [(_ name . rest)
       (quasisyntax/loc stx
         (define-node-internal name . rest))]))

  (define-syntax define-node-definer
    (syntax-rules ()
      ;; TODO - We should check that any supplied superclass is actually
      ;; a subclass of DEFAULT-SUPERCLASS.
      [(_ definer-name default-superclass)
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


  ;;-----------------------------------------------------------------------
  ;;  Nodes
  ;;-----------------------------------------------------------------------
  ;;  A program is (among other things) a tree of nodes.  The root node
  ;;  contains %card%s and %card-group%s.  %card-group%s contain %card%s
  ;;  and other %card-group%s.  Any node may contain %element%s.

  (provide %node% node? node-name node-full-name node-parent
           node-elements find-node find-running-node find-static-node
           find-child-node make-node-type-predicate)

  (define (add-shared-node-members! inst)
    (with-instance inst
      (def (register-in table)
        (let [[name (.full-name)]]
          (when (hash-table-has-key? table name)
            (error (cat "Duplicate copies of node " name)))
          (hash-table-put! table name self)))

      (def (unregister-from table)
        (let [[name (.full-name)]]
          (%assert (hash-table-has-key? table name))
          (hash-table-remove! table name)))
      
      (def (full-name)
        (let [[parent (.parent)]]
          (if (and parent (not (root-node? parent)))
            (string->symbol (cat (parent .full-name) "/" (.name)))
            (.name))))
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

    ;; Attributes shared by all nodes.
    ;;
    ;; TODO - The whole wants-cursor? system is fairly strange, because it
    ;; really only applies to elements (at least right now), and yet we
    ;; declare this variable for all nodes.
    (attr wants-cursor? #f :label "Wants cursor?" :writable? #t)

    (def (active?)
      (eq? (.node-state) 'ACTIVE))

    (def (to-string)
      (check-for-initialization self 'to-string)
      (cat "#<inst " (.full-name) ">"))

    (def (register)
      (.register-in (*engine* .running-node-table)))

    (def (unregister)
      (.unregister-from (*engine* .running-node-table)))

    ;; Instantiate all the static elements defined anywhere in our class
    ;; hierarchy.
    (def (instantiate-static-elements)
      ;; Starting from %node%, walk down the class hierarchy and
      ;; instantiate all our statically-declared elements.
      (let recurse [[klass (.class)]]
        (unless (eq? klass %node%)
          (recurse (klass .superclass)))
        (foreach [static-elem (klass .elements)]
          ;; It is *very* important that we pass in :parent explicitly
          ;; here, if we want the nested node syntax to support (.parent)
          ;; in initializers.  This guarantees that (.parent) is
          ;; immediately available on our %initializer-keywords% object,
          ;; and not set by a (default ...) statement too late to do any
          ;; good.
          (static-elem .new :parent self :name (static-elem .name)))))

    ;;; Find a child element by name.
    ;;; TODO - We may rename this; I added it to help Robinson port our
    ;;; library code.
    (def (find-elem name)
      (find-child-node self name #t))

    (with-instance (.class)
      (attr name #f :writable? #t)   ; May be #f if class not in hierarchy.
      (attr parent #f :writable? #t)
      (attr elements '() :writable? #t)

      (def (to-string)
        (if (.name)
          (cat "#<class " (.full-name) ">")
          (super)))

      ;;; Returns #t if this node is part of the hierarchy of named nodes,
      ;;; and can be run directly.  TODO - We may want to name this better.
      ;;; It corresponds to things defined by DEFINE-NODE, and not by
      ;;; ordinary DEFINE-CLASS.
      (def (can-be-run?)
        (and (.name) #t))

      ;; Given the name of a static node, set up the .NAME and .PARENT
      ;; fields appropriately.
      (def (process-static-node-name name)
        (with-values [[parent local-name] (analyze-node-name name)]
          (check-node-name local-name)
          (set! (.name) local-name)
          (set! (.parent) parent)))

      (def (register)
        (.register-in (*engine* .static-node-table)))

      ;; Register this static element with its lexically-surrounding parent
      ;; node, or raise an error if it can't be nested.
      (def (register-with-lexical-parent parent)
        (error (cat self " may not be nested within another node")))

      ;; Given an element class with a properly set-up name and parent,
      ;; register it with this class so that it can be created at runtime
      ;; by the default SETUP method.
      (def (register-static-element elem)
        (%assert (elem .name))
        (%assert (eq? self (elem .parent)))
        ;; Add this element to our static child element list.
        (set! (.elements) (append (.elements) (list elem)))
        ;; Declare a helper method which can be called at runtime to find
        ;; the running version of our child element.
        (.define-method (elem .name) 
          (method () (find-child-node self (elem .name) #t))))

      ;; Takes a list of pairs of names and methods and turns each into the
      ;; equivalent of:
      ;;
      ;;  (VALUE name (instance-exec (.parent) meth)). 
      ;;
      ;; This is for the keyword argument lists in DEFINE-NODE.
      (def (values-with-instance-parent alist)
        (foreach [(cons name meth) alist]
          (.attr-initializer name
                             (method ()
                               (instance-exec (.parent) meth))
                             #f)))
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
  (define (node-full-name node-or-path) (node-or-path .full-name))

  (define (check-for-duplicate-nodes node-list node)
    (let recurse [[node-list node-list]]
      (cond
       [(null? node-list)
        #f]
       [(eq? (node-name node) (node-name (car node-list)))
        (error (cat "Duplicate node: " (node .full-name)))]
       [#t
        (recurse (cdr node-list))])))

  (define (node-add-element! node elem)
    ;; We need to check for duplicates before adding or we violate
    ;; some pretty obvious invariants.
    ;; TODO - Why don't we call this for nodes other than elements?
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
  
  ;; This is an internal function used for gluing together a base name and
  ;; a node name, and doing a lookup.  The API is subject to change!
  (define (find-child-node base name running?)
    (if (root-node? base)
      (find-node name running?)
      (let* [[base-name (base .full-name)]
             [candidate (string->symbol (cat base-name "/" name))]]
        (find-node candidate running?))))

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
  ;;  TODO - Some of non-element-related code on %node% should move here.

  (define-class %group-member% (%node%)
    (with-instance (.class)
      (def (register)
        (super)
        (when (node-parent self)
          (group-add-member! (node-parent self) self))
        ;; TODO - We currently only call this for %group-member%s, and not
        ;; for %element%s, because that's the way it used to work.  We
        ;; might just want to update all this to a new hook system soon.
        ;; Note that this hook gets called before
        ;; .register-with-lexical-parent, if that's the sort of thing you
        ;; care about.
        (call-hook-functions *node-defined-hook* self)))

    ;;; Return the static version of this node.
    (def (static-node)
      (define result (.class))
      (%assert (result .can-be-run?))
      result)
    )

  ;;-----------------------------------------------------------------------
  ;;  Jumping
  ;;-----------------------------------------------------------------------

  ;;; Jump to the specified node.
  (define (jump node)
    ;; We keep this wrapper function around for nostalgia value.
    (node .jump))

  
  ;;-----------------------------------------------------------------------
  ;;  Groups of Cards
  ;;-----------------------------------------------------------------------
  ;;  By default, groups of cards are not assumed to be in any particular
  ;;  linear order, at least for purposes of program navigation.

  (provide group %card-group% group-members card-group?)

  (define (card-or-card-group? node)
    (or (card? node) (card-group? node)))

  (define-class %card-group% (%group-member%)
    (with-instance (.class)
      (attr members '() :type <list> :writable? #t)))

  ;; TODO - Eliminate these wrappers.
  (define card-group? (make-node-type-predicate %card-group%))
  (define (card-group-members grp) (grp .members))
  (define (set-card-group-members! grp val) (set! (grp .members) val))
  (define (card-group-active? grp) (grp .active?))

  (define group-members card-group-members)
  (define set-group-members! set-card-group-members!)

  (define (group-add-member! group member)
    ;; We need to check for duplicates before adding or we violate
    ;; some pretty obvious invariants.
    (check-for-duplicate-nodes (group-members group) member)
    (set! (group-members group)
          (append (group-members group) (list member))))

  (define-node-definer group %card-group%)

  ;;-----------------------------------------------------------------------
  ;;  The root node
  ;;-----------------------------------------------------------------------

  (provide %root-node%)

  (define-class %root-node% (%card-group%)
    (set! (.name) '|/|)
    (value name (%root-node% .name)) ; Copy class name attr to instance.
    (value parent #f))

  ;;-----------------------------------------------------------------------
  ;;  Cards
  ;;-----------------------------------------------------------------------
  ;;  More functions are defined in the next section below.

  (provide %card% card? card jump-current)

  (define-class %card% (%group-member%)
    (with-instance (.class)
      (def (register)
        (super)
        (*engine* .register-card self))
      
      (def (jump)
        (*engine* .jump-to-card self))
      )

    ;; This is a fairly easy mistake to make.
    (def (jump)
      (error (cat "Cannot jump to running card " self ", jump to static "
                  "counterpart " (.class) " instead.")))
    )

  ;; TODO - Get rid of wrapper functions.  
  (define card? (make-node-type-predicate %card%))
  
  (define-node-definer card %card%)
  
  (define (jump-current)
    ((current-card) .static-node))

  
  ;;-----------------------------------------------------------------------
  ;; Elements
  ;;-----------------------------------------------------------------------

  (provide element? %element% elem
           default-element-parent call-with-default-element-parent
           with-default-element-parent)

  ;; The rest of this definition lives in tamale.ss.
  ;; TODO - Do we need to think about the division of responsibilities
  ;; here?
  (define-class %element% (%node%)
    (default name (gensym))
    (default parent (default-element-parent))

    (def (initialize &rest keys)
      (super)
      (unless (.parent)
        (error (cat (.class) " .new: Can't create '" (.name)
                    " without a parent node")))
      (unless (symbol? (.name))
        (error (cat (.class) " .new: name must be a symbol; given " (.name))))
      (check-node-name (.name))
      (node-add-element! (.parent) self))

    (with-instance (.class)
      ;;; We call .enter-node on elements as soon as we create them (unlike
      ;;; cards, which handle this in a slightly different way).  It's
      ;;; important that we don't call .enter node until _after_
      ;;; ruby-object-initialized? is set to #f by the default .new method,
      ;;; or else .setup and .run will appear to be running inside the
      ;;; constructor.  So we can't put this code in .initialize!
      (def (new &rest keys)
        (define elem (super))
        (elem .enter-node)
        elem)

      (def (process-static-node-name name)
        ;; We don't actually need to extract a .PARENT from NAME here,
        ;; because elements aren't allowed to have compound names.  If we
        ;; ever decide to allow a nesting syntax for CARD, etc., similar to
        ;; that of ELEM, we would then modify %node% to do something
        ;; similar.
        (check-node-name name)
        (set! (.name) name))

      (def (register)
        ;; We don't need to register elements in the global table the same
        ;; way we handle cards, etc.
        (void))

      ;; Register this static element with its lexically-surrounding parent
      ;; node.  This is how nested ELEM forms find their containing
      ;; classes.
      (def (register-with-lexical-parent parent)
        (%assert (.name))
        (set! (.parent) parent)
        (parent .register-static-element self))
      )
      
    )

  ;; TODO - Get rid of wrapper functions.  
  (define element? (make-node-type-predicate %element%))

  ;; XXX - The default superclass here needs to be %custom-element%, but
  ;; I don't want to drag that whole beast into this file.
  (define-node-definer elem %element%)
  
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

  ;;; This is the official public API for deleting an element.
  (define (delete-element elem)
    (elem .%delete))

  (with-instance %element%
    (def (%delete)
      ;; We're the master node deletion routine--C++ is no longer in
      ;; charge.  TODO - We're called repeatedly as nodes get deleted,
      ;; resulting in an O(N^2) time to delete N child nodes of a given
      ;; parent node.  Not good, but we can live with it for the moment.
      (let [[parent (.parent)]]
        ;; Remove the node from its parent's NODE-ELEMENTS first, to
        ;; reduce the odds that a script can find the node mid-deletion.
        (set! (parent .elements)
              (filter (lambda (e) (not (eq? self e)))
                      (parent .elements)))
        ;; Now it's safe to delete the node.
        (.exit-node)
        (*engine* .delete-element self)))
    (.seal-method! '%delete))
  
  
  ;;=======================================================================
  ;;  Changing Cards
  ;;=======================================================================
  ;;  We call these function whenever we enter or exit a node.  They never
  ;;  recurse up or down the node hierarchy; that work is done by other
  ;;  functions below.

  (with-instance %node%
    (def (enter-node)
      ;; TODO - Make sure all our template properties are bound.  Mark this
      ;; node as running so we can call .NEW on elements.
      (%assert (eq? (.node-state) 'ENTERING))
      ;; Register this node in the table of running nodes.
      (.register)
      ;; Because we haven't been running, we shouldn't have any child
      ;; elements yet.
      (%assert (null? (node-elements self)))
      ;; Let the world know that we're starting to run this node.
      (.notify-enter)
      ;; If we have an associated engine node, create it now.
      (.create-engine-node)
      ;; Do initial setup, and create any child elements.
      (.call-method-with-mandatory-super 'setup)
      ;; Let the node know all initialization functions have been run.
      ;; (This allows "two-phase" construction, where templates can effectively
      ;; send messages to subtemplates.)
      (.call-method-with-mandatory-super 'setup-finished)
      ;; Run our body function.  This may take quite a while, play media,
      ;; and so on--this is where "card" bodies actually get run.
      (.call-method-with-mandatory-super 'run)
      ;; Let the hook system know that we've finished card body.  This is
      ;; theoretically useful for various sorts of "card completed" systems
      ;; and possibly for automated testing.
      (.notify-body-finished)
      (set! (.node-state) 'ACTIVE))

    ;; XXX - These are actually internal functions for overring by
    ;; nodes.ss.  If you want public access to them, it will probably be
    ;; necessary to actually tidy this stuff up.
    (def (notify-enter))
    (def (notify-body-finished))
    (def (notify-exit))
    (def (notify-reached-trunk)
      ;; This function actually exists so that events.ss can attach a hook
      ;; here using ADVISE AFTER.
      )

    (def (exit-node)
      (%assert (memq (.node-state) '(ENTERING ACTIVE)))
      (set! (.node-state) 'EXITING)
      (.notify-exit)
      ;; Exit all our child elements.
      ;; TRICKY - We call into the engine to do element deletion safely.
      ;; We work with a copy of (NODE-ELEMENTS SELF) the original
      ;; will be modified as we run.
      (foreach [elem (node-elements self)]
        (delete-element elem))
      ;; Unregister our state-db listeners, if we have any.
      (*engine* .exit-node self)
      ;; Run any exit handler.
      (run-on-exit-handler self)
      ;; Unregister from the running node table.
      (.unregister)
      ;; Mark this node as no longer active.  This can be checked by
      ;; assertions, if that helps anybody.
      (set! (.node-state) 'EXITED))
    )

  (with-instance %card%
    (def (notify-enter)
      (*engine* .notify-enter-card self))

    (def (notify-body-finished)
      (*engine* .notify-card-body-finished self))

    (def (notify-exit)
      (*engine* .notify-exit-card self)
      (set! (*engine* .last-card-visited) (.static-node)))
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
      ;; It is *very* important that we pass in :parent explicitly here, if
      ;; we want the nested node syntax to support (.parent) in
      ;; initializers.  This guarantees that (.parent) is immediately
      ;; available on our %initializer-keywords% object, and not set by a
      ;; (default ...) statement too late to do any good.
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

      ;; We've gone down one branch, and we're ready to up another.  But
      ;; send an internal message here so it can be hooked by events.ss, which
      ;; needs to update the expensive event state.
      (trunk-node-inst .notify-reached-trunk)

      ;; Actually run the card.
      (debug-log (cat "Begin card: <" (new-card-class .full-name) ">"))
      (with-errors-blocked (non-fatal-error)
        (enter-node-recursively new-card-class trunk-node-inst))))

  (define *running-on-exit-handler-for-node* #f)

  (define (run-on-exit-handler node)
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
              (node .call-method-with-mandatory-super 'exit))
            (set! exited-safely? #t)))
        (lambda ()
          (unless exited-safely?
            (fatal-error (cat "Cannot JUMP in (on exit () ...) handler for "
                              (node .full-name)))))))

  )
