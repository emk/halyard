(module ruby-objects (lib "swindle.ss" "swindle")
  (require (lib "struct.ss")) ; For DEFINE-STRUCT/PROPERTIES.
  (require-for-syntax (lib "capture.ss" "halyard"))
  (require (lib "util.ss" "mizzen"))
  (require-for-syntax (lib "util.ss" "mizzen"))
  (require-for-syntax (lib "syntax-util.ss" "halyard"))
  (require (lib "begin-var.ss" "mizzen"))
  
  ;; For setting up SELF and SUPER as syntax-parameters
  (require (lib "stxparam.ss" "mzlib"))
  (require-for-syntax (lib "default-self.ss" "halyard"))

  (provide ruby-object? make-ruby-instance-of?-predicate
           ruby-class? define-class %class% %object% 
           self super method~ send def
           instance-exec with-instance attr value default advise)

  ;; This will be overridden later on, once we have .TO-STRING set up
  ;; properly.
  (define *ruby-object-printer* #f)
     
  ;; Print an object.  Note that we substitute a simple print routine until
  ;; the real one gets installed.
  (define (print-ruby-object obj port write?)
    (if *ruby-object-printer*
      (*ruby-object-printer* obj port write?)
      ;; TODO - I'm not sure that it's always safe to try to CAT the return
      ;; value of ruby-object-class at this code--it might recurse
      ;; infinitely depending on where we are in the bootstrap process.
      (write-string (cat "#<" (ruby-object-klass obj) " ???>") port)))

  ;; Internal implementation of Ruby objects.  We use
  ;; DEFINE-STRUCT/PROPERTIES here because it's faster than relying on
  ;; Swindle, but still allows us to define a custom write routine.
  (define-struct/properties ruby-object
    [klass
     slots
     ;; Has the initialization protocol (if any) for this object been
     ;; finished?
     initialized?]
    [[prop:custom-write print-ruby-object]])

  (define (new-ruby-object klass)
    (make-ruby-object klass
                      (make-hash-table)  ; slots
                      #f))               ; initialized?

  ;;; Return a function which returns #t if and only if OBJ is an instance
  ;;; of KLASS.
  (define (make-ruby-instance-of?-predicate klass)
    (lambda (obj)
      (and (ruby-object? obj)
           (app~ obj .instance-of? klass))))

  ;; We really need some error-checking for this.
  (define (ruby-object-class obj)
    (if (ruby-object? obj)
        (ruby-object-klass obj)
        (error (cat "Not a Ruby-style object: " obj))))

  ;; Every time the return value of APPLICABLE-METHODS might change (for
  ;; any set of arguments), this number must be incremented.
  (define *generation-id* 1)

  (define-struct (ruby-class ruby-object)
    [name
     superclass
     methods
     cached-methods-generation-id
     cached-methods
     method-seal-table])

  (define (new-ruby-class &key klass (initialized? #f) name superclass)
    (make-ruby-class klass
                     (make-hash-table)   ; slots
                     initialized? name superclass
                     (make-hash-table)   ; methods
                     0                   ; cached-methods-generation-id
                     (make-hash-table)   ; cached-methods
                     (make-hash-table))) ; method-seal-table

  (define-struct initializer
    [name method ignorable? skippable?])

  (define (slot% object name)
    (hash-table-get (ruby-object-slots object) name
                    (lambda ()
                      (error (cat "Can't find slot <" name "> on " object)))))

  (define (set-slot%! object name value)
    (hash-table-put! (ruby-object-slots object) name value))

  (define (has-slot%? object name)
    (hash-table-has-key? (ruby-object-slots object) name))

  (define (slot-or-default% object name default)
    (if (has-slot%? object name)
        (slot% object name)
        default))

  (define (method-sealed? klass method-name)
    (if (not klass)
      #f
      (hash-table-get (ruby-class-method-seal-table klass) method-name
                      (lambda ()
                        (method-sealed? (ruby-class-superclass klass)
                                        method-name)))))

  (define (set-method-sealed?! klass method-name value)
    (hash-table-put! (ruby-class-method-seal-table klass) method-name value))
  
  (define (add-method! object name method)
    (when (method-sealed? object name)
      (error (cat "Cannot override ." name " on " object
                  " without unsealing it")))
    (when (hash-table-has-key? (ruby-class-methods object) name)
      (error (cat "Cannot define ." name " twice on " object)))
    (inc! *generation-id*)
    (hash-table-put! (ruby-class-methods object) name method))

  (define (advise-method! object combination name meth)
    (define original
      (hash-table-get (ruby-class-methods object) name
                      (lambda () (method~ args (super)))))
    (define new-method 
      (case combination
        [[before] 
         (method~ args
           (apply instance-exec self meth args)
           (apply-method% name original self super args))]
        [[after] 
         (method~ args
           (let [[result (apply-method% name original self super args)]]
             (apply instance-exec self meth args)
             result))]
        [else (error "advise-method!: '" combination " is not a valid advice "
                     "combination.")]))
    (inc! *generation-id*)
    (hash-table-put! (ruby-class-methods object) name new-method))

  (define-syntax (define-class stx)
    (syntax-case stx ()
      [(_ klass () . body)
       (quasisyntax/loc stx
         (define-class klass (%object%) . body))]
      [(_ klass (super) . body)
       (begin
         (check-syntax-is-symbol 'define-class #'klass
                                 "Class name must be a symbol")
         (unless (eq? '%object% (syntax-object->datum #'klass))
           (check-syntax-is-symbol 'define-class #'super
                                   "Superclass must be a symbol"))

         (quasisyntax/loc stx
           (begin
             ;; We want to be able to define "class" methods on KLASS.  But
             ;; since we only support instance methods, we need to make
             ;; KLASS an instance of a "metaclass".  Metaclasses are
             ;; themselves classes.
             (define metaclass
               (new-ruby-class
                :name (symcat "metaclass:" 'klass)   ; name
                :klass %class%                       ; klass
                ;; The initialization protocol doesn't apply to metaclasses.
                :initialized? #t
                ;; The metaclass of %object% inherits from %class%.  Other
                ;; metaclasses inherit from SUPER's metaclass, so that
                ;; class methods are visible on a class and all its
                ;; subclasses.  For a full picture of what's going on here,
                ;; see ruby-objects.png.
                :superclass (if super
                                (ruby-object-class super)
                                %class%)))
             (define klass
               (new-ruby-class
                :name 'klass
                :klass metaclass
                :superclass super))
             ;; Call our initialize method to make sure any attribute
             ;; initializers get run.  This allows classes to have slots,
             ;; aiding metaprogrammers.  (%object% and
             ;; %initializer-keywords% are declared before we can safely
             ;; call initialize, so we skip this step for them.)
             (unless (or (eq? 'klass '%object%)
                         (eq? 'klass '%initializer-keywords%))
               (send klass 'initialize))
             (set! (ruby-object-initialized? klass) #t)
             (with-instance klass . body)
             klass)))]))
  
  (define-syntax-parameter self %default-self)
  (define-syntax-parameter super
    (lambda (stx)
      (raise-syntax-error #f "can only be used inside a method." stx)))
  
  (define-syntax method~
    (syntax-rules ()
      [(_ args . body)
       (lambda (self* super* . args)
         ;; Parameterize SELF and SUPER to refer to appropriate versions.
         (syntax-parameterize [[self (make-rename-transformer #'self*)]
                               [super (make-rename-transformer #'super*)]]
           (begin/var . body)))]))
  
  (define (instances-respond-to?% klass method-name)
    (if (not klass)
      #f
      (hash-table-get (ruby-class-methods klass) method-name 
                      (lambda () 
                        (instances-respond-to?% 
                         (ruby-class-superclass klass)
                         method-name)))))

  ;; Walk up the class hierarchy, making a list of all methods with a given
  ;; name.  The methods are sorted from most-specific to least-specific.
  ;; See the comment on *GENERATION-ID*--there are some non-trivial
  ;; correctness constraints here.
  ;;
  ;; TODO - Rely on methods that have already been cached.
  (define (applicable-methods klass method-name)
    (define (recurse)
      (applicable-methods (ruby-class-superclass klass) method-name))
    (if (not klass)
      '()
      (let [[method (hash-table-get (ruby-class-methods klass) method-name
                                    (lambda () #f))]]
        (if method
          (cons method (recurse))
          (recurse)))))

  ;; The front-end half of method dispatch.  This function handles method
  ;; caching, and then passes control to CALL-WITH-METHOD-LIST.
  (define (send object method-name . args)
    ;; This is a fairly common error, so give a good message.
    (unless (ruby-object? object)
      (error (cat "Cannot send ." method-name " to " object
                  ", because it is not a Ruby-style object.")))

    ;; Fetch various information about the class we're dispatching to.
    (let [[klass (ruby-object-klass object)]]

      ;; If the method cache isn't up-to-date, clear it.
      (define generation-id (ruby-class-cached-methods-generation-id klass))
      (unless (= generation-id *generation-id*)
        (%assert (< generation-id *generation-id*))
        (set! (ruby-class-cached-methods klass) (make-hash-table))
        (set! (ruby-class-cached-methods-generation-id klass) *generation-id*))

      ;; Uncomment this code to log a complete call chain.
      ;;(debug-log (cat "Calling ." method-name " on "
      ;;                (ruby-class-name klass)))
      
      ;; Look up the cached method list.  If we don't have it, create it.
      (let* [[cached-methods (ruby-class-cached-methods klass)]
             [methods (hash-table-get cached-methods method-name
                                      (lambda () #f))]]
        (unless methods
          (set! methods (applicable-methods klass method-name))
          (hash-table-put! cached-methods method-name methods))
        
        ;; Make the actual method call.
        (call-with-method-list object methods method-name args))))

  ;; Get user-visible arity of a method (2 less than the actual function 
  ;; arity, since there are two hidden arguments, SELF and SUPER).
  (define (ruby-method-arity m)
    (define arity (procedure-arity m))
    (cond 
      [(integer? arity) (- arity 2)]
      [(arity-at-least? arity) (make-arity-at-least 
                                (- (arity-at-least-value arity) 
                                   2))]
      [else (error (cat "Bad arity " arity " on method " m))]))

  ;; Apply a method to an object, giving an appropriate arity error if 
  ;; there is a mismatch.
  (define (apply-method% method-name method object super args)
    ;; Catch arity error before we try calling the method, so we can 
    ;; provide an error message that makes more sense.
    (unless (procedure-arity-includes? method (+ 2 (length args)))
      (apply raise-arity-error 
             (symcat "Method " object " ."
                     ;; If we're passed in a name, use that.  Otherwise, 
                     ;; use the inferred name that MzScheme gives to the
                     ;; function. 
                     (if method-name method-name (object-name method)))
             (ruby-method-arity method)
             args))
    
    (apply method object super args))
  
  ;; The back-end half of method dispatch.  This function takes a
  ;; precomputed list of methods to call, and when that list is exhausted,
  ;; hands further work to METHOD-MISSING.
  (define (call-with-method-list object methods method-name args)
    (if (null? methods)
      ;; No more methods to call, so dispatch this to method-missing.
      (apply send object 'method-missing method-name args)
      ;; Call the first method in the list, and give an implementation of
      ;; SUPER.
      (apply-method% method-name (car methods) object
                     (lambda ()
                       (call-with-method-list object (cdr methods) method-name 
                                              args))
                     args)))

  (define (instance-exec object method . args)
    (apply-method% #f method object 
                   (lambda () (error "Cannot call super using instance-exec"))
                   args))

  (define-syntax with-instance
    (syntax-rules ()
      [(_ object . body)
       (instance-exec object (method~ () . body))]))

  ;; TODO - Rename DEF -> ON after overhauling nodes.ss?
  (define-syntax def
    (syntax-rules ()
      [(_ (name . args) . body)
       (app~ .define-method 'name
             (let [[name (method~ args . body)]]
               name))]))
  
  (define-syntax (attr stx)
    (syntax-case stx ()
      [(_ name default . args)
       (not (keyword? (syntax-object->datum #'default)))
       (syntax/loc stx (attr name :default (method~ () default) . args))]
      [(_ name . args) 
       (quasisyntax/loc stx
         (app~ .attr 'name . args))]))

  (define-syntax (attr-initializer stx)
    (syntax-case stx ()
      [(_ name value ignorable? arg ...)
       (quasisyntax/loc stx
         (app~ .attr-initializer 'name
               (method~ () value) ignorable? arg ...))]))
  
  ;;; Specify the value to use for an attribute.
  (define-syntax value
    (syntax-rules ()
      [(_ name value arg ...)
       (attr-initializer name value #f arg ...)]))

  ;;; Specify the default to use for an attribute.
  (define-syntax default
    (syntax-rules ()
      [(_ name value arg ...)
       (attr-initializer name value #t arg ...)]))

  ;;; Wrap a method NAME with a bit of invisible extra code.  This is based
  ;;; on Lisp's DEFADVICE macro.
  (define-syntax (advise stx)
    (syntax-case stx ()
      [(_ combination (name arg ...) body ...)
       ;; Hack to give the advise method a vaguely appropriate inferred name.
       (let [[debugging-name 
              (datum->syntax-object
               #'name
               (symcat (syntax-object->datum #'combination)
                       " "
                       (syntax-object->datum #'name)))]]
         (quasisyntax/loc stx
           (app~ .advise-method 'combination 'name
                 (let [[#,debugging-name (method~ (arg ...) body ...)]]
                   #,debugging-name))))]))


  ;;=======================================================================
  ;;  Setting Up %class% and %object%
  ;;=======================================================================
  ;;  This is very tricky, %class% and %object% (and their respective
  ;;  metaclasses) have many circular dependencies.  For a diagram, see
  ;;  ruby-objects.png.
  ;;
  ;;  We need to gradually build up this web of objects, using placeholders
  ;;  to represent objects which haven't been created yet.
  ;;
  ;;  Note: If this code hangs, it is almost certainly because somebody
  ;;  tried to call .METHOD-MISSING before it exists.

  ;; We use this macro to mark placeholders.  Whenever we use a placeholder,
  ;; the intended final value should appear to the right.
  (define-syntax PH
    (syntax-rules ()
      [(_)       #f]
      [(_ place) place]))

  ;; %CLASS%, STEP 1: Build %class%.
  (define %class%
    (new-ruby-class
      :name '%class%
      :klass (PH)              ; metaclass-for-%class%
      :inititialized? #t
      :superclass (PH)))       ; %object%

  ;; %CLASS%, STEP 2: Build a metaclass for %class%, and install it.
  (define metaclass-for-%class%
    (new-ruby-class
      :name 'metaclass:%class%
      :klass %class%
      :inititialized? #t
      :superclass (PH)))       ; Metaclass for %object%: (%object% .class)
  (set! (PH (ruby-object-klass %class%)) metaclass-for-%class%)

  ;; %OBJECT%: Create %object% normally.  DEFINE-CLASS does quite a bit of
  ;; weaving on its own, so go take a look.
  (define-class %object% (#f))

  ;; %CLASS%, STEP 3: Finish setting up %class%.  Note that because we
  ;; reshuffle the class hierarchy, we need to dump all our method caches.
  (set! (PH (ruby-class-superclass %class%)) %object%)
  (set! (PH (ruby-class-superclass metaclass-for-%class%))
        (ruby-object-klass %object%))
  (inc! *generation-id*)

  ;; %CLASS%, STEP 4: Make sure DEF will work properly (we need it below).
  ;; (There's no point in trying to install .DEFINE-METHOD before all
  ;; metaclasses are subclasses of %class%, because the metaclasses need to
  ;; be able to find it using method dispatch.)
  (add-method! %class% 'define-method
               (method~ (name impl)
                 (add-method! self name impl)))


  ;;=======================================================================
  ;;  Standard Methods
  ;;=======================================================================

  (provide check-for-initialization check-setter-writability
           check-setter-type safe-to-string)

  ;;; Check that OBJ is properly initialized.  METHOD-NAME should be the
  ;;; name of our caller.
  (define (check-for-initialization obj method-name)
    (unless (app~ obj .initialized?)
      (let [[msg (cat "Called " method-name " on uninitialized object")]]
        ;; If we're in an inifinitely-recursive error loop, NON-FATAL-ERROR
        ;; will at least make sure we see an error dialog.
        ;; (non-fatal-error msg)
        (error msg))))
 
  ;;; Check that method-name is not defined by klass.
  (define (check-method-not-defined klass method-name)
    (when (app~ klass .instances-respond-to? method-name)
      (error (cat "Tried to define attr ." method-name " on " klass
                  ", but it already exists"))))

  ;;; The standard writability check for ATTR setters.
  (define (check-setter-writability obj name writable?)
    (when (and (not writable?) (app~ obj .initialized?))
      (error (cat "Read-only attr: " name " on " obj))))

  ;;; The standard type check for ATTR setters.
  (define (check-setter-type obj name type val)
    (when (and type (not (instance-of?~ val type)))
      (error (cat "Attr " name " of " obj " has type " type 
                  ", tried to assign " val))))
   
  (with-instance %object%
    ;;; The class of this object.
    (def (class)
      (ruby-object-class self))
    ;;; Is this object a direct or indirect instance of KLASS?
    (def (instance-of? klass)
      (send (ruby-object-class self) 'subclass-of? klass))
    ;;; Is this object fully initialized yet?
    (def (initialized?)
      (ruby-object-initialized? self))
    ;;; This method is called whenever a non-existant method is sent to
    ;;; this object.  NAME is the name of the non-existant method, and ARGS
    ;;; are the arguments.
    (def (method-missing name . args)
      ;; METHOD-MISSING must exist for message dispatch to work.
      (error (cat "Method " name " does not exist on " self)))
    ;;; Does this class respond the method NAME?  If you override
    ;;; method-missing, you probably also want to override this (or
    ;;; INSTANCES-RESPOND-TO?).
    (def (responds-to? name)
      (app~ (app~ .class) .instances-respond-to? name))
    ;;; Return a string that should be used as the print representation of
    ;;; this object.
    (def (to-string)
      (check-for-initialization self 'to-string)
      (cat "#<" (ruby-class-name (app~ .class)) ">"))
    ;;; Initialize a newly-created instance.
    (def (initialize &rest keys)
      ;; A hash table of mandatory keywords.
      (define mandatory (make-hash-table))
      ;; Convert our keyword arguments to a hash table.  We store the
      ;; keywords in a hash table until we've walked all the way up the
      ;; inheritence hierarchy and collected the full set.
      (define key-table (keys->hash-table keys))
      ;; Create a proxy object that makes our hash table look like a real
      ;; object, so that initializer code can refer to attributes as
      ;; ".foo".
      (define key-obj
        (app~ %initializer-keywords% .new (app~ .class) key-table))
      ;; Walk up the class hierarchy, dealing with keywords as we go.
      (let loop [[klass (ruby-object-class self)]]
        (when klass
          (foreach [name (app~ klass .mandatory-attrs)]
            (hash-table-put! mandatory name #t))
          (foreach [init (app~ klass .attr-initializers)]
            (define name (initializer-name init))
            (cond
             [(not (hash-table-has-key? key-table name))
              ;; TODO: How should we handle it when bogus keywords aren't
              ;; found?
              (with-handlers [[exn:fail:keyword-not-found?
                               (lambda (exn) 
                                 (unless (initializer-skippable? init)
                                   (raise exn)))]]
                (define init-method (initializer-method init))
                (unless (function? init-method)
                  (error (cat "Initializing " self " ." name
                              ": :default requires a method as an argument, "
                              "got " init-method)))
                (hash-table-put! key-table name
                                 (instance-exec key-obj init-method)))]
             [(initializer-ignorable? init)
              #f]
             [else
              (error (cat "Can't pass :" name " to .new: "
                          "Already specified by " klass))]))
          (loop (app~ klass .superclass))))
      ;; Make sure that all mandatory arguments were specified.
      (hash-table-for-each mandatory
        (lambda (key value)
          (unless (hash-table-has-key? key-table key)
            (error (cat ":" key " must be specified to initialize instance of "
                        (app~ self .class))))))
      ;; Now that we have a full set of keyword arguments, apply them all
      ;; to our object at once.
      (hash-table-for-each key-table
        (lambda (key value)
          (send self (symcat "set-" key "!") value)))
      (void))

    (with-instance (app~ .class)
      ;;; Create a getter.  This behavior is used for ordinary objects, and
      ;;; it overrides the more general (and ickier) behavior needed for
      ;;; getters on classes.
      (def (attr-getter name &key default)
        ;; On ordinary objects, default values are never supplied by getter
        ;; methods, only by the initialization protocol.
        (app~ .define-method name 
              (method~ () (slot name))))
      )
    )

  (define *safe-to-string-stack* '())

  ;;; We frequently call .to-string from error-handling code.  So if
  ;;; .to-string fails, there's a real danger of infinitely recursive
  ;;; errors.  This, in turn, leads to stack overflow.  The SAFE-TO-STRING
  ;;; function attempts to avoid all known error cases.  See case 963 for
  ;;; an example of why we need this.
  (define (safe-to-string obj)
    (define (class-name)
      ;; Brian is feeling very paranoid here.
      (if (and (ruby-object? obj)
               (ruby-class? (ruby-object-klass obj)))
        (ruby-class-name (ruby-object-klass obj))
        "not-a-well-formed-ruby-object???"))
    (with-handlers [[exn:fail?
                     (lambda (exn)
                       (cat "#<" (class-name) " (to-string error: "
                            (exn-message exn) ")>"))]]
      (cond
       ;; If .to-string fails, it often results in somebody trying to call
       ;; .to-string *again* in order to print an error message.  That can
       ;; cause infinite loops that we can't catch with WITH-HANDLERS
       ;; above.  See case 2517.  This also protects against attempts to
       ;; print circular data structures.
       [(memq obj *safe-to-string-stack*)
        (cat "#<" (class-name) " (recursive to-string)>")]
       [(not (app~ obj .initialized?))
        (cat "#<" (class-name) " uninitialized>")]
       [else
        (fluid-let [[*safe-to-string-stack*
                     (cons obj *safe-to-string-stack*)]]
          (app~ obj .to-string))])))

  ;;; Now that our printing machinery is all set up, it should be safe to
  ;;; let Scheme know about it.  If we install this any earlier, we won't
  ;;; be able to format errors without crashing.
  (define (real-print-ruby-object obj port write?)
    (write-string (safe-to-string obj) port))
  (set! *ruby-object-printer* real-print-ruby-object)

  (with-instance %class%
    ;;; Return the superclass of this class.
    (def (superclass)
      (ruby-class-superclass self))
    ;;; Is this class a subclass of OTHER?
    (def (subclass-of? other)
      (cond
       [(eq? self other) #t]
       [(eq? self %object%) #f]
       [#t
        (app~ (app~ .superclass) .subclass-of? other)]))
    ;;; Define a class method for this class.  (This is actually an
    ;;; instance method on this class's metaclass.)
    (def (define-class-method name meth)
      (app~ (app~ .class) .define-method name meth))
    ;;; Do instances of this class respond to the given method name?
    (def (instances-respond-to? name)
      (instances-respond-to?% self name))
    ;;; Attach a snippet of code to an existing function.  See the
    ;;; ADVISE macro for documentation.
    (def (advise-method combination name meth)
      (advise-method! self combination name meth))
    ;;; Create a new attribute on this class.
    (def (attr name &key default (writable? #f) (mandatory? #t) (type #f)
               (getter? #t) (setter? #t))
      (when default
        (app~ .attr-initializer name default #t))
      (when mandatory?
        (app~ .mandatory-attr name))
      (when getter?
        (check-method-not-defined self name)
        (app~ .attr-getter name :default default)
        (app~ .seal-method! name))
      (when setter?
        (let [[setter-name (symcat "set-" name "!")]]
          (check-method-not-defined self setter-name)
          (app~ .attr-setter name :writable? writable? :type type)
          (app~ .seal-method! setter-name))))
    ;;; Create just the getter for an attribute, without setting up the
    ;;; normal initialization protocol or anything else (except for the
    ;;; hackish default magic needed in a few special cases).
    (def (attr-getter name &key default)
      ;; Hackish support for attribute defaults on already-initialized
      ;; objects (generally instances of %class%).  An example of why
      ;; this is necessary:
      ;;
      ;;   (define-class %foo%)
      ;;     (with-instance (.class) (attr bar (method () 2)))
      ;;     (.bar))
      ;;
      ;; Here, we want to add an attribute to %foo%'s metaclass, and use
      ;; it right away.  But %foo% has already been initialized, so our
      ;; attribute default is normally ignored.  Our fix: Define a new
      ;; getter method that handles the defaulting when needed.  A better
      ;; fix would be to allow our initialization protocol to run
      ;; incrementally.
      ;;
      ;; This getter method is only used for attributes on classes.  For
      ;; attributes on regular objects, this will be created by the
      ;; ATTR-GETTER method on (%OBJECT% .CLASS), which overrides this
      ;; method.  We use this method to make class ATTRs default sensibly,
      ;; even if they were added after initialization.
      (app~ .define-method name
            (method~ ()
              (when (and (not (has-slot%? self name)) default)
                (set! (slot name) (instance-exec self default)))
              (slot name))))
    ;;; Create just the setter for an attribute.
    (def (attr-setter name &key (writable? #f) (type #f))
      (app~ .define-method (symcat "set-" name "!")
            (method~ (val)
              (check-setter-writability self name writable?)
              (check-setter-type self name type val)
              (set! (slot name) val))))
    ;;; Attribute initializers for this class.
    (def (attr-initializers)
      ;; Implemented as a method, so we don't have to worry about making it
      ;; default correctly for metaclasses, %object%, %class%, etc.  This
      ;; avoids doing more bootstrapping work than necessary.
      (slot-or-default% self 'attr-initializers '()))
    ;;; Specify the initializer to use for an attribute.  If ignorable? is
    ;;; true, treat as an overridable default value.
    (def (attr-initializer name value ignorable? 
                           &key (skip-if-missing-values? #f))
      (set! (slot 'attr-initializers)
            (append (app~ .attr-initializers) ; O(N^2)
                    (list (make-initializer name value ignorable? 
                                            skip-if-missing-values?))))
      (void))
    ;;; List of attributes that must be specified to initialize object.
    (def (mandatory-attrs)
      (slot-or-default% self 'mandatory-attrs '()))
    ;;; Specify that attribute NAME must be supplied a value during
    ;;; initialization.
    (def (mandatory-attr name)
      (set! (slot 'mandatory-attrs) (cons name (app~ .mandatory-attrs))))

    ;;; Seal a method, and prevent it from being accidentally overridden by
    ;;; subclasses.  To reverse, call UNSEAL-METHOD!.
    (def (seal-method! name)
      (set! (method-sealed? self name) #t))

    ;;; Unseal a method, allowing to be overridden by the current class and
    ;;; any subclasses.  To reverse, call SEAL-METHOD!.
    (def (unseal-method! name)
      (set! (method-sealed? self name) #f))

    ;;; A sealed method cannot be overridden.  Note that overriding this
    ;;; method doesn't affect the underlying tests performed by the object
    ;;; model itself--this method is just a public interface to an internal
    ;;; function.
    (def (method-sealed? name)
      (method-sealed? self name))

    ;;; Create a new object of this class.
    (def (new &rest args)
      (define obj (new-ruby-object self))
      (apply send obj 'initialize args)
      (assert (not (ruby-object-initialized? obj)))
      (set! (ruby-object-initialized? obj) #t)
      obj)
    (def (to-string)
      (check-for-initialization self 'to-string)
      (cat (ruby-class-name self)))

    ;;; And now, for our metaclass.
    (with-instance (ruby-object-class self)
      ;;; Raises an error, because classes cannot be created on the fly,
      ;;; because we would need to set up the whole metaclass structure.
      (def (new &rest keys)
        (error "Can't create a new class using .new")))
    )
  
  ;; Create a subtype of exn:fail to represent an attempt to access a
  ;; non-existant keyword.
  (define-struct (exn:fail:keyword-not-found exn:fail) (class name))

  ;; Report the absence of an expected initializer keyword.
  (define (keyword-not-found klass name)
    (raise (make-exn:fail:keyword-not-found
            (string->immutable-string
             (cat klass ": Initializer keyword :" name " not specified"))
            (current-continuation-marks)
            klass name)))

  ;; Internal: Used to implement method calls in attribute initializers of
  ;; the form: (value foo (.bar)).  This basically wraps a hash table
  ;; and makes it look like a read-only object.  We choose to store
  ;; partially-built key/value pairs in a hash table until we have computed
  ;; the entire set and we're ready to apply them to our object.
  ;;
  ;; NOTE: Requires special bootstrapping code in DEFINE-CLASS.  Once this
  ;; class is defined, you should be able to define any other classes you
  ;; want.
  (define-class %initializer-keywords% ()
    (def (initialize klass hash)
      (set! (slot 'class) klass)
      (set! (slot 'hash) hash))
    (def (method-missing name . args)
      (unless (null? args)
        (error (cat (slot 'class) ": ." name
                    " should have no arguments in initializer")))
      (hash-table-get (slot 'hash) name
                      (lambda () (keyword-not-found (slot 'class) name)))))
  
  
  ;;=======================================================================
  ;;  "Universal" Class Functions
  ;;=======================================================================
  ;; We currently have two different object models (RUBY and SWINDLE), but
  ;; both use the same syntax to refer to classes and instances. To help
  ;; avoid confusion, we have "universal" functions that can operate on
  ;; objects and classes from either system.
  
  (provide swindle-object? swindle-class? swindle-instance-of?
           swindle-subclass? object?~ class?~
           instance-of?~ subclass?~)
  
  ;;; Provide swindle-specific versions of the class functions.
  (define swindle-object? object?)
  (define swindle-class? class?)
  (define swindle-instance-of? instance-of?)
  (define swindle-subclass? subclass?)
  
  ;;; Is the given value a swindle or ruby object?
  (define (object?~ value)
    (or (ruby-object? value)
        (swindle-object? value)))
  
  ;;; Is the given value a swindle or ruby class?
  (define (class?~ value)
    (or (ruby-class? value)
        (swindle-class? value)))
  
  ;;; Given an object and a class, determine if the object is an instance
  ;;; of the given class.
  (define (instance-of?~ obj class)
    (cond
     [(and (ruby-object? obj)
           (ruby-class? class))
      (app~ obj .instance-of? class)]
     [else
      (instance-of? obj class)]))
  
  
  ;;; Given two classes, determine if one is a subclass of the other.
  (define (subclass?~ c1 c2)
    (cond
     [(and (ruby-class? c1) (ruby-class? c2))
      (app~ c1 .subclass-of? c2)]
     [(and (swindle-class? c1) (swindle-class? c2))
      (swindle-subclass? c1 c2)]
     [else
      #f]))
  
  
  ;;=======================================================================
  ;;  Utility Functions
  ;;=======================================================================

  ;;; Convert a keyword argument list to a hash table, doing some
  ;;; error-checking along the way.
  (define (keys->hash-table keys)
    (define (malformed)
      (error (cat "Malformed keyword argument list: " keys)))
    (define result (make-hash-table))
    (let recurse [[ks keys]]
      (cond
       [(null? ks) (void)]
       [(null? (cdr ks))
        (malformed)]
       [#t
        (unless (keyword? (car ks))
          (malformed))
        (hash-table-put! result (keyword-name (car ks)) (cadr ks))
        (recurse (cddr ks))]))
    result)


  ;;=======================================================================
  ;;  Method Dispatch & Slot Syntax 
  ;;=======================================================================
  ;;  Here there be dragons.
  ;;
  ;;  We support a method dispatch syntax (very loosely) based on Ruby,
  ;;  Python, C++, etc.  Of course, this is horrendously non-portable to
  ;;  any Scheme but PLT.  Examples:
  ;;
  ;;    (foo .layout :desired-width 50)
  ;;    (set! (@title .text) "Hello!")
  ;;
  ;;  When there is a SELF variable in scope, you can also write:
  ;;
  ;;    (.layout :desired-width 50)
  ;;    (set! (.text) "Hello!")
  ;;
  ;;  Note that this code may interact with various Swindle internals in
  ;;  unpleasant ways.  Swindle's SETF!, in particular, contains code to
  ;;  deal with #%APP, and may start complaining about SET-#%APP! if it
  ;;  doesn't recognize our version of #%APP as such.  So if you have
  ;;  problems, make sure that SETF! is doing the right thing.
  ;;
  ;;  We also provide a (SLOT 'foo) macro which accesses a private storage
  ;;  slot named FOO associated with SELF.  This only works if SELF is in
  ;;  scope--by not exporting SLOT%, etc., which work for any object, we
  ;;  make object slots semi-private.

  (provide app~ has-slot? slot set!~)

  ;; These definitions are visible from within DEFINE-SYNTAX forms.
  (begin-for-syntax

    ;; Returns #t iff NAME-STX is a symbol beginning with ".".
    (define (dotted-name? name-stx)
      (let [[name-sym (syntax-object->datum name-stx)]]
        (and (symbol? name-sym)
             (let [[name (symbol->string name-sym)]]
               (and (> (string-length name) 0)
                    (eq? (string-ref name 0) #\.))))))
    
    ;; Remove a leading "." from a string.
    (define (undot-string name)
      (regexp-replace #rx"^\\." name ""))

    ;; Convert a syntax object to a string, apply F to it, and convert the
    ;; result back.
    (define (munge-name f name-stx)
      (let* [[name (symbol->string (syntax-object->datum name-stx))]
             [munged (f name)]]
        (datum->syntax-object name-stx (string->symbol munged))))

    ;; ".foo" -> "foo"
    (define (getter-name name-stx)
      (munge-name undot-string name-stx))

    ;; ".foo" -> "set-foo!"
    (define (setter-name name-stx)
      (munge-name (lambda (name)
                    (string-append "set-" (undot-string name) "!"))
                  name-stx))
    )

  (define-syntax (app~ stx)
    (syntax-case stx ()
      ;; Method dispatch with implicit SELF.
      [(_ method . args)
       (dotted-name? #'method)
       (quasisyntax/loc
        stx
        (send #,(make-self stx) '#,(getter-name #'method) . args))]
      ;; Method dispatch with explicit SELF.
      [(_ object method . args)
       (dotted-name? #'method)
       (quasisyntax/loc stx (send object '#,(getter-name #'method) . args))]
      ;; Regular function call.
      [(_ function . args)
       (syntax/loc stx (function . args))]))

  ;; Getter macro for HAS-SLOT?.
  (define-syntax (has-slot? stx)
    (syntax-case stx ()
      [(_ name)
       (quasisyntax/loc
        stx
        (has-slot%? self name))]))

  ;; Getter macro for SLOT.  See also SET!~, which special-cases the
  ;; setter version.
  (define-syntax (slot stx)
    (syntax-case stx ()
      [(_ name)
       (quasisyntax/loc
        stx
        (slot% self name))]))

  ;; Because of the aforementioned #%app expansion problems (and related
  ;; issues), we also override Swindle's SET! and handle a few cases
  ;; manually.  This is really more a workaround than anything else.
  (define-syntax (set!~ stx)
    (syntax-case stx (slot)
      ;; (set! (.name) "Foonly")
      [(_ (method args ...) value)
       (dotted-name? #'method)
       (quasisyntax/loc
        stx
        (send #,(make-self #'method) '#,(setter-name #'method)
               args ... value))]
      ;; (set! (foo .name) "Foonly")
      [(_ (object method args ...) value)
       (dotted-name? #'method)
       (quasisyntax/loc
        stx
        (send object '#,(setter-name #'method) args ... value))]
      ;; (set! (slot 'name) "Foonly")
      ;; (If we had defined the SLOT macro in a module that was using our
      ;; custom #%APP, we wouldn't need to define a specific setter special
      ;; implementation for this macro.)
      [(_ (slot name) value)
       (quasisyntax/loc stx (set-slot%! self name value))]
      [(_ args ...)
       (syntax/loc stx (set! args ...))]))

  )
