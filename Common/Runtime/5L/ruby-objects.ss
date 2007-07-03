(module ruby-objects (lib "swindle.ss" "swindle")
  (require-for-syntax (lib "capture.ss" "5L"))
  (require (lib "util.ss" "5L"))
  (require (lib "begin-var.ss" "5L"))

  (provide <ruby-object> ruby-object? make-ruby-instance-of?-predicate
           define-class %class% %object% method~ def
           instance-exec with-instance attr attr-value attr-default)

  (defclass <ruby-object> ()
    klass
    [slots :initializer (lambda () (make-hash-table))]
    ;; Has the initialization protocol (if any) for this object been
    ;; finished?
    [initialized? :initvalue #f]
    :auto #t :printer #f)

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
        (error (cat "Expected an object, got: " obj))))

  (defclass <ruby-class> (<ruby-object>)
    name
    superclass
    [methods :initializer (lambda () (make-hash-table))]
    :auto #t :printer #f
    )

  (defclass <initializer> ()
    name method ignorable? skippable?
    :auto #t :printer #t)

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

  (define (add-method! object name method)
    (hash-table-put! (ruby-class-methods object) name method))

  (define-syntax define-class
    (syntax-rules ()
      [(_ klass () . body)
       (define-class klass (%object%) . body)]
      [(_ klass (super) . body)
       (begin
         ;; We want to be able to define "class" methods on KLASS.  But
         ;; since we only support instance methods, we need to make KLASS an
         ;; instance of a "metaclass".  Metaclasses are themselves classes.
         (define metaclass
           (make <ruby-class>
             :name (symcat "metaclass:" 'klass)
             :klass %class%
             ;; The initialization protocol doesn't apply to metaclasses.
             :inititialized? #t
             ;; The metaclass of %object% inherits from %class%.  Other
             ;; metaclasses inherit from SUPER's metaclass, so that class
             ;; methods are visible on a class and all its subclasses.  For
             ;; a full picture of what's going on here, see
             ;; ruby-objects.png.
             :superclass (if super 
                           (ruby-object-class super)
                           %class%)))
         (define klass
           (make <ruby-class>
             :name 'klass
             :klass metaclass
             :superclass super))
         ;; Call our initialize method to make sure any attribute
         ;; initializers get run.  This allows classes to have slots,
         ;; aiding metaprogrammers.  (%object% and %initializer-keywords%
         ;; are declared before we can safely call initialize, so we skip
         ;; this step for them.)
         (unless (or (eq? 'klass '%object%)
                     (eq? 'klass '%initializer-keywords%))
           (send% klass 'initialize))
         (set! (ruby-object-initialized? klass) #t)
         (with-instance klass . body)
         (void))]))

  (define-syntax (method~ stx)
    (syntax-case stx ()
      [(_ args . body)
       ;; Bind SELF and SUPER using the context of BODY.
       (with-syntax [[self (make-capture-var/ellipsis #'body 'self)]
                     [super (make-capture-var/ellipsis #'body 'super)]]
         (syntax/loc stx (lambda (self super . args) (begin/var . body))))]))

  (define (send% object method . args)
    (define (send-to-class klass)
      (with-values
          [[found-klass implementation]
           (let recurse [[klass klass]]
             (if (not klass)
                 ;; Case 1: Searched everywhere, no method.
                 (values #f (method~ args
                              (apply send% self 'method-missing method args)))
                 (let [[found (hash-table-get (ruby-class-methods klass) method
                                              (lambda () #f))]]
                   (if found
                       ;; Case 2: Found it.
                       (values klass found)
                       ;; Case 3: Recurse and try our superclass.
                       (recurse (ruby-class-superclass klass))))))]
        (apply implementation object 
               (lambda () (send-to-class (ruby-class-superclass found-klass)))
               args)))
    (send-to-class (ruby-object-class object)))

  (define (instance-exec object method . args)
    (apply method object 
           (lambda () (error "Cannot call super using instance-exec"))
           args))

  (define-syntax with-instance
    (syntax-rules ()
      [(_ object . body)
       (instance-exec object (method~ () . body))]))

  ;; TODO - Rename DEF -> ON after overhauling nodes.ss?
  (define-syntax (def stx)
    (syntax-case stx ()
      [(_ (name . args) . body)
       (quasisyntax/loc
        stx
        (app~ #,(make-self #'name) .define-method 'name
              (method~ args . body)))]))

  (define-syntax (attr stx)
    (syntax-case stx ()
      [(_ name default . args)
       (not (keyword? (syntax-object->datum #'default)))
       (syntax/loc stx (attr name :default (method~ () default) . args))]
      [(_ name . args) 
       (quasisyntax/loc
        stx
        (app~ #,(make-self #'name) .attr 'name . args))]))

  (define-syntax (attr-initializer stx)
    (syntax-case stx ()
      [(_ name value ignorable? arg ...)
       (quasisyntax/loc
        stx
        (app~ #,(make-self #'name) .attr-initializer 'name
              (method~ () value) ignorable? arg ...))]))

  ;;; Specify the value to use for an attribute.
  (define-syntax attr-value
    (syntax-rules ()
      [(_ name value arg ...)
       (attr-initializer name value #f arg ...)]))

  ;;; Specify the default to use for an attribute.
  (define-syntax attr-default
    (syntax-rules ()
      [(_ name value arg ...)
       (attr-initializer name value #t arg ...)]))


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
    (make <ruby-class>
      :name '%class%
      :klass (PH)              ; metaclass-for-%class%
      :inititialized? #t
      :superclass (PH)))       ; %object%

  ;; %CLASS%, STEP 2: Build a metaclass for %class%, and install it.
  (define metaclass-for-%class%
    (make <ruby-class>
      :name 'metaclass:%class%
      :klass %class%
      :inititialized? #t
      :superclass (PH)))       ; Metaclass for %object%: (%object% .class)
  (set! (PH (ruby-object-klass %class%)) metaclass-for-%class%)

  ;; %OBJECT%: Create %object% normally.  DEFINE-CLASS does quite a bit of
  ;; weaving on its own, so go take a look.
  (define-class %object% (#f))

  ;; %CLASS%, STEP 3: Finish setting up %class%.
  (set! (PH (ruby-class-superclass %class%)) %object%)
  (set! (PH (ruby-class-superclass metaclass-for-%class%))
        (ruby-object-klass %object%))

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

  (with-instance %object%
    ;;; The class of this object.
    (def (class)
      (ruby-object-class self))
    ;;; Is this object a direct or indirect instance of KLASS?
    (def (instance-of? klass)
      (send% (ruby-object-class self) 'subclass-of? klass))
    ;;; Is this object fully initialized yet?
    (def (initialized?)
      (ruby-object-initialized? self))
    ;;; Call method NAME on an object, passing in ARGS.
    (def (send name args)
      (apply send% self name args))
    ;;; This method is called whenever a non-existant method is sent to
    ;;; this object.  NAME is the name of the non-existant method, and ARGS
    ;;; are the arguments.
    (def (method-missing name . args)
      ;; METHOD-MISSING must exist for message dispatch to work.
      (error (cat "Method " name " does not exist on " self)))
    ;;; Return a string that should be used as the print representation of
    ;;; this object.
    (def (to-string)
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
      (define key-obj (app~ %initializer-keywords% .new key-table))
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
                (hash-table-put! key-table name
                                 (instance-exec key-obj
                                                (initializer-method init))))]
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
          (send% self (symcat "set-" key "!") value)))
      (void))
    )

  ;;; Now that our printing machinery is all set up, it should be safe to
  ;;; let Swindle know about it.  If we install this any earlier, we won't
  ;;; be able to format errors without crashing.
  (defmethod (print-object [object <ruby-object>] esc? port)
    (display (app~ object .to-string) port))

  ;; XXX - We need to duplicate this for each swindle class we override
  ;; PRINT-OBJECT on.  I'm not quite sure why--should setting :printer #f
  ;; be enough to make this go away?
  (defmethod (print-object [object <ruby-class>] esc? port)
    (display (app~ object .to-string) port))

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
    ;;; Create a new attribute on this class.
    (def (attr name &key default (writable? #f) (mandatory? #t) (type #f))
      (when default
        (app~ .attr-initializer name default #t))
      (when mandatory?
        (app~ .mandatory-attr name))
      (app~ .define-method name 
            (method~ () (slot name)))
      (app~ .define-method (symcat "set-" name "!")
            (method~ (val)
              (cond
               [(and (not writable?) (app~ .initialized?))
                (error (cat "Read-only attr: " name))]
               [(and type (not (instance-of? val type)))
                (error (cat "Attr " name " has type " type ", tried to assign "
                            val))]
               [#t
                (set! (slot name) val)]))))
    ;;; Hackish support for attribute defaults on already-initialized
    ;;; objects (generally instances of %class%).  An example of why this
    ;;; is necessary:
    ;;;
    ;;;   (define-class %foo%)
    ;;;     (with-instance (.class) (.auto-default-attr bar (method () 2)))
    ;;;     (.bar))
    ;;;
    ;;; Here, we want to add an attribute to %foo%'s metaclass, and use
    ;;; it right away.  But %foo% has already been initialized, so our
    ;;; attribute default is normally ignored.  Our fix: Define a new
    ;;; getter method that handles the defaulting when needed.  A better
    ;;; fix would be to allow our initialization protocol to run
    ;;; incrementally.
    (def (auto-default-attr name default 
                            &key (writable? #f) (mandatory? #t))
      ;; Set up our regular attr stuff.
      (app~ .attr name
            :default default :writable? writable? :mandatory? mandatory?)
      ;; Clobber previous method with this name.
      (app~ .define-method name
            (method~ ()
              (unless (has-slot%? self name)
                (set! (slot% self name) (instance-exec self default)))
              (slot% self name))))
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
    ;;; Create a new object of this class.
    (def (new &rest args)
      (define obj (make <ruby-object> :klass self))
      (apply send% obj 'initialize args)
      (assert (not (ruby-object-initialized? obj)))
      (set! (ruby-object-initialized? obj) #t)
      obj)
    (def (to-string)
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
  (define-struct (exn:fail:keyword-not-found exn:fail) (name))

  ;; Report the absence of an expected initializer keyword.
  (define (keyword-not-found name)
    (raise (make-exn:fail:keyword-not-found
            (string->immutable-string
             (cat "Initializer keyword :" name " not specified"))
            (current-continuation-marks)
            name)))

  ;; Internal: Used to implement method calls in attribute initializers of
  ;; the form: (attr-value foo (.bar)).  This basically wraps a hash table
  ;; and makes it look like a read-only object.  We choose to store
  ;; partially-built key/value pairs in a hash table until we have computed
  ;; the entire set and we're ready to apply them to our object.
  ;;
  ;; NOTE: Requires special bootstrapping code in DEFINE-CLASS.  Once this
  ;; class is defined, you should be able to define any other classes you
  ;; want.
  (define-class %initializer-keywords% ()
    (def (initialize hash)
      (set! (slot 'hash) hash))
    (def (method-missing name . args)
      (unless (null? args)
        (error (cat "." name " should have no arguments in initializer")))
      (hash-table-get (slot 'hash) name
                      (lambda () (keyword-not-found name)))))


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

  (provide app~ slot set!~)

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
        (send% #,(make-self #'method) '#,(getter-name #'method) . args))]
      ;; Method dispatch with explicit SELF.
      [(_ object method . args)
       (dotted-name? #'method)
       (quasisyntax/loc stx (send% object '#,(getter-name #'method) . args))]
      ;; Regular function call.
      [(_ function . args)
       (syntax/loc stx (function . args))]))

  ;; Getter macro for SLOT.  See also HACKED-SET!, which special-cases the
  ;; setter version.
  (define-syntax (slot stx)
    (syntax-case stx ()
      [(_ name)
       (quasisyntax/loc
        stx
        (slot% #,(make-self #'name) name))]))

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
        (send% #,(make-self #'method) '#,(setter-name #'method)
               args ... value))]
      ;; (set! (foo .name) "Foonly")
      [(_ (object method args ...) value)
       (dotted-name? #'method)
       (quasisyntax/loc
        stx
        (send% object '#,(setter-name #'method) args ... value))]
      ;; (set! (slot 'name) "Foonly")
      ;; (If we had defined the SLOT macro in a module that was using our
      ;; custom #%APP, we wouldn't need to define a specific setter special
      ;; implementation for this macro.)
      [(_ (slot name) value)
       (quasisyntax/loc stx (set-slot%! #,(make-self #'name) name value))]
      [(_ args ...)
       (syntax/loc stx (set! args ...))]))

  )
