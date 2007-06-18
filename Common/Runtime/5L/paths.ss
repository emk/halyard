(module paths (lib "language.ss" "5L")
  (require (lib "util.ss" "5L"))
  (require-for-syntax (lib "util.ss" "5L"))

  ;; Get some string processing stuff from the SRFI libraries.
  (require (only (lib "13.ss" "srfi") string-tokenize string-join))
  (require-for-syntax (only (lib "13.ss" "srfi") string-tokenize string-join))
  (require (only (lib "14.ss" "srfi") char-set char-set-complement))
  (require-for-syntax (only (lib "14.ss" "srfi") char-set char-set-complement))


  ;;=======================================================================
  ;;  Node Paths
  ;;=======================================================================

  ;; Our public API.  We temporarily rename the path constructor function
  ;; to prevent a collision with nodes.ss.
  (provide %node-path% (rename @* make-node-path))

  ;;; A path in our node hierarchy.  Can represent either nested node
  ;;; classes, or nested nodes themselves.
  (define-class %node-path% ()
    (attr components)

    ;;; Print a path in a readable format, but without the leading @ sign.
    ;;; The opposite of @*.
    (def (to-path-string)
      (components->path-string (.components)))

    ;;; Print a path in a readable format.
    (def (to-string)
      (string-append "@" (.to-path-string)))

    ;;; Build a symbol representing this path.  For legacy use.
    (def (to-symbol)
      (string->symbol (.to-path-string)))

    ;;; Resolve this path relative to BASE.
    #| XXX - Disabled because we're not ready to integrate this yet.
    (def (resolve base)
      (define (find-node-internal base components)
        (cond
         [(empty? components) base]
         [(eq? '|.| (car components))
          (find-node-internal base (cdr components))]
         [(eq? '|..| (car components))
          (find-node-internal (base .parent) (cdr components))]
         [else (find-node-internal (base .child (car components))
                                   (cdr components))]))
      (define components (slot 'components))
      (if (and (not (null? components))
               (eq? '|.| (car components)))
          (find-node-internal base (cdr components))
          (find-node-internal (base .root) components)))
    |#
    )

  ;;; Run code at both syntax expansion time and runtime.
  (define-syntax begin-for-syntax-and-runtime
    (syntax-rules ()
      [(_ code ...)
       (begin
         (begin-for-syntax code ...)
         code ...)]))

  (begin-for-syntax-and-runtime
    ;;; Break a path string appart into a list of components.
    (define (path-string->components path-string)
      (let* [[absolute? (and (> (string-length path-string) 0)
                             (eq? #\/ (string-ref path-string 0)))]
             [components
              (string-tokenize path-string
                               (char-set-complement (char-set #\/)))]]
        (map string->symbol
             (if absolute? components (cons "." components)))))

    ;;; Combine components back into a path string.
    (define (components->path-string components)
      (define (join lst)
        (string-join (map symbol->string lst) "/"))
      (if (and (not (null? components))
               (eq? '|.| (car components)))
          (join (cdr components))
          (string-append "/" (join components))))
    )

  ;; @* CHANGED (Now takes a string as an argument.  This used to return a
  ;; node; now it just returns a path in the node tree.  It now also
  ;; distinguishes between relative and absolute paths, and no longer
  ;; searches up the hierarchy.)
  (define (@* path-string)
    (%node-path% .new :components (path-string->components path-string)))


  ;;=======================================================================
  ;;  Short-Term Glue Layer
  ;;=======================================================================
  ;;  This is an ugly hack designed to help us migrate from the old object
  ;;  system to the new one.  Essentially, it wraps %node% and subclasses,
  ;;  making each of the usual accessor methods smart enough to resolve
  ;;  paths if that's what they get instead of real objects.  Ugly, ugly.

  (provide define-path-or-node-function define-node-class)

  ;;; Create a function NAME which takes either a node or a node path as
  ;;; an argument, resolves any node path to a node, and calls WRAPPED.
  (define-syntax define-path-or-node-function
    (syntax-rules ()
      [(_ name wrapped)
       (define (name path-or-node . args)
         ;; (with-handlers [[void (lambda (exn)
         ;;                      (debug-log (format-trace exn))
         ;;                      (non-fatal-error (exn-message exn)))]]
         ;;(non-fatal-error (cat "Called " name ": " path-or-node " " ar gs))
         (if (and (ruby-object? path-or-node)
                  (path-or-node .instance-of? %node-path%))
             ;; .resolve-path is provided in nodes.ss.
             (apply wrapped (path-or-node .resolve-path) args)
             (apply wrapped path-or-node args)))]))

  ;;; Create a wrapper for each function defined in the process of defining
  ;;; a class.  These wrappers all use DEFINE-PATH-OR-NODE-FUNCTION.
  (define-syntax (define-node-class stx)
    (define (strip-class-brackets class-name)
      (string->symbol
       ;; This line adapted from Swindle's clos.ss file.
       (regexp-replace #rx"^<(.*)>$" (symbol->string class-name) "\\1")))
    (define (build-id name-stx &key (setter? #f) class-name-stx (wrapped? #t))
      (datum->syntax-object stx
       (string->symbol
        (apply string-append
          (map symbol->string
               (append
                (if setter? '(set-) '())
                (if class-name-stx
                    (list (strip-class-brackets
                           (syntax-object->datum class-name-stx))
                          '-)
                    '())
                (list (syntax-object->datum name-stx))
                (if wrapped? (list '-wrapped) '())
                (if setter? '(!) '())))))))
    (define (renamed-slot slot-stx)
      (define (expand name args)
        (quasisyntax/loc slot-stx
         (#,(build-id name)
          :initarg #,(symcat '|:| (syntax-object->datum name)) #,@args)))
      (syntax-case slot-stx ()
        [(name . args) (expand #'name #'args)]
        [name (expand #'name '())]))
    (define (slot-wrapper slot-stx class-name-stx)
      (define (expand name)
        (quasisyntax/loc slot-stx
         (begin
           (define-path-or-node-function
             #,(build-id name :class-name-stx class-name-stx :wrapped? #f)
             #,(build-id name :class-name-stx class-name-stx))
           (define-path-or-node-function
             #,(build-id name :setter? #t :class-name-stx class-name-stx
                         :wrapped? #f)
             #,(build-id name :setter? #t :class-name-stx class-name-stx)))))
      (syntax-case slot-stx ()
        [(name . args) (expand #'name)]
        [name (expand #'name)]))
    (let [[result
           (syntax-case stx ()
             [(_ name supers . slots)
              (quasisyntax/loc
               stx
               (begin
                 (defclass name supers #,@(map renamed-slot
                                               (syntax-e #'slots)))
                 #,@(map (lambda (slot-stx) (slot-wrapper slot-stx #'name))
                         (syntax-e #'slots))))])]]
      ;;(non-fatal-error (cat (syntax-object->datum result)))
      result))

  )
