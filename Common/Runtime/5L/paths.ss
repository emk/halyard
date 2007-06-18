(module paths (lib "language.ss" "5L")
  (require (lib "util.ss" "5L"))

  ;; Get some string processing stuff from the SRFI libraries.
  (require (only (lib "13.ss" "srfi") string-tokenize string-join))
  (require-for-syntax (only (lib "13.ss" "srfi") string-tokenize string-join))
  (require (only (lib "14.ss" "srfi") char-set char-set-complement))
  (require-for-syntax (only (lib "14.ss" "srfi") char-set char-set-complement))

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

  )
