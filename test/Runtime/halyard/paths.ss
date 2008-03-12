(module paths (lib "language.ss" "halyard")
  (require (lib "util.ss" "halyard"))
  (require-for-syntax (lib "util.ss" "halyard"))

  (require (lib "nodes.ss" "halyard"))
  (require (lib "indent.ss" "halyard"))

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
  (provide %node-path% node-path? @* @ resolve)

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

    ;;; Redirect any method calls we don't understand to our associated node.
    (def (method-missing name . args)
      (apply send (.resolve-path) name args))

    ;; XXX - Nasty hack since we haven't decided what to do about interfaces
    ;; yet.  This will need more thought.
    ;; TODO - Decide if we want a general .implements-interface? method.
    (def (instance-of? klass)
      (or (super)
          ((.resolve-path) .instance-of? klass)))

    ;;; Get the full name of the running node corresponding to this path.
    ;;; If you are interested in the static node, you'll need to resolve it
    ;;; first.
    (def (full-name)
      (define (not-found-fn)
        (error (cat "Cannot find " self "; "
                    "If referring to a static node, please resolve it first.")))
      ((.resolve-path :running? #t :if-not-found not-found-fn) .full-name))

    (def (jump)
      ;; When we jump, we want to resolve things to point to the static
      ;; node.  Yeah, another ugly special case...
      ((.resolve-path :running? #f) .jump))

    ;;; Resolve a path.
    (def (resolve-path
          &key (running? #t)
               (if-not-found
                (lambda ()
                  (error (cat "Can't find relative path: " self)))))
      (unless (current-group-member)
        (error (cat "Can't find relative path '@" (.to-symbol)
                    "' outside of a card")))
      (or (find-node-relative (if running?
                                  (current-group-member)
                                  ((current-group-member) .static-node))
                              (.to-symbol) running?)
          (if-not-found)))

    ;;; Note that (delete-element @foo) will pass a .%delete message to
    ;;; this %node-path%, which we must forward appropriately.
    )

  ;;; Is OBJ a node path?
  (define node-path? (make-ruby-instance-of?-predicate %node-path%))

  ;; Treat 'name' as a relative path.  If 'name' can be found relative to
  ;; 'base', return it.  If not, try the parent of base if it exists.  If
  ;; all fails, return #f.
  (define (find-node-relative base name running?)
    (if base
      (or (find-child-node base name running?)
          (find-node-relative (node-parent base) name running?))
      #f))

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

  (define-syntax @
    ;; Syntactic sugar for creating a path.
    (syntax-rules ()
      [(@ name)
       ;; TODO - Make our caller pass us a string instead.
       (@* (symbol->string 'name))]))
  (define-syntax-indent @ function)

  ;;; Given either a %node-path% or a node, return a node.
  (define (resolve path-or-node &key (running? #t))
    (if (node-path? path-or-node)
        (path-or-node .resolve-path :running? running?)
        path-or-node))
  
  )
