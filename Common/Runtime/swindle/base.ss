;;; Written by Eli Barzilay: Maze is Life!  (eli@barzilay.org)

;;> The `base' module defines some basic low-level syntactic extensions to
;;> MzScheme.  It can be used by itself to get these extensions.

(module base mzscheme

(provide (all-from-except mzscheme
          #%module-begin #%top #%app define let let* letrec lambda))

;;>> (#%module-begin ...)
;;>   `base' is a language module -- it redefines `#%module-begin' to load
;;>   itself for syntax definitions.
(provide (rename module-begin~ #%module-begin))
(define-syntax (module-begin~ stx)
  (let ((e (if (syntax? stx) (syntax-e stx) stx)))
    (if (pair? e)
      (datum->syntax-object
       (quote-syntax here)
       (list* (quote-syntax #%plain-module-begin)
              (datum->syntax-object stx
                                    (list (quote-syntax require-for-syntax)
                                          '(lib "base.ss" "swindle")))
              (cdr e))
       stx)
      (raise-syntax-error #f "bad syntax" stx)))
  ;; This doesn't work anymore (from 203.4)
  ;; (syntax-rules ()
  ;;   ((_ . body) (#%plain-module-begin
  ;;                (require-for-syntax (lib "base.ss" "swindle")) . body)))
  )

;;>> (#%top . id)
;;>   This special syntax is redefined to make keywords (symbols whose names
;;>   begin with a ":") evaluate to themselves.  Note that this does not
;;>   interfere with using such symbols for local bindings.
(provide (rename top~ #%top))
(define-syntax (top~ stx)
  (syntax-case stx ()
    ((_ . x)
     (let ((x (syntax-object->datum #'x)))
       (and (symbol? x) (not (eq? x '||))
            (eq? #\: (string-ref (symbol->string x) 0))))
     (syntax/loc stx (#%datum . x)))
    ((_ . x) (syntax/loc stx (#%top . x)))))

;;>> (#%app ...)
;;>   Redefined so it is possible to apply using dot notation: `(foo x . y)'
;;>   is the same as `(apply foo x y)'.  This is possible only when the last
;;>   (dotted) element is an identifier.
(provide (rename app~ #%app))
(define-syntax (app~ stx)
  (syntax-case stx ()
    ((_ x ...) (syntax/loc stx (#%app x ...)))
    ((_ . x)
     (let loop ((s (syntax-e #'x)) (r '()))
       (cond ((list? s) (syntax/loc stx (#%app . x)))
             ((pair? s) (loop (cdr s) (cons (car s) r)))
             (else (let ((e (and (syntax? s) (syntax-e s))))
                     (if (or (null? e) (pair? e))
                       (loop e r)
                       (quasisyntax/loc stx
                         (#%app apply . #,(reverse! (cons s r))))))))))))

;;>> (define id-or-list ...)
;;>   The standard `define' form is modified so instead of an identifier
;;>   name for a function, a list can be used -- resulting in a curried
;;>   function.
;;>     => (define (((plus x) y) z) (+ x y z))
;;>     => plus
;;>     #<procedure:plus>
;;>     => (plus 5)
;;>     #<procedure:plus:1>
;;>     => ((plus 5) 6)
;;>     #<procedure:plus:2>
;;>     => (((plus 5) 6) 7)
;;>     18
;;>   Note the names of intermediate functions.
;;>
;;>   In addition, the following form can be used to define multiple values:
;;>     => (define (values a b) (values 1 2))
(provide (rename define~ define))
(define-syntax (define~ stx)
  ;; simple version
  ;; (syntax-case stx ()
  ;;   ((_ (name arg ...) body ...)
  ;;    #`(define~ name (lambda~ (arg ...) body ...)))
  ;;   ((_ name body ...) #'(define name body ...)))
  ;; this version makes created closures have meaningful names
  (syntax-case stx (values)
    ((_ name expr) (identifier? #'name)
     #'(define-values (name) expr))
    ((_ (values name ...) expr)
     #'(define-values (name ...) expr))
    ((_ names body0 body ...) (pair? (syntax-e #'names))
     (let loop ((s #'names) (args '()))
       (syntax-case s ()
         ((name . arg) (loop #'name (cons #'arg args)))
         (name
          (let ((sym (syntax-object->datum #'name)))
            (let loop ((i    (sub1 (length args)))
                       (as   (reverse (cdr args)))
                       (body #'(begin body0 body ...)))
              (if (zero? i)
                (quasisyntax/loc stx
                  (define name (lambda~ #,(car args) #,body)))
                (loop (sub1 i) (cdr as)
                      (syntax-property
                       (quasisyntax/loc stx (lambda~ #,(car as) #,body))
                       'inferred-name
                       (string->symbol (format "~a:~a" sym i)))))))))))))

;;>> (let ((id-or-list ...) ...) ...)
;;>> (let* ((id-or-list ...) ...) ...)
;;>> (letrec ((id-or-list ...) ...) ...)
;;>   All standard forms of `let' are redefined so they can generate
;;>   functions using the same shortcut that `define' allows.  This includes
;;>   the above extension to the standard `define'.  For example:
;;>     => (let ((((f x) y) (+ x y))) ((f 1) 2))
;;>     3
;;>   It also includes the `values' keyword in a similar way to `define'.
;;>   For example:
;;>     => (let (((values i o) (make-pipe))) i)
;;>     #<pipe-input-port>
(provide (rename let~ let) (rename let*~ let*) (rename letrec~ letrec))
(define-syntaxes (let~ let*~ letrec~)
  (let* ((process
          (lambda (stx var0 val0 . flat?)
            (syntax-case var0 (values)
              ((values var ...) (null? flat?) #`((var ...) . #,val0))
              (_ (let loop ((var var0) (args '()))
                   (if (identifier? var)
                     (if (null? args)
                       (let ((val (syntax->list val0)))
                         (if (and (pair? val) (null? (cdr val)))
                           (list (if (null? flat?) (list var) var) (car val))
                           (raise-syntax-error
                            #f "bad binding" stx #`(#,var0 #,@val0))))
                       (let ((sym (syntax-e var)))
                         (let loop ((i   (sub1 (length args)))
                                    (as  (reverse args))
                                    (val val0))
                           (if (< i 0)
                             (list (if (null? flat?) (list var) var)
                                   (car (syntax->list val)))
                             (loop (sub1 i) (cdr as)
                                   (let ((val #`((lambda~ #,(car as) #,@val))))
                                     (if (zero? i)
                                       val
                                       (syntax-property
                                        val 'inferred-name
                                        (if (zero? i)
                                          sym
                                          (string->symbol
                                           (format "~a:~a" sym i)))))))))))
                (syntax-case var ()
                  ((var . args1) (loop #'var (cons #'args1 args))))))))))
         (mk-bindings
          (lambda (stx bindings . flat?)
            (syntax-case bindings ()
              (((var val more ...) ...)
               (datum->syntax-object
                #'bindings
                (map (lambda (x y) (apply process stx x y flat?))
                     (syntax->list #'(var ...))
                     (syntax->list #'((val more ...) ...)))
                #'bindings)))))
         (mk-let
          (lambda (tag . lbl)
            (lambda (stx)
              (syntax-case stx ()
                ((_ label bindings body0 body ...)
                 (and (identifier? #'label) (pair? lbl))
                 (quasisyntax/loc stx
                   (#,(car lbl) label #,(mk-bindings stx #'bindings #t)
                    body0 body ...)))
                ((_ bindings body0 body ...)
                 (quasisyntax/loc stx
                   (#,tag #,(mk-bindings stx #'bindings) body0 body ...))))))))
    (values (mk-let #'let-values #'let)
            (mk-let #'let*-values)
            (mk-let #'letrec-values))))

;;>> (lambda formals body ...)
;;>   The standard `lambda' is extended with Lisp-like &-keywords in its
;;>   argument list.  This extension is available using the above short
;;>   syntax.  Available &-keywords are:
(provide (rename lambda~ lambda))
(define-syntax (lambda~ stx)
  (define (process-optional-arg o)
    (syntax-case o ()
      ((var default) (identifier? #'var) (list #'var #'default))
      ((var) (identifier? #'var) (list #'var #'#f))
      (var (identifier? #'var) (list #'var #'#f))
      (var (raise-syntax-error #f "not a valid &optional spec" stx #'var))))
  (define (process-keyword-arg k)
    (define (key var)
      (datum->syntax-object
       k
       (string->symbol
        (string-append ":" (symbol->string (syntax-object->datum var))))
       k k))
    (syntax-case k ()
      ((var key default) (identifier? #'var) (list #'var #'key #'default))
      ((var default) (identifier? #'var) (list #'var (key #'var) #'default))
      ((var) (identifier? #'var) (list #'var (key #'var) #'#f))
      (var (identifier? #'var) (list #'var (key #'var) #'#f))
      (var (raise-syntax-error #f "not a valid &key spec" stx #'var))))
  (syntax-case stx ()
    ((_ formals body ...)
     (let ((vars          '())
           (opts          '())
           (keys          '())
           (rest          #f)
           (rest-keys     #f)
           (rest-all-keys #f))
       (let loop ((state #f) (args #'formals))
         (syntax-case args ()
           (() #f)
           ((v . xs)
            (let* ((v  #'v)
                   (k  (if (symbol? v) v (and (identifier? v) (syntax-e v))))
                   (x  (and k (symbol->string k))))
              (cond
               ;; check &-keywords according to their name, so something like
               ;;  (let ((&rest 1)) (lambda (&rest r) ...))
               ;; works as expected
               ((and x (> (string-length x) 0) (eq? #\& (string-ref x 0)))
                (case k
;;>   * &optional, &opt, &opts: denote an optional argument, possibly with a
;;>     default value (if the variable is specified as `(var val)').
;;>       => ((lambda (x &optional y (z 3)) (list x y z)) 1)
;;>       (1 #f 3)
;;>       => ((lambda (x &optional y (z 3)) (list x y z)) 1 2 #f)
;;>       (1 2 #f)
                  ((&optional &opt &opts)
                   (if state
                     (raise-syntax-error
                      #f "misplaced &optional argument" stx #'formals)
                     (loop 'o #'xs)))
;;>   * &keys, &key: a keyword argument -- the variable should be specified
;;>     as `x' or `(x)' to be initialized by an `:x' keyword, `(x v)' to
;;>     specify a default value `v', and `(x k v)' to further specify an
;;>     arbitrary keyword `k'.
;;>       => ((lambda (&key x (y 2) (z :zz 3)) (list x y z)) :x 'x :zz 'z)
;;>       (x 2 z)
;;>     Note that keyword values take precedence on the left, and that
;;>     keywords are not verified:
;;>       => ((lambda (&key y) y) :y 1 :z 3 :y 2)
;;>       1
                  ((&key &keys)
                   (if (memq state '(#f o r))
                     (loop 'k #'xs)
                     (raise-syntax-error
                      #f "misplaced &keys argument" stx #'formals)))
;;>   * &rest: a `rest' argument which behaves exactly like the Scheme dot
;;>     formal parameter and is actually a synonym for it.  Note that in
;;>     case of optional arguments, the rest variable holds any arguments
;;>     that were not used for defaults, but using keys doesn't change its
;;>     value.  For example:
;;>       => ((lambda (x &rest r) r) 1 2 3)
;;>       (2 3)
;;>       => ((lambda (x &optional y &rest r) r) 1)
;;>       ()
;;>       => ((lambda (x &optional y &rest r) r) 1 2 3)
;;>       (3)
;;>       => ((lambda (x &optional y . r) r) 1 2 3)
;;>       (3)
;;>       => ((lambda (x &key y &rest r) (list y r)) 1 :y 2 3 4)
;;>       (2 (:y 2 3 4))
;;>       => ((lambda (x &key y &rest r) (list y r)) 1 :y 2 3 4 5)
;;>       (2 (:y 2 3 4 5))
;;>     Note that the last two examples indicate that there is no error if
;;>     the given argument list is not balanced.
                  ((&rest)
                   (if (pair? (syntax-e #'xs))
                     (loop 'rr #'xs)
                     (raise-syntax-error
                      #f "no name for &rest argument" stx #'formals)))
;;>   * &rest-keys: similar to `&rest', but all specified keys are removed
;;>     with their values.
;;>       => ((lambda (x &key y &rest r) r) 1 :x 2 :y 3)
;;>       (:x 2 :y 3)
;;>       => ((lambda (x &key y &rest-keys r) r) 1 :x 2 :y 3)
;;>       (:x 2)
                  ((&rest-keys)
                   (if (pair? (syntax-e #'xs))
                     (loop 'rk #'xs)
                     (raise-syntax-error
                      #f "no name for &rest-keys argument" stx #'formals)))
;;>   * &rest-all-keys: similar to `&rest-keys', but all key/values are
;;>     removed one by one until a non-key is encountered.
;;>   * &body: a synonym for `&rest-all-keys', (warning: this is not the
;;>     same as in CL).
;;>       => ((lambda (x &key y &body r) r) 1 :x 2 :y 3)
;;>       ()
;;>       => ((lambda (x &key y &body r) r) 1 :x 2 :y 3 5 6)
;;>       (5 6)
                  ((&rest-all-keys &body)
                   (if (pair? (syntax-e #'xs))
                     (loop 'ra #'xs)
                     (raise-syntax-error
                      #f "no name for &rest-all-keys or &body argument"
                      stx #'formals)))
                  (else (raise-syntax-error
                         #f "unknown lambda &-keyword" stx v))))
               ((not (or x (memq state '(o k))))
                (raise-syntax-error #f "not an identifier" stx v))
               (else
                (case state
                  ((#f) (set! vars (cons v vars)))
                  ((o)  (set! opts (cons v opts)))
                  ((k)  (set! keys (cons v keys)))
                  ((r)  (raise-syntax-error
                         #f "no &-keyword after &rest arg" stx v))
                  ((rr)
                   (when rest
                     (raise-syntax-error
                      #f "too many &rest arguments" stx #'formals))
                   (set! rest v) (set! state 'r))
                  ((rk)
                   (when rest-keys
                     (raise-syntax-error
                      #f "too many &rest-keys arguments" stx #'formals))
                   (set! rest-keys v) (set! state 'r))
                  ((ra)
                   (when rest-all-keys
                     (raise-syntax-error
                      #f "too many &rest-all-keys or &body arguments"
                      stx #'formals))
                   (set! rest-all-keys v) (set! state 'r))
                  (else (raise-syntax-error
                         #f "bad lambda formals" stx v)))
                (loop state #'xs)))))
           (v (loop state #'(&rest v)))))
       (set! vars (reverse! vars))
       (set! opts (map process-optional-arg (reverse! opts)))
       (set! keys (map process-keyword-arg  (reverse! keys)))
       (cond ((and (null? vars) (null? opts) (null? keys))
              (quasisyntax/loc stx
                (lambda #,(or rest #'()) body ...)))
             ((and (null? opts) (null? keys))
              (quasisyntax/loc stx
                (lambda (#,@vars . #,(or rest #'())) body ...)))
             (else
              (unless rest (set! rest #'rest))
              (quasisyntax/loc stx
                (lambda (#,@vars . #,rest)
                  (let* (#,@(map (lambda (o)
                                   #`(#,(car o)
                                      (if (pair? #,rest)
                                        (begin0 (car #,rest)
                                          (set! #,rest (cdr #,rest)))
                                        #,(cadr o))))
                                 opts)
                         #,@(map (lambda (k)
                                   #`(#,(car k)
                                      (getarg #,rest #,(cadr k)
                                              (lambda () #,(caddr k)))))
                                 keys)
                         #,@(if rest-keys
                              #`((#,rest-keys
                                  (filter-out-keys '#,(map cadr keys)
                                                   #,rest)))
                              #'())
                         #,@(if rest-all-keys
                              #`((#,rest-all-keys
                                  (filter-out-all-keys #,rest)))
                              #'()))
                    body ...)))))))))

;; Utilities for the above (note: no errors for odd length)
(provide keyword? syntax-keyword?
         getarg syntax-getarg getargs keys/args filter-out-keys)
;;>> (keyword? x)
;;>   A predicate for keyword symbols (symbols that begin with a ":").
(define (keyword? x)
  (and (symbol? x) (not (eq? x '||))
       (eq? (string-ref (symbol->string x) 0) #\:)))
;;>> (syntax-keyword? x)
;;>   Similar to `keyword?' but also works for an identifier (a syntax
;;>   object) that contains a keyword.
(define (syntax-keyword? x)
  (keyword? (if (syntax? x) (syntax-e x) x)))
;;>> (getarg args keyword [not-found])
;;>   Searches the given list of arguments for a value matched with the
;;>   given keyword.  Similar to CL's `getf', except no error checking is
;;>   done for an unbalanced list.  In case no value is found, the optional
;;>   default value can be used -- this can be either a thunk, a promise, or
;;>   any other value that will be used as is.  For a repeated keyword the
;;>   leftmost occurrence is used.
(define (getarg args keyword . not-found)
  (let loop ((args args))
    (cond ((or (null? args) (null? (cdr args)))
           (and (pair? not-found)
                (let ((x (car not-found)))
                  (cond ((procedure? x) (x))
                        ((promise? x) (force x))
                        (else x)))))
          ((eq? (car args) keyword) (cadr args))
          (else (loop (cddr args))))))
;;>> (syntax-getarg syntax-args keyword [not-found])
;;>   Similar to `getarg' above, but the input is a syntax object of a
;;>   keyword-value list.
(define (syntax-getarg syntax-args keyword . not-found)
  (when (syntax? keyword) (set! keyword (syntax-e keyword)))
  (let loop ((args syntax-args))
    (syntax-case args ()
      ((key arg . more)
       (if (eq? (syntax-e #'key) keyword) #'arg (loop #'more)))
      (_ (and (pair? not-found)
              (let ((x (car not-found)))
                (cond ((procedure? x) (x))
                      ((promise? x) (force x))
                      (else x))))))))
;;>> (getargs initargs keyword)
;;>   The same as `getarg' but return the list of all key values matched --
;;>   no need for a default value.  The result is in the same order as in
;;>   the input.
(define (getargs initargs keyword)
  (define (scan tail)
    (cond ((null? tail) '())
          ((null? (cdr tail)) (error 'getargs "keyword list not balanced."))
          ((eq? (car tail) keyword) (cons (cadr tail) (scan (cddr tail))))
          (else (scan (cddr tail)))))
  (scan initargs))
;;>> (keys/args args)
;;>   The given argument list is scanned and split at the point where there
;;>   are no more keyword-values, and the two parts are returned as two
;;>   values.
;;>     => (keys/args '(:a 1 :b 2 3 4 5))
;;>     (:a 1 :b 2)
;;>     (3 4 5)
(define (keys/args args)
  (let loop ((args args) (keys '()))
    (cond ((or (null? args) (null? (cdr args)) (not (keyword? (car args))))
           (values (reverse! keys) args))
          (else (loop (cddr args) (list* (cadr args) (car args) keys))))))
;;>> (filter-out-keys outs args)
;;>   The keywords specified in the outs argument, with their matching
;;>   values are filtered out of the second arguments.
(define (filter-out-keys outs args)
  (let loop ((as args) (r '()))
    (cond ((null? as) (reverse! r))
          ((null? (cdr as)) (reverse! (cons (car as) r)))
          (else
           (loop (cddr as)
                 (if (memq (car as) outs) r (list* (cadr as) (car as) r)))))))
(define (filter-out-all-keys args)
  (if (or (null? args) (null? (cdr args)) (not (keyword? (car args))))
    args
    (filter-out-all-keys (cddr args))))

)
