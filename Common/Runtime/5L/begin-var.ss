;;=========================================================================
;;  (var ...) Declarations
;;=========================================================================
;;  We want our users to be able to declare variables anywhere within a
;;  body of code, not just at the top (as allowed by R5RS "internal
;;  definitions").  To do this, we provide a (begin/var ...) form which
;;  can be used as follows:
;;
;;    (begin/var
;;      1
;;      (define x 2)
;;      3
;;      (define y)
;;      (list x y))
;;
;;  Variables are bound by a LETREC covering the entire scope of the body,
;;  but will remain uninitialized until execution reaches the appropriate
;;  DEFINE.
;;
;;  * Features & Limitations
;;
;;  If the BEGIN/VAR form contains no "out-of-place" definitions, it
;;  will be handled by MzScheme.  If there are "out-of-place" definitions,
;;  the body is considered "complex", and is subject to the following
;;  restrictions:
;;
;;    - DEFINE and DEFINE-VALUES work.
;;    - Other internal definitions do not.  This is a bug.
;;    - It may not appear at the top-level.

(module begin-var (lib "lispish.ss" "5L")
  
  (provide define/var begin/var)
  
  (define-syntax define/var
    ;; We need to define this here, and explicitly support it below,
    ;; because MzScheme's module-identifier=? pays strict attention
    ;; to the identity of bindings.  Since some of the modules which
    ;; import us will want to rename DEFINE/VAR to DEFINE, we have to
    ;; provide our own copy of DEFINE/VAR and be prepared to handle it
    ;; everywhere we'd normally expect a DEFINE.
    (syntax-rules ()
      [(define/var (synopsis ...) body ...)
       (define (synopsis ...) (begin/var body ...))]
      [(define/var other ...)
       (define other ...)]))
  
  (define-syntax (begin/var stx) 
      
    (define (definition? elem-stx)
      ;; Does the expression elem-stx appear to be a definition?
      (let [[elem (syntax-e elem-stx)]]
        (and (pair? elem)
             (identifier? (car elem))
             (let [[id (car elem)]]
               (or (module-identifier=? id #'define)
		   (module-identifier=? id #'define/var)
                   (module-identifier=? id #'define-syntax)
                   (module-identifier=? id #'define-values)
                   (module-identifier=? id #'define-syntaxes)
                   (module-identifier=? id #'define-struct))))))
    
    (define (simple-body? body)
      ;; Does 'body' contain any internal definitons anywhere
      ;; but at the top of the body?
      (let examine-defs [[body body]]
        (cond
          [(null? body) #t]
          [(definition? (car body)) (examine-defs (cdr body))]
          [else
           (let examine-rest [[body body]]
             (cond
               [(null? body) #t]
               [(definition? (car body)) #f]
               [else (examine-rest (cdr body))]))])))
    
    (define (transform-body body)
      ;; Transform a complex body into a letrec.  We keep fully-processed
      ;; letrec clauses in 'letrec-clauses', and any outstanding elements
      ;; in 'elements' (both lists are in reverse order).  When we
      ;; encounter a new definition, we use that definition and whatever
      ;; is in 'elements' to build a new letrec clause.
      ;;
      ;; We don't have any way to handle syntax, structures, etc., and
      ;; we don't implement Scheme's bizarre "define in top-level begin"
      ;; semantics.  So we must refuse to handle those cases.
      (let [[letrec-clauses '()]
            [elements '()]]
        (let loop [[body body]]
          (when (not (null? body))
            (if (not (definition? (car body)))
                (set! elements (cons (car body) elements))
                (let* [[before (reverse! elements)]
                       [clause
                        (syntax-case (car body) (define define/var
						 define-values)
                          [(define (name . args) . function-body)
                           #`([name]
                              (begin
                                #,@before
                                (lambda args
                                  (begin/var . function-body))))]
                          [(define name expr)
                           #`([name] (begin #,@before expr))]
                          [(define . junk)
                           (raise-syntax-error #f "malformed define"
                                               stx (car body))]
                          [(define/var (name . args) . function-body)
                           #`([name]
                              (begin
                                #,@before
                                (lambda args
                                  (begin/var . function-body))))]
                          [(define/var name expr)
                           #`([name] (begin #,@before expr))]
                          [(define/var . junk)
                           (raise-syntax-error #f "malformed define"
                                               stx (car body))]
                          [(define-values names expr)
                           #`(names (begin #,@before expr))]
                          [(define-values . junk)
                           (raise-syntax-error #f "malformed define-values"
                                               stx (car body))]                           
                          [other
                           (raise-syntax-error #f "complex body only allows variable definitions"
                                               stx (car body))])]]
                  (set! elements '())
                  (set! letrec-clauses (cons clause letrec-clauses))))
            (loop (cdr body))))
        (if (null? elements)
            (raise-syntax-error #f "body ends with definition" stx stx)
            (quasisyntax/loc stx
                             (letrec-values #,(reverse! letrec-clauses)
                               #,@(reverse! elements))))))

    (syntax-case stx ()
      [(begin/var . body-stx)
       (let [[body (syntax->list #'body-stx)]]
         ;; Decide whether mzscheme should handle this, or whether we
         ;; should process it ourself.  MzScheme knows how to handle
         ;; define-syntax, define-struct, etc., but we know how to
         ;; handle define and define-values in arbitrary locations.
         (if (simple-body? body)
             (quasisyntax/loc stx (begin . body-stx))
             (if (eq? (syntax-local-context) 'top-level)
                 (raise-syntax-error #f "complex body not allowed at top level"
                                     stx stx)
                 (transform-body body))))]))
  
  #|
  (define (sample)
    (begin/var
      1
      (define x 2)
      3
      (define (f a b c) (+ a b c))
      (define-values [y z] (values 4 5))
      6
      (f x y z)))
  |#
  
  ) ; end module
