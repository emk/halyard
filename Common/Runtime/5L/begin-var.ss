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
;;      (var x 2)
;;      3
;;      (var y)
;;      (list x y))
;;
;;  Uninitialized variables default to #f.  Each variable introduced by
;;  (var ...) has scope from its declaration until the end of the current
;;  block.  Fun!

(module begin-var mzscheme
  
  #|
  (define sample (syntax (1 (var v 2) 3 v)))
  
  (define $var-identifier (syntax var))
    
  (define (var-form? stx)
    (let [[expanded (syntax-e stx)]]
      (and (pair? expanded)
           (identifier? (car expanded))
           (free-identifier=? (car expanded) $var-identifier)))) 
  |#
  
  (define-syntax (begin/var stx) 
    (syntax-case stx (var)
      [(_)
       (quasisyntax/loc stx (begin))]
      [(_ (var . args))
       (raise-syntax-error #f "var at end of body is useless" stx)]
      [(_ (var name) . body)
       (quasisyntax/loc stx (let [[name #f]] (begin/var . body)))]
      [(_ (var name value) . body)
       (quasisyntax/loc stx (let [[name value]] (begin/var . body)))]
      [(_ (var . args) . body)
       (raise-syntax-error #f "var requires a name and optional value"
                           stx #'args)]
      [(_ expr)
       (quasisyntax/loc stx expr)]
      [(_ expr . body)
       (quasisyntax/loc stx (begin expr (begin/var . body)))]))

  ) ; end module
