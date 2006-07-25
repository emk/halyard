
(module classidmap mzscheme

  (require (lib "stx.ss" "syntax"))

  (define (make-method-apply id this orig-args)
    (let loop ([args orig-args][accum null])
      (cond
       [(stx-null? args)
	(list* id this orig-args)]
       [(stx-pair? args)
	(loop (stx-cdr args) (cons (stx-car args) accum))]
       [else
	(list* 'apply id this (reverse (cons args accum)))])))

  (define (find the-finder name src)
    (let ([this-id (syntax-local-value the-finder)])
      (datum->syntax-object this-id name src)))

  ;; Help Desk binding info:
  (define (binding from to stx)
    (syntax-property
     stx
     'bound-in-source
     (cons from (syntax-local-introduce to))))


  (define (make-this-map the-finder the-obj)
    (let ([set!-stx (datum->syntax-object the-finder 'set!)])
      (make-set!-transformer
       (lambda (stx)
	 (syntax-case stx ()
	   [(set! id expr)
	    (module-identifier=? (syntax set!) set!-stx)
	    (raise-syntax-error 'class "cannot mutate object identifier" stx)]
	   [(id . args)
	    (datum->syntax-object 
	     stx
	     (cons (find the-finder the-obj stx) (syntax args))
	     stx)]
	   [id (find the-finder the-obj stx)])))))

  (define (make-field-map the-finder the-obj the-binder field-accessor field-mutator field-pos/null)
    (let ([set!-stx (datum->syntax-object the-finder 'set!)])
      (make-set!-transformer
       (lambda (stx)
	 (syntax-case stx ()
	   [(set! id expr)
	    (module-identifier=? (syntax set!) set!-stx)
	    (binding
	     the-binder (syntax id)
	     (datum->syntax-object 
	      the-finder
	      (list* field-mutator (find the-finder the-obj stx) (append field-pos/null (list (syntax expr))))
	      stx))]
	   [(id . args)
	    (binding
	     the-binder (syntax id)
	     (datum->syntax-object 
	      the-finder
	      (cons (list* field-accessor (find the-finder the-obj stx) field-pos/null) (syntax args))
	      stx))]
	   [_else
	    (binding
	     the-binder stx
	     (datum->syntax-object 
	      the-finder
	      (list* field-accessor (find the-finder the-obj stx) field-pos/null)
	      stx))])))))

  (define (make-method-map the-finder the-obj the-binder method-accessor)
    (let ([set!-stx (datum->syntax-object the-finder 'set!)])
      (make-set!-transformer
       (lambda (stx)
	 (syntax-case stx ()
	   [(set! id expr)
	    (module-identifier=? (syntax set!) set!-stx)
	    (raise-syntax-error 'class "cannot mutate method" stx)]
	   [(id . args)
	    (binding
	     the-binder (syntax id)
	     (datum->syntax-object 
	      the-finder
	      (make-method-apply
	       (list method-accessor (find the-finder the-obj stx))
	       (find the-finder the-obj stx)
	       (syntax args))
	      stx))]
	   [_else
	    (raise-syntax-error 
	     'class 
	     "misuse of method (not in application)" 
	     stx)])))))

  ;; For methods that are dirrectly available via their names
  ;;  (e.g., private methods)
  (define (make-direct-method-map the-finder the-obj the-binder new-name)
    (let ([set!-stx (datum->syntax-object the-finder 'set!)])
      (make-set!-transformer
       (lambda (stx)
	 (syntax-case stx ()
	   [(set! id expr)
	    (module-identifier=? (syntax set!) set!-stx)
	    (raise-syntax-error 'class "cannot mutate method" stx)]
	   [(id . args)
	    (binding
	     the-binder (syntax id)
	     (datum->syntax-object 
	      the-finder
	      (make-method-apply (find the-finder new-name stx) (find the-finder the-obj stx) (syntax args))
	      stx))]
	   [_else
	    (raise-syntax-error 
	     'class 
	     "misuse of method (not in application)" 
	     stx)])))))

  (define (make-rename-map the-finder the-obj the-binder rename-temp)
    (let ([set!-stx (datum->syntax-object the-finder 'set!)])
      (make-set!-transformer
       (lambda (stx)
	 (syntax-case stx ()
	   [(set! id expr)
	    (module-identifier=? (syntax set!) set!-stx)
	    (raise-syntax-error 'class "cannot mutate super method" stx)]
	   [(id . args)
	    (binding
	     the-binder (syntax id)
	     (datum->syntax-object 
	      the-finder
	      (make-method-apply (find the-finder rename-temp stx) (find the-finder the-obj stx) (syntax args))
	      stx))]
	   [_else
	    (raise-syntax-error 
	     'class 
	     "misuse of super method (not in application)" 
	     stx)])))))

  (define init-error-map
    (make-set!-transformer
     (lambda (stx)
       (raise-syntax-error 
	'class
	"cannot use non-field init variable in a method"
	stx))))

  (define super-error-map
    (make-set!-transformer
     (lambda (stx)
       (raise-syntax-error 
	'class
	"cannot use superclass initialization form in a method"
	stx))))

  (define (make-with-method-map set!-stx id-stx method-stx method-obj-stx)
    (make-set!-transformer
     (lambda (stx)
       (syntax-case stx ()
	 [(set! id expr)
	  (module-identifier=? (syntax set!) set!-stx)
	  (raise-syntax-error 'with-method "cannot mutate method" stx)]
	 [(id . args)
	  (datum->syntax-object 
	   set!-stx
	   (make-method-apply
	    method-stx
	    method-obj-stx
	    (syntax args))
	   stx)]
	 [_else
	  (raise-syntax-error 
	   'with-method 
	   "misuse of method (not in application)" 
	   stx)]))))

  (define (flatten-args orig-args)
    (let loop ([args orig-args][accum null])
      (cond
       [(stx-null? args) orig-args]
       [(stx-pair? args)
	(loop (stx-cdr args) (cons (stx-car args) accum))]
       [else
	(reverse (cons args accum))])))

  (define-struct private-name (orig-id gen-id))

  (define (localize id)
    (let ([v (syntax-local-value id (lambda () #f))])
      (if (and v (private-name? v))
	  (list 'unquote 
		(binding (private-name-orig-id v)
			 id
			 (private-name-gen-id v)))
	  id)))


  (provide make-this-map make-field-map make-method-map 
	   make-direct-method-map make-rename-map
	   init-error-map super-error-map 
	   make-with-method-map
	   flatten-args
	   make-private-name localize))

    
