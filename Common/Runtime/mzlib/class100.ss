
(module class100 mzscheme
  (require "class.ss")

  (require-for-syntax (lib "stx.ss" "syntax"))

  (define-syntax class100*/names
    (lambda (stx)
      (let ([main
	     (lambda (stx)
	       (syntax-case stx ()
		 [(_ (this-id super-init-id . optional-super-inst-id)
		     super-expr
		     (interface-expr ...)
		     init-vars
		     clauses ...)
		  (let ([se (lambda (msg expr)
			      (raise-syntax-error #f msg stx expr))])
		    ;; Check this and super-init:
		    (unless (identifier? (syntax this-id))
		      (se "not an identifier" (syntax this-id)))
		    (unless (identifier? (syntax super-init-id))
		      (se "not an identifier" (syntax super-init-id)))
		    ;; Unpack init arguments, with default expressions:
		    (let-values ([(init-ids init-defs init-rest-id)
				  (let loop ([inits (syntax init-vars)][need-def? #f])
				    (syntax-case inits ()
				      [() (values null null #f)]
				      [id 
				       (identifier? (syntax id))
				       (values null null (syntax id))]
				      [(id . rest) 
				       (identifier? (syntax id))
				       (begin
					 (when need-def?
					   (se "expected identifier with default value" (syntax id)))
					 (let-values ([(ids defs rest) (loop (syntax rest) #f)])
					   (values (cons (syntax id) ids)
						   (cons #f defs)
						   rest)))]
				      [((id def) . rest)
				       (identifier? (syntax id))
				       (let-values ([(ids defs rest) (loop (syntax rest) #f)])
					 (values (cons (syntax id) ids)
						 (cons (syntax def) defs)
						 rest))]
				      [(first . rest)
				       (se "bad initialization declaration" (syntax first))]
				      [else (se "improper identifier list" (syntax init-vars))]))])
		      ;; Unpack all body clauses:
		      (let* ([extract-ivars
			      ;; Unpacks a public, private, or override clause
			      (lambda (kind can-rename? decls)
				(map
				 (lambda (decl)
				   (syntax-case decl ()
				     [id (identifier? (syntax id))
					 (list kind (syntax id) (syntax id) (syntax/loc (syntax id) (void)))]
				     [(id expr) (identifier? (syntax id))
				      (list kind (syntax id) (syntax id) (syntax expr))]
				     [(id) (and can-rename? (identifier? (syntax id)))
				      (list kind (syntax id) (syntax id) (syntax/loc (syntax id) (void)))]
				     [((iid eid) expr) (and can-rename?
							    (identifier? (syntax iid))
							    (identifier? (syntax eid)))
				      (list kind (syntax iid) (syntax eid) (syntax expr))]
				     [else (se (format "bad ~a clause" kind) decl)]))
				 (syntax->list decls)))]
			     [body 
			      ;; Make a list of normalized clause-like lists, e.g:
			      ;;  (list (list 'public internal-id extenal-id expr) ...)
			      (apply
			       append
			       (map
				(lambda (clause)
				  (syntax-case clause (public override private private-field rename inherit sequence)
				    [(public decl ...)
				     (extract-ivars 'public #t (syntax (decl ...)))]
				    [(override decl ...)
				     (extract-ivars 'override #t (syntax (decl ...)))]
				    [(private decl ...)
				     (extract-ivars 'private #f (syntax (decl ...)))]
				    [(private-field decl ...)
				     (extract-ivars 'private-field #f (syntax (decl ...)))]
				    [(rename (iid eid) ...)
				     (let ([iids (syntax->list (syntax (iid ...)))]
					   [eids (syntax->list (syntax (eid ...)))])
				       (for-each (lambda (s)
						   (unless (identifier? s)
						     (se "expected an identifier" s)))
						 (append iids eids))
				       (map (lambda (iid eid)
					      (list 'rename iid eid))
					    iids eids))]
				    [(inherit id ...)
				     (map
				      (lambda (decl)
					(syntax-case decl ()
					  [id (identifier? (syntax id))
					      (list 'inherit (syntax id) (syntax id))]
					  [(iid eid) (and (identifier? (syntax iid))
							  (identifier? (syntax eid)))
					   (list 'inherit (syntax iid) (syntax eid))]
					  [else (se "bad inherit clause" decl)]))
				      (syntax->list (syntax (id ...))))]
				    [(sequence expr ...)
				     (map
				      (lambda (expr)
					(list 'sequence expr))
				      (syntax->list (syntax (expr ...))))]
				    [else (se "not a class100 clause" clause)]))
				(syntax->list (syntax (clauses ...)))))]
			     [get-info (lambda (tags select)
					 (let loop ([body body])
					   (cond
					    [(null? body) null]
					    [(memq (caar body) tags)
					     (cons (select (car body)) (loop (cdr body)))]
					    [else (loop (cdr body))])))]
			     [make-idp (lambda (x) (list (cadr x) (caddr x)))]
			     [make-decl (lambda (x) (with-syntax ([name (cadr x)]
								  [expr (cadddr x)])
						      (syntax (define-values (name) expr))))]
			     [make-seq (lambda (x) (if (eq? (car x) 'private-field)
						       (with-syntax ([name (cadr x)]
								     [expr (cadddr x)])
							 (syntax (define-values (name) expr)))
						       (cadr x)))])
			;; Extract internal and external ids, and create xformed body:
			(with-syntax ([public-ipds (get-info '(public) make-idp)]
				      [override-ipds (get-info '(override) make-idp)]
				      [rename-ipds (get-info '(rename) make-idp)]
				      [inherit-ipds (get-info '(inherit) make-idp)]
				      [private-ids (get-info '(private) (lambda (x) (cadr x)))]
				      
				      [(method-decl ...) (get-info '(public override private) make-decl)]
				      [(expr ...) (get-info '(private-field sequence) make-seq)]
				      [(init-expr ...) (let loop ([init-ids init-ids]
								  [init-defs init-defs])
							 (cond
							  [(null? init-ids)
							   (if init-rest-id
							       (with-syntax ([init-rest-id init-rest-id])
								 (list (syntax (init-rest init-rest-id))))
							       (list (syntax (init-rest))))]
							  [(car init-defs)
							   (with-syntax ([id (car init-ids)]
									 [def (car init-defs)])
							     (cons (syntax (init [id def]))
								   (loop (cdr init-ids)
									 (cdr init-defs))))]
							  [else
							   (with-syntax ([id (car init-ids)])
							     (cons (syntax (init id))
								   (loop (cdr init-ids)
									 (cdr init-defs))))]))]
				      [super-instantiate-id (if (stx-pair? #'optional-super-inst-id)
								(stx-car #'optional-super-inst-id)
								'super-instantiate)])
			  (syntax/loc stx
			    (class*/names 
			     (this-id super-instantiate-id super-init-id) super-expr (interface-expr ...)
			     init-expr ...
			     (private . private-ids)
			     (public . public-ipds)
			     (override . override-ipds)
			     (rename . rename-ipds)
			     (inherit . inherit-ipds)
			     method-decl ...
			     expr ...))))))]))])

	(syntax-case stx ()
	  [(_ (this-id super-init-id)
	      super-expr
	      (interface-expr ...)
	      init-vars
	      clauses ...)
	   (main stx)]
	  
	  [(_ (this-id super-init-id super-inst-id)
	      super-expr
	      (interface-expr ...)
	      init-vars
	      clauses ...)
	   (identifier? #'super-inst-id)
	   (main stx)]
	  
	  ;; Error cases
	  ;; --
	  [(_ bad-this-super
	      super-expr
	      (interface-expr ...)
	      init-vars
	      clauses ...)
	   (raise-syntax-error 
	    #f
	    "bad this and super bindings"
	    stx
	    (syntax bad-this-super))]
	  ;; --
	  [(_ this-super
	      super-expr
	      bad-interface-seq
	      init-vars
	      clauses ...)
	   (raise-syntax-error 
	    #f
	    "expected sequence of interface expressions"
	    stx
	    (syntax bad-interface-seq))]
	  ;;
	  [(_ this-super
	      super-expr
	      interface-seq)
	   (raise-syntax-error 
	    #f
	    "missing initialization arguments"
	    stx
	    (syntax bad-this-super))]
	  [(_ this-super
	      super-expr)
	   (raise-syntax-error 
	    #f
	    "missing interface expressions"
	    stx
	    (syntax bad-this-super))]
	  [(_ this-super)
	   (raise-syntax-error 
	    #f
	    "missing this and super-init bindings"
	    stx
	    (syntax bad-this-super))]))))
  
  (define-syntax class100*
    (lambda (stx)
      (syntax-case stx ()
	[(_ super-expr
	    (interface-expr ...)
	    init-vars
	    clauses ...)
	 (with-syntax ([this (datum->syntax-object (stx-car stx) 'this stx)]
		       [super-init (datum->syntax-object (stx-car stx) 'super-init stx)])
	   (syntax/loc stx
	       (class100*/names (this super-init)
				super-expr 
				(interface-expr ...)
				init-vars
				clauses ...)))])))

  (define-syntax class100
    (lambda (stx)
      (syntax-case stx ()
	[(_ super-expr
	    init-vars
	    clauses ...)
	 (with-syntax ([this (datum->syntax-object (stx-car stx) 'this stx)]
		       [super-init (datum->syntax-object (stx-car stx) 'super-init stx)])
	   (syntax/loc stx 
	       (class100*/names (this super-init) super-expr () init-vars 
				clauses ...)))])))

  (define-syntax class100*-asi
    (lambda (stx)
      (syntax-case stx ()
	[(_ super (interface ...) body ...)
	 (with-syntax ([this (datum->syntax-object (stx-car stx) 'this stx)]
		       [super-init (datum->syntax-object (stx-car stx) 'super-init stx)])
	   (syntax/loc stx 
	     (class100*/names (this super-init) super (interface ...) args 
			      body ... (sequence (apply super-init args)))))])))

  (define-syntax class100-asi
    (lambda (stx)
      (syntax-case stx ()
	[(_ super body ...)
	 (with-syntax ([this (datum->syntax-object (stx-car stx) 'this stx)]
		       [super-init (datum->syntax-object (stx-car stx) 'super-init stx)])
	   (syntax/loc stx 
	     (class100*/names (this super-init) super () args 
			      body ... (sequence (apply super-init args)))))])))
  
  (provide class100 class100* class100*/names
	   class100-asi class100*-asi))
