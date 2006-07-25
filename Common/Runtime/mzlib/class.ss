
(module class mzscheme
  (require (lib "list.ss"))
  (require-for-syntax (lib "kerncase.ss" "syntax")
		      (lib "stx.ss" "syntax")
		      (lib "name.ss" "syntax")
		      "private/classidmap.ss")

  (define insp (current-inspector)) ; for all structures

  ;;--------------------------------------------------------------------
  ;;  class macros
  ;;--------------------------------------------------------------------

  (define-syntax class*/names
    (lambda (stx)
      (syntax-case stx ()
	[(_  (this-id . supers) super-expression (interface-expr ...)
	     defn-or-expr
	     ...)
	 (let-values ([(defn-and-exprs) (syntax->list (syntax (defn-or-expr ...)))]
		      [(this-id) (syntax this-id)]
		      [(the-obj) (datum->syntax-object (quote-syntax here) (gensym 'self))]
		      [(the-finder) (datum->syntax-object (quote-syntax here) (gensym 'find-self))]
		      [(super-instantiate-id super-make-object-id)
		       (let ([s (syntax supers)])
			 (if (stx-null? s)
			     (values (quote-syntax super-instantiate)
				     (quote-syntax super-make-object))
			     (values (stx-car s)
				     (let ([s2 (stx-cdr s)])
				       (if (stx-null? s2)
					   (quote-syntax super-make-object)
					   (begin0
					    (stx-car s2)
					    (unless (stx-null? (stx-cdr s2))
					      (when (and (identifier? (stx-car s))
							 (identifier? (stx-car s2)))
						(raise-syntax-error
						 #f
						 "extra forms following identifiers for this, super-instantiate, and super-make-object"
						 stx)))))))))])
	   (unless (identifier? this-id)
	     (raise-syntax-error
	      #f
	      "not an identifier for `this'"
	      stx
	      this-id))
	   (unless (identifier? super-instantiate-id)
	     (raise-syntax-error
	      #f
	      "not an identifier for `super-instantiate'"
	      stx
	      super-instantiate-id))
	   (unless (identifier? super-make-object-id)
	     (raise-syntax-error
	      #f
	      "not an identifier for `super-make-object'"
	      stx
	      super-make-object-id))

	   ;; ----- Expand definitions -----
	   (let ([defn-and-exprs (let* ([stop-forms
					 (append
					  (kernel-form-identifier-list (quote-syntax here))
					  (list 
					   (quote-syntax init)
					   (quote-syntax init-rest)
					   (quote-syntax field)
					   (quote-syntax init-field)
					   (quote-syntax inherit-field)
					   (quote-syntax private)
					   (quote-syntax public)
					   (quote-syntax override)
					   (quote-syntax public-final)
					   (quote-syntax override-final)
					   (quote-syntax rename)
					   (quote-syntax inherit)
					   this-id
					   super-instantiate-id
					   super-make-object-id))]
					[expand
					 (lambda (defn-or-expr)
					   (local-expand
					    defn-or-expr
					    'internal-define
					    stop-forms))])
				   (let loop ([l defn-and-exprs])
				     (if (null? l)
					 null
					 (let ([e (expand (car l))])
					   (syntax-case e (begin)
					     [(begin expr ...)
					      (loop (append
						     (syntax->list (syntax (expr ...)))
						     (cdr l)))]
					     [(begin . _)
					      (raise-syntax-error 
					       #f
					       "ill-formed begin expression"
					       e)]
					     [_else (cons e (loop (cdr l)))])))))]
		 [bad (lambda (msg expr)
			(raise-syntax-error #f msg stx expr))]
		 [class-name (let ([s (syntax-local-infer-name stx)])
			       (if (syntax? s)
				   (syntax-e s)
				   s))])

	     ;; ------ Basic syntax checks -----
	     (for-each (lambda (stx)
			 (syntax-case stx (init init-rest field init-field inherit-field
						private public override 
						public-final override-final
						rename inherit)
			   [(form idp ...)
			    (and (identifier? (syntax form))
				 (or (module-identifier=? (syntax form) (quote-syntax init))
				     (module-identifier=? (syntax form) (quote-syntax init-field))))

			    (let ([form (syntax-e (syntax form))])
			      (for-each 
			       (lambda (idp)
				 (syntax-case idp ()
				   [id (identifier? (syntax id)) 'ok]
				   [((iid eid)) (and (identifier? (syntax iid))
						     (identifier? (syntax eid))) 'ok]
				   [(id expr) (identifier? (syntax id)) 'ok]
				   [((iid eid) expr) (and (identifier? (syntax iid))
							  (identifier? (syntax eid))) 'ok]
				   [else
				    (bad 
				     (format
				      "~a element is not an optionally renamed identifier or identifier-expression pair"
				      form)
				     idp)]))
			       (syntax->list (syntax (idp ...)))))]
			   [(init . rest)
			    (bad "ill-formed init clause" stx)]
			   [(init-rest)
			    'ok]
			   [(init-rest rest)
			    (identifier? (syntax rest))
			    'ok]
			   [(init-rest . rest)
			    (bad "ill-formed init-rest clause" stx)]
			   [(init-field . rest)
			    (bad "ill-formed init-field clause" stx)]
			   [(field idp ...)
			    (for-each (lambda (idp)
					(syntax-case idp ()
					  [(id expr) (identifier? (syntax id)) 'ok]
					  [((iid eid) expr) (and (identifier? (syntax iid))
								  (identifier? (syntax eid)))
					   'ok]
					  [else
					   (bad 
					    "field element is not an optionally renamed identifier-expression pair"
					    idp)]))
				      (syntax->list (syntax (idp ...))))]
			   [(field . rest)
			    (bad "ill-formed field clause" stx)]
			   [(private id ...)
			    (for-each
			     (lambda (id)
			       (unless (identifier? id)
				 (bad "private element is not an identifier" id)))
			     (syntax->list (syntax (id ...))))]
			   [(private . rest)
			    (bad "ill-formed private clause" stx)]
			   [(form idp ...)
			    (and (identifier? (syntax form))
				 (ormap (lambda (f) (module-identifier=? (syntax form) f))
					(syntax-e (quote-syntax (public
								  override
								  public-final
								  override-final
								  inherit
								  inherit-field)))))
			    (let ([form (syntax-e (syntax form))])
			      (for-each
			       (lambda (idp)
				 (syntax-case idp ()
				   [id (identifier? (syntax id)) 'ok]
				   [(iid eid) (and (identifier? (syntax iid)) (identifier? (syntax eid))) 'ok]
				   [else
				    (bad 
				     (format
				      "~a element is not an identifier or pair of identifiers"
				      form)
				     idp)]))
			       (syntax->list (syntax (idp ...)))))]
			   [(public . rest)
			    (bad "ill-formed public clause" stx)]
			   [(override . rest)
			    (bad "ill-formed override clause" stx)]
			   [(public-final . rest)
			    (bad "ill-formed public-final clause" stx)]
			   [(override-final . rest)
			    (bad "ill-formed override-final clause" stx)]
			   [(inherit . rest)
			    (bad "ill-formed inherit clause" stx)]
			   [(inherit-field . rest)
			    (bad "ill-formed inherit-field clause" stx)]
			   [(rename idp ...)
			    (for-each 
			     (lambda (idp)
			       (syntax-case idp ()
				 [(iid eid) (and (identifier? (syntax id)) (identifier? (syntax eid))) 'ok]
				 [else
				  (bad 
				   "rename element is not a pair of identifiers"
				   idp)]))
			     (syntax->list (syntax (idp ...))))]
			   [(rename . rest)
			    (bad "ill-formed rename clause" stx)]
			   [_ 'ok]))
		       defn-and-exprs)

	     ;; ----- Sort body into different categories -----
	     (let ([extract (lambda (kws l out-cons)
			      ;; returns two lists: expressions that start with an identifier in `kws',
			      ;; and expressions that don't
			      (let loop ([l l])
				(if (null? l)
				    (values null null)
				    (let-values ([(in out) (loop (cdr l))])
				      (cond
				       [(and (stx-pair? (car l))
					     (let ([id (stx-car (car l))])
					       (and (identifier? id)
						    (ormap (lambda (k) (module-identifier=? k id)) kws))))
					(values (cons (car l) in) out)]
				       [else
					(values in (out-cons (car l) out))])))))]
		   [flatten (lambda (alone l)
			      (apply append
				     (map (lambda (i)
					    (let ([l (cdr (syntax->list i))])
					      (if alone
						  (map (lambda (i)
							 (if (identifier? i)
							     (alone i)
							     (cons (stx-car i)
								   (stx-car (stx-cdr i)))))
						       l)
						  l)))
					  l)))]
		   [pair (lambda (i) (cons i i))]
		   [normalize-init/field (lambda (i)
					   ;; Put i in ((iid eid) optional-expr) form
					   (cond
					    [(identifier? i) (list (list i i))]
					    [else (let ([a (stx-car i)])
						    (if (identifier? a)
							(cons (list a a) (stx-cdr i))
							i))]))]
		   [norm-init/field-iid (lambda (norm) (stx-car (stx-car norm)))]
		   [norm-init/field-eid (lambda (norm) (stx-car (stx-cdr (stx-car norm))))])
	       (let*-values ([(extract*) (lambda (kws l)
					   (let-values ([(in out) (extract kws l void)])
					     in))]
			     [(decls exprs)
			      (extract (syntax-e (quote-syntax (inherit-field
								private
								public
								override
								public-final
								override-final
								rename
								inherit)))
				       defn-and-exprs
				       cons)]
			     [(plain-inits)
			      ;; Normalize after, but keep un-normal for error reporting
			      (flatten #f (extract* (syntax-e 
						     (quote-syntax (init init-rest)))
						    exprs))]
			     [(normal-plain-inits) (map normalize-init/field plain-inits)]
			     [(init-rest-decls _)
			      (extract (list (quote-syntax init-rest))
				       exprs
				       void)]
			     [(inits)
			      (flatten #f (extract* (syntax-e 
						     (quote-syntax (init init-field)))
						    exprs))]
			     [(normal-inits)
			      (map normalize-init/field inits)]
			     [(plain-fields)
			      (flatten #f (extract* (list (quote-syntax field)) exprs))]
			     [(normal-plain-fields)
			      (map normalize-init/field plain-fields)]
			     [(plain-init-fields)
			      (flatten #f (extract* (list (quote-syntax init-field)) exprs))]
			     [(normal-plain-init-fields)
			      (map normalize-init/field plain-init-fields)]
			     [(inherit-fields)
			      (flatten pair (extract* (list (quote-syntax inherit-field)) decls))]
			     [(privates)
			      (flatten pair (extract* (list (quote-syntax private)) decls))]
			     [(publics)
			      (flatten pair (extract* (list (quote-syntax public)) decls))]
			     [(overrides)
			      (flatten pair (extract* (list (quote-syntax override)) decls))]
			     [(public-finals)
			      (flatten pair (extract* (list (quote-syntax public-final)) decls))]
			     [(override-finals)
			      (flatten pair (extract* (list (quote-syntax override-final)) decls))]
			     [(renames)
			      (flatten pair (extract* (list (quote-syntax rename)) decls))]
			     [(inherits)
			      (flatten pair (extract* (list (quote-syntax inherit)) decls))])
		 
		 ;; At most one init-rest:
		 (unless (or (null? init-rest-decls)
			     (null? (cdr init-rest-decls)))
		   (bad "multiple init-rest clauses" (cadr init-rest-decls)))

		 ;; Make sure init-rest is last
		 (unless (null? init-rest-decls)
		   (let loop ([l exprs] [saw-rest? #f])
		     (unless (null? l)
		       (cond
			[(and (stx-pair? (car l))
			      (identifier? (stx-car (car l))))
			 (let ([form (stx-car (car l))])
			   (cond
			    [(module-identifier=? #'init-rest form)
			     (loop (cdr l) #t)]
			    [(not saw-rest?) (loop (cdr l) #f)]
			    [(module-identifier=? #'init form)
			     (bad "init clause follows init-rest clause" (car l))]
			    [(module-identifier=? #'init-field form)
			     (bad "init-field clause follows init-rest clause" (car l))]
			    [else (loop (cdr l) #t)]))]
			[else (loop (cdr l) saw-rest?)]))))

		 ;; --- Check initialization on inits: ---
		 (let loop ([inits inits] [normal-inits normal-inits])
		   (unless (null? normal-inits)
		     (if (stx-null? (stx-cdr (car normal-inits)))
			 (loop (cdr inits)(cdr normal-inits))
			 (let loop ([inits (cdr inits)] [normal-inits (cdr normal-inits)])
			   (unless (null? inits)
			     (if (stx-null? (stx-cdr (car normal-inits)))
				 (bad "initializer without default follows an initializer with default"
				      (car inits))
				 (loop (cdr inits) (cdr normal-inits))))))))
		 
		 ;; ----- Extract method definitions; check that they look like procs -----
		 ;;  Optionally transform them, can expand even if not transforming.
		 (let* ([field-names (map norm-init/field-iid
					  (append normal-plain-fields normal-plain-init-fields))]
			[inherit-field-names (map car inherit-fields)]
			[plain-init-names (map norm-init/field-iid normal-plain-inits)]
			[inherit-names (map car inherits)]
			[rename-names (map car renames)]
			[local-public-normal-names (map car (append publics overrides))]
			[local-public-names (append (map car (append public-finals override-finals))
						    local-public-normal-names)]
			[local-method-names (append (map car privates) local-public-names)]
			[expand-stop-names (append
					    local-method-names
					    field-names
					    inherit-field-names
					    plain-init-names
					    inherit-names
					    rename-names
					    (list 
					     this-id
					     super-instantiate-id
					     super-make-object-id)
					    (kernel-form-identifier-list
					     (quote-syntax here)))]
			[add-method-property (lambda (l)
					       (syntax-property l 'method-arity-error #t))]
			[proc-shape (lambda (name expr xform?)
				      ;; expands an expression enough that we can check whether
				      ;; it has the right form; must use local syntax definitions
				      (define (expand expr locals)
					(local-expand
					 expr
					 'expression
					 (append locals expand-stop-names)))
				      ;; Checks whether the vars sequence is well-formed
				      (define (vars-ok? vars)
					(or (identifier? vars)
					    (stx-null? vars)
					    (and (stx-pair? vars)
						 (identifier? (stx-car vars))
						 (vars-ok? (stx-cdr vars)))))
				      ;; mk-name: constructs a method name
				      ;; for error reporting, etc.
				      (define (mk-name name)
					(datum->syntax-object 
					 #f 
					 (string->symbol (format "~a method~a~a" 
								 (syntax-e name)
								 (if class-name
								     " in "
								     "")
								 (or class-name 
								     ""))) 
					 #f))
				      ;; -- tranform loop starts here --
				      (let loop ([stx expr][can-expand? #t][name name][locals null])
					(syntax-case stx (lambda case-lambda letrec-values let-values)
					  [(lambda vars body1 body ...)
					   (vars-ok? (syntax vars))
					   (if xform?
					       (with-syntax ([the-obj the-obj]
							     [the-finder the-finder]
							     [name (mk-name name)])
						 (let ([l (syntax/loc stx 
							      (lambda (the-obj . vars) 
								(fluid-let-syntax ([the-finder (quote-syntax the-obj)])
										  body1 body ...)))])
						   (with-syntax ([l (add-method-property l)])
						     (syntax/loc stx 
							 (let ([name l]) name)))))
					       stx)]
					  [(lambda . _)
					   (bad "ill-formed lambda expression for method" stx)]
					  [(case-lambda [vars body1 body ...] ...)
					   (andmap vars-ok? (syntax->list (syntax (vars ...))))
					   (if xform?
					       (with-syntax ([the-obj the-obj]
							     [the-finder the-finder]
							     [name (mk-name name)])
						 (let ([cl (syntax/loc stx
							       (case-lambda [(the-obj . vars) 
									     (fluid-let-syntax ([the-finder (quote-syntax the-obj)])
											       body1 body ...)] ...))])
						   (with-syntax ([cl (add-method-property cl)])
						     (syntax/loc stx
							 (let ([name cl]) name)))))
					       stx)]
					  [(case-lambda . _)
					   (bad "ill-formed case-lambda expression for method" stx)]
					  [(let- ([(id) expr] ...) let-body)
					   (and (or (module-identifier=? (syntax let-) 
									 (quote-syntax let-values))
						    (module-identifier=? (syntax let-) 
									 (quote-syntax letrec-values)))
						(andmap identifier? (syntax->list (syntax (id ...)))))
					   (let* ([letrec? (module-identifier=? (syntax let-) 
										(quote-syntax letrec-values))]
						  [ids (syntax->list (syntax (id ...)))]
						  [new-ids (if xform?
							       (map
								(lambda (id)
								  (datum->syntax-object
								   #f
								   (gensym (syntax-e id))))
								ids)
							       ids)]
						  [body-locals (append ids locals)]
						  [exprs (map (lambda (expr id)
								(loop expr #t id (if letrec?
										     body-locals
										     locals)))
							      (syntax->list (syntax (expr ...)))
							      ids)]
						  [body (let ([body (syntax let-body)])
							  (if (identifier? body)
							      (ormap (lambda (id new-id)
								       (and (bound-identifier=? body id)
									    new-id))
								     ids new-ids)
							      (loop body #t name body-locals)))])
					     (unless body
					       (bad "bad form for method definition" stx))
					     (with-syntax ([(proc ...) exprs]
							   [(new-id ...) new-ids]
							   [mappings
							    (if xform?
								(map
								 (lambda (old-id new-id)
								   (with-syntax ([old-id old-id]
										 [new-id new-id]
										 [the-obj the-obj]
										 [the-finder the-finder])
								     (syntax (old-id (make-direct-method-map 
										      (quote-syntax the-finder)
										      (quote the-obj)
										      (quote-syntax old-id)
										      (quote new-id))))))
								 ids new-ids)
								null)]
							   [body body])
					       (if xform?
						   (if letrec?
						       (syntax/loc stx (letrec-syntax mappings
									 (let- ([(new-id) proc] ...) 
									       body)))
						       (syntax/loc stx (let- ([(new-id) proc] ...) 
									     (letrec-syntax mappings
									       body))))
						   (syntax/loc stx (let- ([(new-id) proc] ...) 
									 body)))))]
					  [_else 
					   (if can-expand?
					       (loop (expand stx locals) #f name locals)
					       (bad "bad form for method definition" stx))])))])
		   ;; Do the extraction:
		   (let-values ([(methods          ; (listof (cons id stx))
				  private-methods  ; (listof (cons id stx))
				  exprs            ; (listof stx)
				  stx-defines)     ; (listof (cons (listof id) stx))
				 (let loop ([exprs exprs][ms null][pms null][es null][sd null])
				   (if (null? exprs)
				       (values (reverse! ms) (reverse! pms) (reverse! es) (reverse! sd))
				       (syntax-case (car exprs) (define-values define-syntaxes)
					 [(define-values (id ...) expr)
					  (let ([ids (syntax->list (syntax (id ...)))])
					    ;; Check form:
					    (for-each (lambda (id)
							(unless (identifier? id)
							  (bad "not an identifier for definition" id)))
						      ids)
					    ;; method defn? (id in the list of privates/publics/overrides?)
					    (if (ormap (lambda (id)
							 (ormap (lambda (i) (bound-identifier=? i id))
								local-method-names))
						       ids)
						;; Yes, it's a method:
						(begin
						  (unless (null? (cdr ids))
						    (bad "each method variable needs its own definition"
							 (car exprs)))
						  (let ([expr (proc-shape #f (syntax expr) #f)]
							[public? (ormap (lambda (i) 
									  (bound-identifier=? i (car ids)))
									local-public-names)])
						    (loop (cdr exprs) 
							  (if public?
							      (cons (cons (car ids) expr) ms)
							      ms)
							  (if public?
							      pms
							      (cons (cons (car ids) expr) pms))
							  es
							  sd)))
						;; Non-method defn:
						(loop (cdr exprs) ms pms (cons (car exprs) es) sd)))]
					 [(define-values . _)
					  (bad "ill-formed definition" (car exprs))]
					 [(define-syntaxes (id ...) expr)
					  (let ([ids (syntax->list (syntax (id ...)))])
					    (for-each (lambda (id) (unless (identifier? id)
								     (bad "syntax name is not an identifier" id)))
						      ids)
					    (loop (cdr exprs) ms pms es (cons (cons ids (car exprs)) sd)))]
					 [(define-syntaxes . _)
					  (bad "ill-formed syntax definition" (car exprs))]
					 [_else
					  (loop (cdr exprs) ms pms (cons (car exprs) es) sd)])))])
		     
		     ;; ---- Extract all defined names, including field accessors and mutators ---
		     (let ([defined-syntax-names (apply append (map car stx-defines))]
			   [defined-method-names (append (map car methods)
							 (map car private-methods))]
			   [private-field-names (let loop ([l exprs])
						  (if (null? l)
						      null
						      (syntax-case (car l) (define-values)
							[(define-values (id ...) expr)
							 (append (syntax->list (syntax (id ...)))
								 (loop (cdr l)))]
							[_else (loop (cdr l))])))]
			   [init-mode (cond
				       [(null? init-rest-decls) 'normal]
				       [(stx-null? (stx-cdr (car init-rest-decls))) 'stop]
				       [else 'list])])

		       ;; -- Look for duplicates --
		       (let ([dup (check-duplicate-identifier
				   (append defined-syntax-names
					   defined-method-names
					   private-field-names
					   field-names
					   inherit-field-names
					   plain-init-names
					   inherit-names
					   rename-names
					   (list this-id super-instantiate-id super-make-object-id)))])
			 (when dup
			   (bad "duplicate declared identifier" dup)))
		       
		       ;; -- Could still have duplicates within private/public/override --
		       (let ([dup (check-duplicate-identifier local-method-names)])
			 (when dup
			   (bad "duplicate declared identifier" dup)))
		       
		       ;; -- Check for duplicate external method names, init names, or field names
		       (let ([check-dup
			      (lambda (what l)
				(let ([ht (make-hash-table)])
				  (for-each (lambda (id)
					      (when (hash-table-get ht (syntax-e id) (lambda () #f))
						(bad (format "duplicate declared external ~a name" what) id))
					      (hash-table-put! ht (syntax-e id) #t))
					    l)))])
			 ;; method names
			 (check-dup "method" (map cdr (append publics overrides public-finals override-finals)))
			 ;; inits
			 (check-dup "init" (map norm-init/field-eid (append normal-inits)))
			 ;; fields
			 (check-dup "field" (map norm-init/field-eid (append normal-plain-fields normal-plain-init-fields))))

		       ;; -- Check that private/public/override are defined --
		       (let ([ht (make-hash-table)]
			     [stx-ht (make-hash-table)])
			 (for-each
			  (lambda (defined-name)
			    (let ([l (hash-table-get ht (syntax-e defined-name) (lambda () null))])
			      (hash-table-put! ht (syntax-e defined-name) (cons defined-name l))))
			  defined-method-names)
			 (for-each
			  (lambda (defined-name)
			    (let ([l (hash-table-get stx-ht (syntax-e defined-name) (lambda () null))])
			      (hash-table-put! stx-ht (syntax-e defined-name) (cons defined-name l))))
			  defined-syntax-names)
			 (for-each
			  (lambda (pubovr-name)
			    (let ([l (hash-table-get ht (syntax-e pubovr-name) (lambda () null))])
			      (unless (ormap (lambda (i) (bound-identifier=? i pubovr-name)) l)
				;; Either undefined or defined as syntax:
				(let ([stx-l (hash-table-get stx-ht (syntax-e pubovr-name) (lambda () null))])
				  (if (ormap (lambda (i) (bound-identifier=? i pubovr-name)) stx-l)
				      (bad 
				       "method declared but defined as syntax"
				       pubovr-name)
				      (bad 
				       "method declared but not defined"
				       pubovr-name))))))
			  local-method-names))
		       
		       ;; ---- Convert expressions ----
		       ;;  Non-method definitions to set!
		       ;;  Initializations args access/set!
		       (let ([exprs (map (lambda (e)
					   (syntax-case e (define-values field init-rest)
					     [(define-values (id ...) expr)
					      (syntax/loc e (set!-values (id ...) expr))]
					     [(-init idp ...)
					      (and (identifier? (syntax -init))
						   (ormap (lambda (it) 
							    (module-identifier=? it (syntax -init)))
							  (syntax-e (quote-syntax (init
										   init-field)))))
					      (let* ([norms (map normalize-init/field
								 (syntax->list (syntax (idp ...))))]
						     [iids (map norm-init/field-iid norms)]
						     [exids (map norm-init/field-eid norms)])
						(with-syntax ([(id ...) iids]
							      [(idpos ...) (map localize exids)]
							      [(defval ...) 
							       (map (lambda (norm)
								      (if (stx-null? (stx-cdr norm))
									  (syntax #f)
									  (with-syntax ([defexp (stx-car (stx-cdr norm))])
									    (syntax (lambda () defexp)))))
								    norms)]
							      [class-name class-name])
						  (syntax/loc e 
						    (begin 
						      1 ; to ensure a non-empty body
						      (set! id (extract-arg 'class-name `idpos init-args defval))
						      ...))))]
					     [(field idp ...)
					      (with-syntax ([(((iid eid) expr) ...)
							     (map normalize-init/field (syntax->list #'(idp ...)))])
						(syntax/loc e (begin 
								1 ; to ensure a non-empty body
								(set! iid expr)
								...)))]
					     [(init-rest id/rename)
					      (with-syntax ([n (+ (length plain-inits)
								  (length plain-init-fields)
								  -1)]
							    [id (if (identifier? #'id/rename)
								    #'id/rename
								    (stx-car #'id/rename))])
						(syntax/loc e (set! id (extract-rest-args n init-args))))]
					     [(init-rest)
					      (syntax (void))]
					     [_else e]))
					 exprs)]
			     [mk-method-temp
			      (lambda (id-stx)
				(datum->syntax-object (quote-syntax here)
						      (gensym (syntax-e id-stx))))])
			 
			 ;; ---- set up field and method mappings ----
			 (with-syntax ([(rename-orig ...) (map car renames)]
				       [(rename-temp ...) (generate-temporaries (map car renames))]
				       [(private-name ...) (map car privates)]
				       [(private-temp ...) (map mk-method-temp (map car privates))]
				       [(public-final-name ...) (map car public-finals)]
				       [(override-final-name ...) (map car override-finals)]
				       [(public-final-temp ...) (map
								 mk-method-temp
								 (map car public-finals))]
				       [(override-final-temp ...) (map
								   mk-method-temp
								   (map car override-finals))]
				       [(method-name ...) (append local-public-normal-names
								  (map car inherits))]
				       [(method-accessor ...) (generate-temporaries
							       (map car
								    (append publics
									    overrides
									    inherits)))]
				       [(inherit-field-accessor ...) (generate-temporaries
								      (map (lambda (id)
									     (format "get-~a"
										     (syntax-e id)))
									   inherit-field-names))]
				       [(inherit-field-mutator ...) (generate-temporaries
								     (map (lambda (id)
										  (format "set-~a!"
											  (syntax-e id)))
									  inherit-field-names))]
				       [(inherit-field-name ...) inherit-field-names]
				       [(local-field ...) (append field-names
								  private-field-names)]
				       [(local-field-pos ...) (let loop ([pos 0][l (append field-names
											   private-field-names)])
								(if (null? l)
								    null
								    (cons pos (loop (add1 pos) (cdr l)))))]
				       [(plain-init-name ...) plain-init-names])
			   (let ([mappings
				  ;; make-XXX-map is supplied by private/classidmap.ss
				  (with-syntax ([the-obj the-obj]
						[the-finder the-finder]
						[this-id this-id])
				    (syntax 
				     ([(this-id
					inherit-field-name ...
					local-field ...
					rename-orig ...
					method-name ...
					private-name ...
					public-final-name ...
					override-final-name ...)
				       (values
					(make-this-map (quote-syntax the-finder)
						       (quote the-obj))
					(make-field-map (quote-syntax the-finder)
							(quote the-obj)
							(quote-syntax inherit-field-name)
							(quote-syntax inherit-field-accessor)
							(quote-syntax inherit-field-mutator)
							'())
					...
					(make-field-map (quote-syntax the-finder)
							(quote the-obj)
							(quote-syntax local-field)
							(quote-syntax local-accessor)
							(quote-syntax local-mutator)
							'(local-field-pos))
					...
					(make-rename-map (quote-syntax the-finder)
							 (quote the-obj)
							 (quote-syntax rename-orig)
							 (quote rename-temp))
					...
					(make-method-map (quote-syntax the-finder)
							 (quote the-obj)
							 (quote-syntax method-name)
							 (quote-syntax method-accessor))
					...
					(make-direct-method-map (quote-syntax the-finder)
								(quote the-obj)
								(quote-syntax private-name)
								(quote private-temp))
					...
					(make-direct-method-map (quote-syntax the-finder)
								(quote the-obj)
								(quote-syntax public-final-name)
								(quote public-final-temp))
					...
					(make-direct-method-map (quote-syntax the-finder)
								(quote the-obj)
								(quote-syntax override-final-name)
								(quote override-final-temp))
					...)])))]
				 [extra-init-mappings
				  (with-syntax ([super-instantiate-id super-instantiate-id]
						[super-make-object-id super-make-object-id]
						[(init-error-map ...)
						 (map (lambda (x)
							(syntax init-error-map))
						      plain-inits)])
				    (syntax 
				     ([(plain-init-name ... 
							super-instantiate-id
							super-make-object-id)
				       (values
					init-error-map
					...
					super-error-map
					super-error-map)])))])
			     
			     (let ([find-method 
				    (lambda (methods)
				      (lambda (name)
					(ormap 
					 (lambda (m)
					   (and (bound-identifier=? (car m) name)
						(with-syntax ([proc (proc-shape (car m) (cdr m) #t)]
							      [extra-init-mappings extra-init-mappings])
						  (syntax
						   (letrec-syntaxes+values extra-init-mappings ()
						     proc)))))
					 methods)))]
				   [localize-cdr (lambda (p) (localize (cdr p)))])
			       
			       ;; ---- build final result ----
			       (with-syntax ([public-names (map localize-cdr publics)]
					     [override-names (map localize-cdr overrides)]
					     [public-final-names (map localize-cdr public-finals)]
					     [override-final-names (map localize-cdr override-finals)]
					     [rename-names (map localize-cdr renames)]
					     [inherit-names (map localize-cdr inherits)]
					     [num-fields (datum->syntax-object
							  (quote-syntax here)
							  (+ (length private-field-names)
							     (length plain-init-fields)
							     (length plain-fields)))]
					     [field-names (map (lambda (norm)
								 (localize (norm-init/field-eid norm)))
							       (append
								normal-plain-fields
								normal-plain-init-fields))]
					     [inherit-field-names (map localize (map cdr inherit-fields))]
					     [init-names (map (lambda (norm)
								(localize
								 (norm-init/field-eid norm)))
							      normal-inits)]
					     [init-mode init-mode]
					     [(private-method ...) (map (find-method private-methods) (map car privates))]
					     [public-methods (map (find-method methods) (map car publics))]
					     [override-methods (map (find-method methods) (map car overrides))]
					     [(public-final-method ...) (map (find-method methods) (map car public-finals))]
					     [(override-final-method ...) (map (find-method methods) (map car override-finals))]
					     [mappings mappings]

					     [extra-init-mappings extra-init-mappings]
					     [exprs exprs]
					     [the-obj the-obj]
					     [the-finder the-finder]
					     [super-instantiate-id super-instantiate-id]
					     [super-make-object-id super-make-object-id]
					     [name class-name]
					     [(stx-def ...) (map cdr stx-defines)])
				 
				 (syntax
				  (let ([superclass super-expression]
					[interfaces (list interface-expr ...)])
				    (compose-class 
				     'name superclass interfaces
				     ;; Field count:
				     num-fields
				     ;; Field names:
				     `field-names
				     `inherit-field-names
				     ;; Method names:
				     `rename-names
				     `public-final-names
				     `public-names
				     `override-final-names
				     `override-names
				     `inherit-names
				     ;; Init arg names (in order)
				     `init-names
				     (quote init-mode)
				     ;; Methods (when given needed super-methods, etc.):
				     (lambda (local-accessor
					      local-mutator
					      inherit-field-accessor ...  ; inherit
					      inherit-field-mutator ...
					      rename-temp ...
					      method-accessor ...) ; public, override, inherit
				       (letrec-syntaxes+values mappings ()
					 stx-def ...
					 (letrec ([private-temp private-method]
						  ...
						  [public-final-temp public-final-method]
						  ...
						  [override-final-temp override-final-method]
						  ...)
					   (values
					    (list public-final-temp ... . public-methods)
					    (list override-final-temp ... . override-methods)
					    ;; Initialization
					    (lambda (the-obj super-id si_c si_inited? si_leftovers init-args)
					      (fluid-let-syntax ([the-finder (quote-syntax the-obj)])
						(letrec-syntax ([super-instantiate-id
								 (lambda (stx)
								   (syntax-case stx () 
								     [(_ (arg (... ...)) (kw kwarg) (... ...))
								      (with-syntax ([stx stx])
									(syntax (-instantiate super-id stx (the-obj si_c si_inited? si_leftovers)
											      (list arg (... ...)) 
											      (kw kwarg) (... ...))))]))]
								[super-make-object-id
								 (lambda (stx)
								   (let ([code 
									  (quote-syntax
									   (lambda args
									     (super-id the-obj si_c si_inited? si_leftovers args null)))])
								     (if (identifier? stx)
									 code
									 (datum->syntax-object
									  code
									  (cons code
										(cdr (syntax-e stx)))))))])
						  (let ([plain-init-name undefined]
							...)
						    (void) ; in case the body is empty
						    . exprs))))))))
				     ;; Not primitive:
				     #f)))))))))))))))])))

  (define-syntax class*
    (lambda (stx)
      (syntax-case stx ()
	[(form super-expression (interface-expr ...)
	       defn-or-expr
	       ...)
	 (with-syntax ([this (datum->syntax-object (syntax form) 'this stx)]
		       [super-init (datum->syntax-object (syntax form) 'super-instantiate stx)]
		       [super-make (datum->syntax-object (syntax form) 'super-make-object stx)])
	   (syntax/loc stx
	    (class*/names (this super-init super-make) super-expression (interface-expr ...)
			  defn-or-expr
			  ...)))])))

  (define-syntax :class
    (lambda (stx)
      (syntax-case stx ()
	[(form super-expression
	       defn-or-expr
	       ...)
	 (with-syntax ([class* (datum->syntax-object (syntax form) 'class* stx)])
	   (syntax/loc stx
	    (class* super-expression ()
		    defn-or-expr
		    ...)))])))

  (define-syntaxes (private* public* public-final* override* override-final*)
    (let ([mk
	   (lambda (who decl-form)
	     (lambda (stx)
	       (syntax-case stx ()
		 [(_ binding ...)
		  (let ([bindings (syntax->list (syntax (binding ...)))])
		    (let ([name-exprs
			   (map (lambda (binding)
				  (syntax-case binding ()
				    [(name expr)
				     (identifier? (syntax name))
				     (cons (syntax name) (syntax expr))]
				    [_else
				     (identifier? (syntax name))
				     (raise-syntax-error
				      #f
				      "expected an identifier and expression"
				      stx
				      binding)]))
				bindings)])
		      (with-syntax ([(name ...) (map car name-exprs)]
				    [(expr ...) (map cdr name-exprs)]
				    [decl-form decl-form])
			(syntax
			 (begin
			   (decl-form name ...)
			   (define name expr)
			   ...)))))])))])
      (values
       (mk 'private* (syntax private))
       (mk 'public* (syntax public))
       (mk 'public-final* (syntax public-final))
       (mk 'override* (syntax override))
       (mk 'override-final* (syntax override-final)))))

  (define-syntaxes (define/private define/public define/public-final define/override define/override-final)
    (let ([mk
	   (lambda (who decl-form)
	     (lambda (stx)
	       (syntax-case stx ()
		 [(_ name expr)
		  (identifier? (syntax name))
		  (with-syntax ([decl-form decl-form])
		    (syntax
		     (begin
		       (decl-form name)
		       (define name expr))))]
		 [(_ (name . ids) expr0 expr ...)
		  (and (identifier? (syntax name))
		       (let loop ([ids (syntax ids)])
			 (cond
			  [(identifier? ids) #t]
			  [(stx-null? ids) #t]
			  [(stx-pair? ids)
			   (and (identifier? (stx-car ids))
				(loop (stx-cdr ids)))]
			  [else (raise-syntax-error
				 #f
				 "bad identifier"
				 stx
				 ids)])))
		  (with-syntax ([decl-form decl-form])
		    (with-syntax ([decl (syntax/loc stx (decl-form name))]
				  [defn (syntax/loc stx (define (name . ids) expr0 expr ...))])
		      (syntax (begin decl defn))))]
		 [(_ d . __)
		  (or (identifier? (syntax d))
		      (and (stx-pair? (syntax d))
			   (identifier? (stx-car (syntax d)))))
		  (raise-syntax-error
		   #f
		   "bad syntax (wrong number of parts)"
		   stx)]
		 [(_ d . __)
		  (raise-syntax-error
		   #f
		   "bad syntax (no identifier for definition)"
		   stx
		   (syntax d))])))])
      (values
       (mk 'define/private (syntax private))
       (mk 'define/public (syntax public))
       (mk 'define/public-final (syntax public-final))
       (mk 'define/override (syntax override))
       (mk 'define/override-final (syntax override-final)))))

  (define-syntax (define-local-member-name stx)
    (syntax-case stx ()
      [(_ id ...)
       (let ([ids (syntax->list (syntax (id ...)))])
	 (for-each (lambda (id)
		     (unless (identifier? id)
		       (raise-syntax-error
			#f
			"expected an identifier"
			stx
			id)))
		   ids)
	 (let ([dup (check-duplicate-identifier ids)])
	   (when dup
	     (raise-syntax-error
	      #f
	      "duplicate identifier"
	      stx
	      dup)))
	 (if (eq? (syntax-local-context) 'top-level)
	     ;; Does nothing in particular at the top level:
	     (syntax/loc stx (define-syntaxes (id ...) (values 'id ...)))
	     ;; Map names to private indicators:
	     (with-syntax ([(gen-id ...) (map (lambda (id)
						;; Need to give the generated id the same context
						;; as the original id:
						(datum->syntax-object
						 id
						 (gensym (syntax-e id))))
					      ids)])
	       (with-syntax ([stx-defs
			      ;; Need to attach srcloc to this definition:
			      (syntax/loc stx
				(define-syntaxes (id ...)
				  (values (make-private-name (quote-syntax id) 
							     (quote-syntax gen-id)) 
					  ...)))])
		 (syntax/loc stx
		   (begin
		     (define-values (gen-id ...)
		       (values (generate-local-member-name 'id) ...))
		     stx-defs))))))]))

  (define (generate-local-member-name id)
    (string->uninterned-symbol
     (symbol->string id)))

  ;;--------------------------------------------------------------------
  ;;  class implementation
  ;;--------------------------------------------------------------------

  (define-struct class (name
			pos supers     ; pos is subclass depth, supers is vector
			>interface     ; self interface

			method-width   ; total number of methods
			method-ht      ; maps public names to vector positions
			method-ids     ; reverse-ordered list of public method names

			methods        ; vector of methods
			meth-flags     ; vector: #f => primitive-implemented
                                       ;         'final => final

			field-width    ; total number of fields
			field-ht       ; maps public field names to (cons accessor mutator)
			field-ids      ; list of public field names

			struct:object  ; structure type for instances
			object?        ; predicate
			make-object    ; constructor
			field-ref      ; accessor
			field-set!     ; mutator

			init-args      ; list of symbols in order; #f => only by position
			init-mode      ; 'normal, 'stop (don't accept by-pos for super), or 'list

			init           ; initializer

			no-super-init?); #t => no super-init needed
                    insp)

  (define (compose-class name                ; symbol
			 super               ; class
			 interfaces          ; list of interfaces

			 num-fields          ; total fields (public & private)
			 public-field-names  ; list of symbols (shorter than num-fields)
			 inherit-field-names ; list of symbols (not included in num-fields)
			 
			 rename-names        ; list of symbols
			 public-final-names
			 public-normal-names
			 override-final-names
			 override-normal-names
			 inherit-names

			 init-args           ; list of symbols in order
			 init-mode           ; 'normal, 'stop, or 'list
			 
			 make-methods        ; takes field and method accessors
			 make-struct:prim)   ; see "primitive classes", below

    ;; -- Check superclass --
    (unless (class? super)
      (obj-error 'class* "superclass expression returned a non-class: ~a~a" 
		 super
		 (for-class name)))
    ;; -- Create new class's name --
    (let* ([name (or name
		     (let ([s (class-name super)])
		       (and s 
			    (not (eq? super object%))
			    (if (symbol? s)
				(format "derived-from-~a" s)
				s))))]
	   ;; Combine method lists
	   [public-names (append public-final-names public-normal-names)]
	   [override-names (append override-final-names override-normal-names)]
	   [final-names (append public-final-names override-final-names)]
	   ;; Mis utilities
	   [no-new-methods? (null? public-names)]
	   [no-method-changes? (and (null? public-names)
				    (null? override-names))]
	   [no-new-fields? (null? public-field-names)]
	   [xappend (lambda (a b) (if (null? b) a (append a b)))])

      ;; -- Check interfaces ---
      (for-each
       (lambda (intf)
	 (unless (interface? intf)
	   (obj-error 'class*/names "interface expression returned a non-interface: ~a~a" 
		      intf
		      (for-class name))))
       interfaces)

      ;; -- Match method and field names to indices --
      (let ([method-ht (if no-new-methods?
			   (class-method-ht super)
			   (make-hash-table))]
	    [field-ht (if no-new-fields?
			  (class-field-ht super)
			  (make-hash-table))]
	    [super-method-ids (class-method-ids super)]
	    [super-field-ids (class-field-ids super)]
	    [super-field-ht (class-field-ht super)])

	;; Put superclass ids in tables, with pos
	(unless no-new-methods?
	  (let loop ([ids super-method-ids][p (sub1 (class-method-width super))])
	    (unless (null? ids)
	      (hash-table-put! method-ht (car ids) p)
	      (loop (cdr ids) (sub1 p)))))
	(unless no-new-fields?
	  (let loop ([ids super-field-ids])
	    (unless (null? ids)
	      (hash-table-put! field-ht (car ids) (hash-table-get super-field-ht (car ids)))
	      (loop (cdr ids)))))

	;; Put new ids in table, with pos (replace field pos with accessor info later)
	(unless no-new-methods?
	  (let loop ([ids public-names][p (class-method-width super)])
	    (unless (null? ids)
	      (when (hash-table-get method-ht (car ids) (lambda () #f))
		(obj-error 'class*/names "superclass already contains method: ~a~a" 
			   (car ids)
			   (for-class name)))
	      (hash-table-put! method-ht (car ids) p)
	      (loop (cdr ids) (add1 p)))))
	(unless no-new-fields?
	  (let loop ([ids public-field-names][p (class-field-width super)])
	    (unless (null? ids)
	      (when (hash-table-get field-ht (car ids) (lambda () #f))
		(obj-error 'class*/names "superclass already contains field: ~a~a" 
			   (car ids)
			   (for-class name)))
	      (hash-table-put! field-ht (car ids) p)
	      (loop (cdr ids) (add1 p)))))

	;; Check that superclass has expected fields
	(for-each (lambda (id)
		    (unless (hash-table-get field-ht id (lambda () #f))
		      (obj-error 'class*/names "superclass does not provide field: ~a~a" 
				 id
				 (for-class name))))
		  inherit-field-names)

	;; Check that superclass has expected methods, and get indices
	(let ([get-indices
	       (lambda (ids)
		 (map
		  (lambda (id)
		    (hash-table-get 
		     method-ht id
		     (lambda ()
		       (obj-error 'class*/names 
				  "superclass does not provide an expected method: ~a~a" 
				  id
				  (for-class name)))))
		  ids))]
	      [method-width (+ (class-method-width super) (length public-names))]
	      [field-width (+ (class-field-width super) num-fields)])
	  (let ([rename-indices (get-indices rename-names)]
		[inherit-indices (get-indices inherit-names)]
		[replace-final-indices (get-indices override-final-names)]
		[replace-normal-indices (get-indices override-normal-names)]
		[new-final-indices (get-indices public-final-names)]
		[new-normal-indices (get-indices public-normal-names)])

	    ;; -- Check that all interfaces are satisfied --
	    (for-each
	     (lambda (intf)
	       (for-each
		(lambda (var)
		  (unless (hash-table-get method-ht var (lambda () #f))
		    (obj-error 'class*/names 
			       "interface-required method missing: ~a~a~a" 
			       var
			       (for-class name)
			       (for-intf (interface-name intf)))))
		(interface-public-ids intf)))
	     interfaces)
	    (let ([c (get-implement-requirement interfaces 'class*/names (for-class name))])
	      (when (and c (not (subclass? super c)))
		(obj-error 'class*/names 
			   "interface-required implementation not satisfied~a~a"
			   (for-class name)
			   (let ([r (class-name c)])
			     (if r
				 (format " required class: ~a" r)
				 "")))))

	    ;; ---- Make the class and its interface ----
	    (let* ([class-make (if name
				   (make-naming-constructor 
				    struct:class 
				    (string->symbol (format "class:~a" name)))
				   make-class)]
		   [interface-make (if name
				       (make-naming-constructor 
					struct:interface
					(string->symbol (format "interface:~a" name)))
				       make-interface)]
		   [method-names (append (reverse public-names) super-method-ids)]
		   [field-names (append public-field-names super-field-ids)]
		   [super-interfaces (cons (class->interface super) interfaces)]
		   [i (interface-make name super-interfaces method-names #f)]
		   [methods (if no-method-changes?
				(class-methods super)
				(make-vector method-width))]
		   [meth-flags (if no-method-changes?
				   (class-meth-flags super)
				   (make-vector method-width))]
		   [c (class-make name
				  (add1 (class-pos super))
				  (list->vector (append (vector->list (class-supers super)) (list #f)))
				  i
				  method-width method-ht method-names
				  methods meth-flags
				  field-width field-ht field-names
				  'struct:object 'object? 'make-object 'field-ref 'field-set!
				  init-args
				  init-mode
				  'init
				  (and make-struct:prim #t))]
		   [obj-name (if name
				 (string->symbol (format "object:~a" name))
				 'object)]
		   ;; Used only for prim classes
		   [dispatcher (lambda (obj box)
				 (when (symbol? (unbox box))
				   ;; Map symbol to number:
				   (set-box! box (hash-table-get method-ht (unbox box))))
				 (let ([c (object-ref obj)]
				       [n (unbox box)])
				   (if (vector-ref (class-meth-flags c) n)
				       (vector-ref (class-methods c) n)
				       #f)))])
	      (vector-set! (class-supers c) (add1 (class-pos super)) c)

	      ;; --- Make the new object struct ---
	      (let*-values ([(prim-tagged-object-make prim-object? struct:prim-object)
			     (if make-struct:prim
				 (make-struct:prim c prop:object dispatcher)
				 (values #f #f #f))]
			    [(struct:object object-make object? object-field-ref object-field-set!)
			     (if make-struct:prim
				 ;; Use prim struct:
				 (values struct:prim-object #f prim-object? #f #f)
				 ;; Normal struct creation:
				 (make-struct-type obj-name
						   (class-struct:object super)
						   0 ;; No init fields
						   ;; Fields for new slots:
						   num-fields undefined
						   null
						   insp))]
			    ;; The second structure associates prop:object with the class.
			    ;; Other classes extend struct:object, so we can't put the
			    ;; property there.
			    [(struct:tagged-object tagged-object-make tagged-object? -ref -set!)
			     (if make-struct:prim
				 ;; Use prim struct:
				 (values #f prim-tagged-object-make #f #f #f)
				 ;; Normal second-struct creation:
				 (make-struct-type obj-name
						   struct:object
						   0 0 #f
						   ;; Map object property to class:
						   (list (cons prop:object c))
						   insp))])
		(set-class-struct:object! c struct:object)
		(set-class-object?! c object?)
		(set-class-make-object! c tagged-object-make)
		(unless (null? public-field-names)
		  ;; We need these only if there are new public fields
		  (set-class-field-ref! c object-field-ref)
		  (set-class-field-set!! c object-field-set!))

		;; --- Build field accessors and mutators ---
		;;  Use public field names to name the accessors and mutators
		(let-values ([(inh-accessors inh-mutators)
			      (values
			       (map (lambda (id) (make-class-field-accessor super id))
				    inherit-field-names)
			       (map (lambda (id) (make-class-field-mutator super id))
				    inherit-field-names))])
		  ;; -- Reset field table to register accessor and mutator info --
		  ;;  There are more accessors and mutators than public fields...
		  (let loop ([ids public-field-names][pos 0])
		    (unless (null? ids)
		      (hash-table-put! field-ht (car ids) (cons c pos))
		      (loop (cdr ids) (add1 pos))))

		  ;; -- Extract superclass methods ---
		  (let ([renames (map (lambda (index)
					(vector-ref (class-methods super) index))
				      rename-indices)])
		    ;; -- Create method accessors --
		    (let ([method-accessors (map (lambda (index)
						   (lambda (obj)
						     (vector-ref (class-methods (object-ref obj)) index)))
						 (append new-normal-indices
							 replace-normal-indices
							 inherit-indices))])
		      
		      ;; -- Get new methods and initializers --
		      (let-values ([(new-methods override-methods init)
				    (apply make-methods
					   object-field-ref
					   object-field-set!
					   (append inh-accessors
						   inh-mutators
						   renames
						   method-accessors))])
			;; -- Fill in method tables --
			;;  First copy old methods
			(unless no-method-changes?
			  (hash-table-for-each
			   (class-method-ht super)
			   (lambda (name index)
			     (vector-set! methods index (vector-ref (class-methods super) index))
			     (vector-set! meth-flags index (vector-ref (class-meth-flags super) index)))))
			;; Add new methods:
			(for-each (lambda (index method)
				    (vector-set! methods index method)
				    (vector-set! meth-flags index (not make-struct:prim)))
				  (append new-final-indices new-normal-indices)
				  new-methods)
			;; Override old methods:
			(for-each (lambda (index method id)
				    (when (eq? 'final (vector-ref meth-flags index))
				      (obj-error 'class*/names 
						 "cannot override final method: ~a~a"
						 id
						 (for-class name)))
				    (vector-set! methods index method)
				    (vector-set! meth-flags index (not make-struct:prim)))
				  (append replace-final-indices replace-normal-indices)
				  override-methods
				  override-names)
			;; Mark final methods:
			(for-each (lambda (id)
				    (vector-set! meth-flags (hash-table-get method-ht id) 'final))
				  final-names)

			;; --- Install initializer into class ---
			(set-class-init! c init)
				    
			;; -- result is the class ---
			c)))))))))))

  (define-values (prop:object object? object-ref) (make-struct-type-property 'object))

  ;;--------------------------------------------------------------------
  ;;  interfaces
  ;;--------------------------------------------------------------------
  
  ;; >> Simplistic implementation for now <<

  (define-syntax :interface
    (lambda (stx)
      (syntax-case stx ()
	[(_ (interface-expr ...) var ...)
	 (let ([vars (syntax->list (syntax (var ...)))]
	       [name (syntax-local-infer-name stx)])
	   (for-each
	    (lambda (v)
	      (unless (identifier? v)
		(raise-syntax-error #f
				    "not an identifier"
				    stx
				    v)))
	    vars)
	   (let ([dup (check-duplicate-identifier vars)])
	     (when dup
	       (raise-syntax-error #f
				   "duplicate name"
				   stx
				   dup)))
	   (with-syntax ([name (datum->syntax-object #f name #f)]
			 [(var ...) (map localize vars)])
	     (syntax/loc
	      stx
	      (compose-interface
	       'name
	       (list interface-expr ...)
	       `(var ...)))))])))

  (define-struct interface (name supers public-ids class) insp)

  (define (compose-interface name supers vars)
    (for-each
     (lambda (intf)
       (unless (interface? intf)
	 (obj-error 'interface 
		    "superinterface expression returned a non-interface: ~a~a" 
		    intf
		    (for-intf name))))
     supers)
    (let ([ht (make-hash-table)])
      (for-each
       (lambda (var)
	 (hash-table-put! ht var #t))
       vars)
      ;; Check that vars don't already exist in supers:
      (for-each
       (lambda (super)
	 (for-each
	  (lambda (var)
	    (when (hash-table-get ht var (lambda () #f))
	      (obj-error 'interface "variable already in superinterface: ~a~a~a" 
			 var
			 (for-intf name)
			 (let ([r (interface-name super)])
			   (if r
			       (format " already in: ~a" r)
			       "")))))
	  (interface-public-ids super)))
       supers)
      ;; Check for [conflicting] implementation requirements
      (let ([class (get-implement-requirement supers 'interface (for-intf name))]
	    [interface-make (if name
				(make-naming-constructor 
				 struct:interface
				 (string->symbol (format "interface:~a" name)))
				make-interface)])
	;; Add supervars to table:
	(for-each
	 (lambda (super)
	   (for-each
	    (lambda (var) (hash-table-put! ht var #t))
	    (interface-public-ids super)))
	 supers)
	;; Done
	(interface-make name supers (hash-table-map ht (lambda (k v) k)) class))))

  (define (get-implement-requirement interfaces where for)
    (let loop ([class #f]
	       [supers interfaces])
      (if (null? supers)
	  class
	  (let ([c (interface-class (car supers))])
	    (loop
	     (cond
	      [(not c) class]
	      [(not class) c]
	      [(subclass? c class) class]
	      [(subclass? class c) c]
	      [else
	       (obj-error 
		where
		"conflicting class implementation requirements in superinterfaces~a"
		for)])
	     (cdr supers))))))

  ;;--------------------------------------------------------------------
  ;;  object%
  ;;--------------------------------------------------------------------
  
  (define (make-naming-constructor type name)
    (let-values ([(struct: make- ? -accessor -mutator)
		  (make-struct-type name type 0 0 #f null insp)])
      make-))
  
  (define object<%> ((make-naming-constructor struct:interface 'interface:object%)
		     'object% null null #f))
  (define object% ((make-naming-constructor struct:class 'class:object%)
		   'object%
		   0 (vector #f) 
		   object<%>

		   0 (make-hash-table) null
		   (vector) (vector)
		   
		   0 (make-hash-table) null
		   
		   'struct:object object? 'make-object
		   'field-ref-not-needed 'field-set!-not-needed

		   null
		   'normal

		   (lambda (this super-init si_c si_inited? si_leftovers args) 
		     (unless (null? args)
		       (unused-args-error this args))
		     (void))

		   #t)) ; no super-init

  (vector-set! (class-supers object%) 0 object%)
  (let*-values ([(struct:obj make-obj obj? -get -set!)
		 (make-struct-type 'object #f 0 0 #f null insp)]
		[(struct:tagged-obj make-tagged-obj tagged-obj? -get -set!)
		 (make-struct-type 'object struct:obj 0 0 #f (list (cons prop:object object%)) insp)])
    (set-class-struct:object! object% struct:obj)
    (set-class-make-object! object% make-tagged-obj))
  (set-class-object?! object% object?) ; don't use struct pred; it wouldn't work with prim classes

  (set-interface-class! object<%> object%)

  ;;--------------------------------------------------------------------
  ;;  instantiation
  ;;--------------------------------------------------------------------
  
  (define make-object 
    (lambda (class . args)
      (do-make-object class args null)))
  
  (define-syntax instantiate
    (lambda (stx)
      (syntax-case stx ()
	[(form class (arg ...) . x)
	 (with-syntax ([orig-stx stx])
	   (syntax/loc stx (-instantiate do-make-object orig-stx (class) (list arg ...) . x)))])))

  ;; Helper; used by instantiate and super-instantiate
  (define-syntax -instantiate
    (lambda (stx)
      (syntax-case stx ()
	[(_ do-make-object orig-stx (maker-arg ...) args (kw arg) ...)
	 (andmap identifier? (syntax->list (syntax (kw ...))))
	 (with-syntax ([(kw ...) (map localize (syntax->list (syntax (kw ...))))])
	   (syntax/loc stx
	     (do-make-object maker-arg ...
			     args
			     (list (cons `kw arg)
				   ...))))]
	[(_ super-make-object orig-stx (make-arg ...) args kwarg ...)
	 ;; some kwarg must be bad:
	 (for-each (lambda (kwarg)
		     (syntax-case kwarg ()
		       [(kw arg)
			(identifier? (syntax kw))
			'ok]
		       [(kw arg)
			(raise-syntax-error
			 #f
			 "by-name argument does not start with an identifier"
			 (syntax orig-stx)
			 kwarg)]
		       [_else
			(raise-syntax-error
			 #f
			 "ill-formed by-name argument"
			 (syntax orig-stx)
			 kwarg)]))
		   (syntax->list (syntax (kwarg ...))))])))

  (define (do-make-object class by-pos-args named-args)
    (unless (class? class)
      (raise-type-error 'instantiate "class" class))
    (let ([o ((class-make-object class))])
      ;; Initialize it:
      (continue-make-object o class by-pos-args named-args #t)
      o))

  (define (continue-make-object o c by-pos-args named-args explict-named-args?)
    (let ([by-pos-only? (not (class-init-args c))])
      ;; Primitive class with by-pos arguments?
      (when by-pos-only?
	(unless (null? named-args)
	  (if explict-named-args?
	      (obj-error 
	       'instantiate
	       "class has only by-position initializers, but given by-name arguments:~a~a" 
	       (make-named-arg-string named-args)
	       (for-class (class-name c)))
	      ;; If args were implicit from subclass, should report as unused:
	      (unused-args-error o named-args))))
      ;; Merge by-pos into named args:
      (let* ([named-args (if (not by-pos-only?)
			     ;; Normal merge
			     (do-merge by-pos-args (class-init-args c) c named-args by-pos-args c)
			     ;; Non-merge for by-position initializers:
			     by-pos-args)]
	     [leftovers (if (not by-pos-only?)
			    (get-leftovers named-args (class-init-args c))
			    null)])
	;; In 'list mode, make sure no by-name arguments are left over
	(when (eq? 'list (class-init-mode c))
	  (unless (or (null? leftovers)
		      (not (ormap car leftovers)))
	    (unused-args-error o (filter car leftovers))))
	(unless (and (eq? c object%)
		     (null? named-args))
	  (let ([inited? (box (class-no-super-init? c))])
	    ;; ----- Execute the class body -----
	    ((class-init c)
	     o 
	     continue-make-super
	     c inited? leftovers ; merely passed through to continue-make-super
	     named-args)
	    (unless (unbox inited?)
	      (obj-error 'instantiate "superclass initialization not invoked by initialization~a"
			 (for-class (class-name c)))))))))

  (define (continue-make-super o c inited? leftovers by-pos-args new-named-args)
    (when (unbox inited?)
      (obj-error 'instantiate "superclass already initialized by class initialization~a"
		 (for-class (class-name c))))
    (set-box! inited? #t)
    (let ([named-args (if (eq? 'list (class-init-mode c))
			  ;; all old args must have been used up
			  new-named-args
			  ;; Normal mode: merge leftover keyword-based args with new ones
			  (append
			   new-named-args
			   leftovers))])
      (continue-make-object o
			    (vector-ref (class-supers c) (sub1 (class-pos c))) 
			    by-pos-args 
			    named-args
			    (pair? new-named-args))))

  (define (do-merge al nl ic named-args by-pos-args c)
    (cond
     [(null? al) named-args]
     [(null? nl)
      ;; continue mapping with superclass init args, if allowed
      (let ([super (and (eq? 'normal (class-init-mode ic))
			(positive? (class-pos ic))
			(vector-ref (class-supers ic) (sub1 (class-pos ic))))])
	(cond
	 [(and super (class-init-args super))
	  (do-merge al (class-init-args super) super named-args by-pos-args c)]
	 [(eq? 'list (class-init-mode ic))
	  ;; All unconsumed named-args must have #f
	  ;;  "name"s, otherwise an error is raised in
	  ;;  the leftovers checking.
	  (append (map (lambda (x) (cons #f x)) al)
		  named-args)]
	 [else
	  (obj-error 'instantiate 
		     "too many initialization arguments:~a~a" 
		     (make-pos-arg-string by-pos-args)
		     (for-class (class-name c)))]))]
     [else (cons (cons (car nl) (car al))
		 (do-merge (cdr al) (cdr nl) ic named-args by-pos-args c))]))

  (define (get-leftovers l names)
    (cond
     [(null? l) null]
     [(memq (caar l) names)
      (get-leftovers (cdr l) (remq (caar l) names))]
     [else (cons (car l) (get-leftovers (cdr l) names))]))

  (define (extract-arg class-name name arguments default)
    (if (symbol? name)
	;; Normal mode
	(let ([a (assq name arguments)])
	  (cond
	   [a (cdr a)]
	   [default (default)]
	   [else (missing-argument-error class-name name)]))
	;; By-position mode
	(cond
	 [(< name (length arguments))
	  (cdr (list-ref arguments name))]
	 [default (default)]
	 [else (obj-error 'instantiate "too few initialization arguments")])))

  (define (extract-rest-args skip arguments)
    (if (< skip (length arguments))
	(map cdr (list-tail arguments skip))
	null))

  (define (make-pos-arg-string args)
    (let ([len (length args)])
      (apply string-append
	     (map (lambda (a)
		    (format " ~e" a))
		  args))))

  (define (make-named-arg-string args)
    (let loop ([args args][count 0])
      (cond
       [(null? args) ""]
       [(= count 3) " ..."]
       [else (let ([rest (loop (cdr args) (add1 count))])
	       (format " (~a ~e)~a"
		       (caar args)
		       (cdar args)
		       rest))])))

  (define (unused-args-error this args)
    (let ([arg-string (make-named-arg-string args)])
      (obj-error 'instantiate "unused initialization arguments:~a~a" 
		 arg-string
		 (for-class/which "instantiated" (class-name (object-ref this))))))

  (define (missing-argument-error class-name name)
    (obj-error 'instantiate "no argument for required init variable: ~a~a"
	       name
	       (if class-name (format " in class: ~a" class-name) "")))

  ;;--------------------------------------------------------------------
  ;;  methods and fields
  ;;--------------------------------------------------------------------
  
  (define-syntaxes (send send/apply)
    (let ([mk
	   (lambda (flatten?)
	     (lambda (stx)
	       (syntax-case stx ()
		 [(_ obj name . args)
		  (begin
		    (unless (identifier? (syntax name))
		      (raise-syntax-error
		       #f
		       "method name is not an identifier"
		       stx
		       (syntax name)))
		    (with-syntax ([name (localize (syntax name))])
		      (if flatten?
			  (if (stx-list? (syntax args))
			      (syntax (let ([this obj])
					(apply (find-method this `name) this . args)))
			      (raise-syntax-error
			       #f
			       "bad syntax (illegal use of `.')"
			       stx))
			  (if (stx-list? (syntax args))
			      (with-syntax ([call (syntax/loc stx
						    ((find-method this `name) this . args))])
				(syntax/loc stx (let ([this obj])
						  call)))
			      (with-syntax ([args (flatten-args (syntax args))])
				(with-syntax ([call (syntax/loc stx
						      (apply (find-method this `name) this . args))])
				  (syntax/loc stx (let ([this obj])
						    call))))))))])))])
      (values (mk #f) (mk #t))))
  
  (define-syntax send*
    (lambda (stx)
      (syntax-case stx ()
	[(_ obj s ...)
	 (with-syntax ([sends (map (lambda (s)
				     (syntax-case s ()
				       [(meth . args)
					(syntax/loc s (send o meth . args))]
				       [_else (raise-syntax-error
					       #f
					       "bad method call"
					       stx
					       s)]))
				   (syntax->list (syntax (s ...))))])
	   (syntax/loc stx
	     (let ([o obj])
	       . sends)))])))
    
  (define (find-method/who who object name)
    (unless (object? object)
      (obj-error who "target is not an object: ~e for method: ~a"
		 object name))
    (let* ([c (object-ref object)]
	   [pos (hash-table-get (class-method-ht c) name (lambda () #f))])
      (if pos
	  (vector-ref (class-methods c) pos)
	  (obj-error who "no such method: ~a~a"
		     name
		     (for-class (class-name c))))))

  (define (find-method object name)
    (find-method/who 'send object name))

  (define (class-field-X who which cwhich class name)
    (unless (class? class)
      (raise-type-error who "class" class))
    (unless (symbol? name)
      (raise-type-error who "symbol" name))
    (let ([p (hash-table-get (class-field-ht class) name
			     (lambda ()
			       (obj-error who "no such field: ~a~a"
					  name
					  (for-class (class-name class)))))])
      (which (cwhich (car p)) (cdr p))))
  
  (define (make-class-field-accessor class name)
    (class-field-X 'class-field-accessor
		   make-struct-field-accessor class-field-ref
		   class name))
  
  (define (make-class-field-mutator class name)
    (class-field-X 'class-field-mutator 
		   make-struct-field-mutator class-field-set!
		   class name))

  (define-struct generic (applicable))

  ;; Internally, make-generic comes from the struct def.
  ;; Externally, make-generic is the following procedure.
  ;; The extra `let' gives it the right name.
  (define make-generic/proc
    (let ([make-generic
	   (lambda (class name)
	     (unless (or (class? class) (interface? class))
	       (raise-type-error 'make-generic "class or interface" class))
	     (unless (symbol? name)
	       (raise-type-error 'make-generic "symbol" name))
	     (make-generic
	      (if (interface? class)
		  (let ([intf class])
		    (unless (method-in-interface? name intf)
		      (obj-error 'make-generic "no such method: ~a~a"
				 name
				 (for-intf (interface-name intf))))
		    (lambda (obj)
		      (unless (is-a? obj intf)
			(raise-type-error 
			 (string->symbol (format "generic:~a~a" name (for-intf (interface-name intf))))
			 (format "instance~a" (for-intf (interface-name intf)))
			 obj))
		      (find-method obj name)))
		  (let ([pos (hash-table-get (class-method-ht class) name
					     (lambda ()
					       (obj-error 'make-generic "no such method: ~a~a"
							  name
							  (for-class (class-name class)))))])
		    (lambda (obj)
		      (unless ((class-object? class) obj)
			(raise-type-error 
			 (symbol->string (format "generic:~a~a" name (for-class (class-name class))))
			 (format "instance~a" (for-class (class-name class)))
			 obj))
		      (vector-ref (class-methods (object-ref obj)) pos))))))])
      make-generic))

  (define-syntax send-generic
    (lambda (stx)
      (syntax-case stx ()
	[(_ obj generic . args)
	 (if (stx-list? (syntax args))
	     (with-syntax ([call (syntax/loc stx
				   (((generic-applicable generic) this) this . args))])
	       (syntax/loc stx (let ([this obj])
				 call)))
	     (with-syntax ([args (flatten-args (syntax args))])
	       (with-syntax ([call (syntax/loc stx
				     (apply ((generic-applicable generic) this) this . args))])
		 (syntax (let ([this obj])
			   call)))))])))
  
  (define-syntaxes (class-field-accessor class-field-mutator generic/form)
    (let ([mk
	   (lambda (make targets)
	     (lambda (stx)
	       (syntax-case stx ()
		 [(_ class-expr name)
		  (let ([name (syntax name)])
		    (unless (identifier? name)
		      (raise-syntax-error
		       #f
		       "expected an indentifier"
		       stx
		       name))
		    (with-syntax ([name (localize name)]
				  [make make])
		      (syntax/loc stx (make class-expr `name))))]
		 [(_ class-expr)
		  (raise-syntax-error
		   #f
		   (format "expected a field name after the ~a expression"
			   targets)
		   stx)])))])
      (values
       (mk (quote-syntax make-class-field-accessor) "class")
       (mk (quote-syntax make-class-field-mutator) "class")
       (mk (quote-syntax make-generic/proc) "class or interface"))))

  (define (find-with-method object name)
    (find-method/who 'with-method object name))

  (define-syntax with-method
    (lambda (stx)
      (syntax-case stx ()
	[(_ ([id (obj-expr name)] ...) body0 body1 ...)
	 (let ([ids (syntax->list (syntax (id ...)))]
	       [names (syntax->list (syntax (name ...)))])
	   (for-each (lambda (id name)
		       (unless (identifier? id)
			 (raise-syntax-error #f
					     "not an identifier for binding"
					     stx
					     id))
		       (unless (identifier? name)
			 (raise-syntax-error #f
					     "not an identifier for method name"
					     stx
					     name)))
		     ids names)
	   (with-syntax ([(method ...) (generate-temporaries ids)]
			 [(method-obj ...) (generate-temporaries ids)]
			 [(name ...) (map localize names)])
	     (syntax/loc stx (let-values ([(method method-obj)
					   (let ([obj obj-expr])
					     (values (find-with-method obj `name) obj))]
					  ...)
			       (letrec-syntaxes+values ([(id) (make-with-method-map
							       (quote-syntax set!)
							       (quote-syntax id)
							       (quote-syntax method)
							       (quote-syntax method-obj))]
							...)
				   ()
				 body0 body1 ...)))))]
	;; Error cases:
	[(_ (clause ...) . body)
	 (begin
	   (for-each (lambda (clause)
		       (syntax-case clause ()
			 [(id (obj-expr name))
			  (and (identifier? (syntax id))
			       (identifier? (syntax name)))
			  'ok]
			 [_else
			  (raise-syntax-error 
			   #f
			   "binding clause is not of the form (identifier (object-expr method-identifier))"
			   stx
			   clause)]))
		     (syntax->list (syntax (clause ...))))
	   ;; If we get here, the body must be bad
	   (if (stx-null? (syntax body))
	       (raise-syntax-error 
		#f
		"empty body"
		stx)
	       (raise-syntax-error 
		#f
		"bad syntax (illegal use of `.')"
		stx)))]
	[(_ x . rest)
	 (raise-syntax-error 
	  #f
	  "not a binding sequence"
	  stx
	  (syntax x))])))

  ;;--------------------------------------------------------------------
  ;;  class, interface, and object properties
  ;;--------------------------------------------------------------------
  
  (define (is-a? v c)
    (cond
     [(class? c) ((class-object? c) v)]
     [(interface? c)
      (and (object? v)
	   (implementation? (object-ref v) c))]
     [else (raise-type-error 'is-a? "class or interface" 1 v c)]))
  
  (define (subclass? v c)
    (unless (class? c)
      (raise-type-error 'subclass? "class" 1 v c))
    (and (class? v)
	 (let ([p (class-pos c)])
	   (and (<= p (class-pos v))
		(eq? c (vector-ref (class-supers v) p))))))

  (define (object-interface o) (class->interface (object-ref o)))
  
  (define (implementation? v i)
    (unless (interface? i)
      (raise-type-error 'implementation? "interface" 1 v i))
    (and (class? v)
	 (interface-extension? (class->interface v) i)))

  (define (interface-extension? v i)
    (unless (interface? i)
      (raise-type-error 'interface-extension? "interface" 1 v i))
    (and (interface? i)
	 (let loop ([v v])
	   (or (eq? v i)
	       (ormap loop (interface-supers v))))))
  
  (define (method-in-interface? s i)
    (unless (symbol? s)
      (raise-type-error 'method-in-interface? "symbol" 0 s i))
    (unless (interface? i)
      (raise-type-error 'method-in-interface? "interface" 1 s i))
    (and (memq s (interface-public-ids i)) #t))

  (define (interface->method-names i)
    (unless (interface? i)
      (raise-type-error 'interface->method-names "interface" i))
    ;; copy list, and also filter private (interned) methods:
    (let loop ([l (interface-public-ids i)])
      (cond
       [(null? l) null]
       [(eq? (car l) (string->symbol (symbol->string (car l)))) 
	;; interned
	(cons (car l) (loop (cdr l)))]
       [else 
	;; uninterned
	(loop (cdr l))])))

  ;;--------------------------------------------------------------------
  ;;  primitive classes
  ;;--------------------------------------------------------------------
  
  (define (make-primitive-class 
	   make-struct:prim     ; see below
	   prim-init            ; primitive initializer: takes obj and list of name-arg pairs
	   name                 ; symbol
	   super                ; superclass
	   init-arg-names       ; #f or list of syms and sym--value lists
	   override-names       ; overridden method names
	   new-names            ; new (public) method names
	   override-methods     ; list of methods
	   new-methods)         ; list of methods

    ; The `make-struct:prim' function takes prop:object, a
    ;  class, and a dispatcher function, and produces:
    ;    * a struct constructor (must have prop:object)
    ;    * a struct predicate
    ;    * a struct type for derived classes (mustn't have prop:object)
    ;
    ; The supplied dispatched takes an object and a boxed symbol/num
    ;  (converts a symbol to a num first time) and returns a method if the
    ;  corresponding method is overridden in the object's class relative to
    ;  the primitive class, #f otherwise.
    ;
    ; When a primitive class has a superclass, the struct:prim maker
    ;  is responsible for ensuring that the returned struct items match
    ;  the supertype predicate.

    (compose-class name
		   (or super object%)
		   null
		   
		   0 null null ; no fields

		   null ; no renames
		   null new-names
		   null override-names
		   null ; no inherits
		   
		   ; #f => init args by position only
		   ; sym => required arg
		   ; sym--value list => optional arg
		   (and init-arg-names  
			(map (lambda (s)
			       (if (symbol? s) s (car s)))
			     init-arg-names))
		   'stop
		   
		   (lambda ignored
		     (values
		      new-methods
		      override-methods
		      (lambda (this super-id/ignored si_c/ignored si_inited?/ignored si_leftovers/ignored init-args)
			(apply prim-init this 
			       (if init-arg-names
				   (extract-primitive-args this name init-arg-names init-args)
				   init-args)))))
		   
		   make-struct:prim))

  (define (extract-primitive-args this class-name init-arg-names init-args)
    (let loop ([names init-arg-names][args init-args])
      (cond
       [(null? names)
	(unless (null? args)
	  (unused-args-error this args))
	null]
       [else (let* ([name (car names)]
		    [id (if (symbol? name)
			    name
			    (car name))])
	       (let ([arg (assq id args)])
		 (cond
		  [arg 
		   (cons (cdr arg) (loop (cdr names) (remq arg args)))]
		  [(symbol? name)
		   (missing-argument-error class-name name)]
		  [else
		   (cons (cadr name) (loop (cdr names) args))])))])))

  ;;--------------------------------------------------------------------
  ;;  misc utils
  ;;--------------------------------------------------------------------
  
  (define undefined (letrec ([x x]) x))
  
  (define-struct (exn:object exn) () insp)

  (define (obj-error where . msg)
    (raise
     (make-exn:object
      (string-append
       (format "~a: " where)
       (apply format msg))
      (current-continuation-marks))))

  (define (for-class name)
    (if name (format " for class: ~a" name) ""))
  (define (for-class/which which name)
    (if name (format " for ~a class: ~a" which name) ""))
  (define (for-intf name)
    (if name (format " for interface: ~a" name) ""))


  (provide (rename :class class)
	   class* class*/names class?
	   (rename :interface interface) interface?
	   object% object?
	   make-object instantiate
	   send send/apply send* class-field-accessor class-field-mutator with-method
	   private* public*  public-final* override* override-final*
	   define/private define/public define/public-final define/override define/override-final
	   define-local-member-name
	   (rename generic/form generic) (rename make-generic/proc make-generic) send-generic
	   is-a? subclass? implementation? interface-extension?
	   object-interface
	   method-in-interface? interface->method-names class->interface
	   exn:object? struct:exn:object make-exn:object
	   make-primitive-class))

