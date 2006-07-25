
(module include mzscheme
  (require-for-syntax (lib "stx.ss" "syntax")
		      "private/increader.ss")
  (require (lib "etc.ss"))

  (define-syntax-set (do-include ; private
		      include-at/relative-to
		      include
		      include-at/relative-to/reader
		      include/reader)

    (define (do-include/proc stx)
      (syntax-case stx ()
	[(_ orig-stx ctx loc fn reader)
	 ;; Parse the file name
	 (let ([file
		(syntax-case* (syntax fn) (build-path) module-or-top-identifier=?
		  [fn
		   (string? (syntax-e (syntax fn)))
		   (syntax-e (syntax fn))]
		  [(build-path elem1 elem ...)
		   (apply build-path (syntax-object->datum (syntax (elem1 elem ...))))])]
	       [ctx (syntax ctx)]
	       [loc (syntax loc)]
	       [reader (syntax reader)]
	       [orig-stx (syntax orig-stx)])
	   
	   (let ([read-syntax (if (syntax-e reader)
                                  (reader-val
                                   (let loop ([e (syntax-object->datum
                                                  (local-expand reader 'expression null))])
                                     (cond
                                       [(reader? e) e]
                                       [(pair? e) (or (loop (car e))
                                                      (loop (cdr e)))]
                                       [else #f])))
				  read-syntax)])
	     (unless (and (procedure? read-syntax)
			  (procedure-arity-includes? read-syntax 2))
	       (raise-syntax-error
		#f
		"reader is not a procedure of two arguments"
		orig-stx))

	     ;; Complete the file name
	     (let ([c-file
		    (if (complete-path? file)
			file
			(path->complete-path
			 file
			 (cond
			  ;; Src of include expression is a path?
			  [(and (string? (syntax-source loc))
				(complete-path? (syntax-source loc)))
			   (let-values ([(base name dir?) 
					 (split-path (syntax-source loc))])
			     (if dir?
				 (syntax-source loc)
				 base))]
			  ;; Load relative?
			  [(current-load-relative-directory)]
			  ;; Current directory
			  [(current-directory)]
			  [else (raise-syntax-error
				 #f
				 "can't determine a base path"
				 orig-stx)])))])
	       ;; Open the included file
	       (let ([p (with-handlers ([not-break-exn?
					 (lambda (exn)
					   (raise-syntax-error
					    #f
					    (format
					     "can't open include file (~a)"
					     (if (exn? exn)
						 (exn-message exn)
						 exn))
					    orig-stx
					    c-file))])
			  (open-input-file c-file))])
		 (port-count-lines! p)
		 ;; Read expressions from file
		 (let ([content
			(let loop ()
			  (let ([r (with-handlers ([not-break-exn?
						    (lambda (exn)
						      (raise-syntax-error
						       #f
						       (format
							"read error (~a)"
							(if (exn? exn)
							    (exn-message exn)
							    exn))
						       orig-stx))])
				     (read-syntax c-file p))])
			    (if (eof-object? r)
				null
				(cons r (loop)))))])
		   ;; Preserve src info for content, but set its
		   ;; lexical context to be that of the include expression
		   (let ([lexed-content
			  (let loop ([content content])
			    (cond
			     [(pair? content)
			      (cons (loop (car content))
				    (loop (cdr content)))]
			     [(null? content) null]
			     [else
			      (let ([v (syntax-e content)])
				(datum->syntax-object
				 ctx
				 (cond
				  [(pair? v) 
				   (loop v)]
				  [(vector? v)
				   (list->vector (loop (vector->list v)))]
				  [(box? v)
				   (box (loop (unbox v)))]
				  [else
				   v])
				 content))]))])
		     (datum->syntax-object
		      (quote-syntax here)
		      `(begin ,@lexed-content)
		      orig-stx)))))))]))
    
    (define (check-fn-form fn stx)
      ;; Check form of fn:
      (syntax-case* fn (build-path) module-or-top-identifier=?
	[fn
	 (string? (syntax-e (syntax fn)))
	 'ok]
	[(build-path elem1 elem ...)
	 (andmap
	  (lambda (e)
	    (or (string? (syntax-e e))
		(and (identifier? e)
		     (or
		      (module-identifier=? e (quote-syntax up))
		      (module-identifier=? e (quote-syntax same))))))
	  (syntax->list (syntax (elem1 elem ...))))
	 'ok]
	[_else (raise-syntax-error #f "bad syntax" stx fn)]))

    (define (include/proc stx)
      (syntax-case stx ()
	[(_ fn)
	 (check-fn-form (syntax fn) stx)
	 (with-syntax ([_stx stx])
	   (syntax/loc stx (do-include _stx _stx _stx fn #f)))]))

    (define (include-at/relative-to/proc stx)
      (syntax-case stx ()
	[(_ ctx loc fn)
	 (check-fn-form (syntax fn) stx)
	 (with-syntax ([_stx stx])
	   (syntax/loc stx (do-include _stx ctx loc fn #f)))]))
      
    (define (include/reader/proc stx)
      (syntax-case stx ()
	[(_ fn reader)
	 (check-fn-form (syntax fn) stx)
	 ;; Expand to do-include:
	 (with-syntax ([_stx stx])
	   (syntax/loc stx 
	     (do-include _stx _stx _stx fn 
			 (letrec-syntax ([the-reader (lambda (stx)
						       (datum->syntax-object
							#'here
							(make-reader reader)))])
			   the-reader))))]))
    
    (define (include-at/relative-to/reader/proc stx)
      (syntax-case stx ()
	[(_ ctx loc fn reader)
	 (check-fn-form (syntax fn) stx)
	 (with-syntax ([_stx stx])
	   (syntax/loc stx 
	     (do-include _stx ctx loc fn 
			 (letrec-syntax ([the-reader (lambda (stx)
						       (datum->syntax-object
							#'here
							(make-reader reader)))])
			   the-reader))))])))
  
  (provide include
	   include-at/relative-to
	   include/reader
	   include-at/relative-to/reader))

		 
			   
	      
		     
