; Time-stamp: <97/08/19 15:07:32 shriram>
; Time-stamp: <97/07/12 12:44:01 shriram>

; Differences from the Chez implementation:

; - The code does not respect tail-calls.
; - If the library is loaded more than once, especially in the middle
;   of a trace, the behavior is not well-defined.

(module trace mzscheme
  (require "pretty.ss")

  (provide trace untrace)

  (define max-dash-space-depth 10)
  (define number-nesting-depth 6)

  (define as-spaces
    (lambda (s)
      (let ((n (string-length s)))
	(apply string-append
	       (let loop ((k n))
		 (if (zero? k) '("")
		     (cons " " (loop (sub1 k)))))))))

  (define-struct prefix-entry (for-first for-rest))

  (define prefixes (make-vector 20 #f))
  (define prefix-vector-length 20)

  (define lookup-prefix
    (lambda (n)
      (and (< n prefix-vector-length)
	   (vector-ref prefixes n))))

  (define insert-prefix
    (lambda (n first rest)
      (if (>= n prefix-vector-length)
	  (let ((v (make-vector (* 2 prefix-vector-length) #f)))
	    (let loop ((k 0))
	      (when (< k prefix-vector-length)
		(vector-set! v k (vector-ref prefixes k))
		(loop (add1 k))))
	    (set! prefixes v)
	    (set! prefix-vector-length (* 2 prefix-vector-length))
	    (insert-prefix n first rest))
	  (vector-set! prefixes n (make-prefix-entry first rest)))))

  (define construct-prefixes
    (lambda (level)
      (let loop ((n level)
		 (first '("|"))
		 (rest '(" ")))
	(if (>= n max-dash-space-depth)
	    (let-values (((pre-first pre-rest)
			  (build-prefixes number-nesting-depth)))
	      (let ((s (number->string level)))
		(values
		 (apply string-append
			(cons pre-first (cons "[" (cons s (cons "]" '())))))
		 (apply string-append
			(cons pre-rest (cons " " (cons (as-spaces s)
						       (cons " " '()))))))))
	    (cond
	     ((= n 0) (values (apply string-append (reverse first))
			      (apply string-append (reverse rest))))
	     ((= n 1) (loop (- n 1)
			    (cons '" " first)
			    (cons '" " rest)))
	     (else (loop (- n 2)
			 (cons " |" first)
			 (cons "  " rest))))))))

  (define build-prefixes
    (lambda (level)
      (let ((p (lookup-prefix level)))
	(if p
	    (values (prefix-entry-for-first p)
		    (prefix-entry-for-rest p))
	    (let-values (((first rest)
			  (construct-prefixes level)))
	      (insert-prefix level first rest)
	      (values first rest))))))

  (define -:trace-level (make-parameter -1))

  (define -:trace-print-args
    (lambda (name args)
      (let-values (((first rest)
		    (build-prefixes (-:trace-level))))
	(parameterize ((pretty-print-print-line
			(lambda (n port offset width)
			  (display
			   (if n
			       (if (zero? n) first
				   (format "~n~a" rest))
			       (format "~n"))
			   port)
			  (if n
			      (if (zero? n)
				  (string-length first)
				  (string-length rest))
			      0))))
	  (pretty-print (cons name args))))))

  (define -:trace-print-results
    (lambda (name results)
      (let-values (((first rest)
		    (build-prefixes (-:trace-level))))
	(parameterize ((pretty-print-print-line
			(lambda (n port offset width)
			  (display
			   (if n
			       (if (zero? n) first
				   (format "~n~a" rest))
			       (format "~n"))
			   port)
			  (if n
			      (if (zero? n)
				  (string-length first)
				  (string-length rest))
			      0))))
	  (cond
	   ((null? results)
	    (pretty-display "*** no values ***"))
	   ((null? (cdr results))
	    (pretty-print (car results)))
	   (else
	    (pretty-print (car results))
	    (parameterize ((pretty-print-print-line
			    (lambda (n port offset width)
			      (display
			       (if n
				   (if (zero? n) rest
				       (format "~n~a" rest))
				   (format "~n"))
			       port)
			      (if n
				  (string-length rest)
				  0))))
	      (for-each pretty-print (cdr results)))))))))

  (define-struct traced-entry (original-proc trace-proc))

  (define -:make-traced-entry make-traced-entry)
  (define -:traced-entry-original-proc traced-entry-original-proc)
  (define -:traced-entry-trace-proc traced-entry-trace-proc)

  (define -:trace-table
    (make-hash-table))

  (define-syntax trace
    (lambda (stx)
      (syntax-case stx ()
	[(_ id ...)
	 (let ([ids (syntax->list (syntax (id ...)))])
	   (for-each (lambda (id)
		       (unless (identifier? id)
			 (raise-syntax-error
			  #f
			  "not an identifier"
			  stx
			  id)))
		     ids)
	   (with-syntax ([(traced-name ...)
			  (map (lambda (id)
				 (datum->syntax-object
				  id
				  (string->symbol
				   (string-append "traced-"
						  (symbol->string (syntax-e id))))
				  #f))
			       ids)])
	     (syntax
	      (begin
		(let ((global (namespace-variable-value 
			       'id #t 
			       (lambda () (error 'trace "~s is not bound as a procedure" 'id)))))
		  (unless (procedure? global)
		    (error 'trace
			   "the top-level value of ~s is not a procedure" 'id)))
		...

		(let ((global-value (namespace-variable-value 'id)))
		  (let ((table-entry (hash-table-get -:trace-table 'id (lambda () #f))))
		    (unless (and table-entry
				 (eq? global-value
				      (-:traced-entry-trace-proc table-entry)))
		      (let* ((real-value global-value)
			     (traced-name
			      (lambda args
				(dynamic-wind
				 (lambda ()
				   (-:trace-level
				    (add1 (-:trace-level))))
				 (lambda ()
				   (-:trace-print-args 'id args)
				   (call-with-values
				    (lambda ()
				      (apply real-value args))
				    (lambda results
				      (flush-output)
				      (-:trace-print-results 'id results)
				      (apply values results))))
				 (lambda ()
				   (-:trace-level
				    (sub1 (-:trace-level))))))))
			(hash-table-put! -:trace-table 'id
					 (-:make-traced-entry real-value traced-name))
			(namespace-set-variable-value! 'id traced-name #f)))))
		...
		'(id ...)))))])))

  (define-syntax untrace
    (lambda (stx)
      (syntax-case stx ()
	[(_ id ...)
	 (let ([ids (syntax->list (syntax (id ...)))])
	   (for-each (lambda (id)
		       (unless (identifier? id)
			 (raise-syntax-error
			  #f
			  "not an identifier"
			  stx
			  id)))
		     ids)
	   (syntax
	    (apply append
		   (list
		    (let ((entry (hash-table-get -:trace-table
						 'id (lambda () #f))))
		      (if (and entry
			       (eq? (namespace-variable-value 'id #f)
				    (-:traced-entry-trace-proc entry)))
			  (begin
			    (hash-table-put! -:trace-table 'id #f)
			    (namespace-set-variable-value! 'id (-:traced-entry-original-proc entry) #f)
			    (list 'id))
			  '()))
		    ...))))])))

  )

