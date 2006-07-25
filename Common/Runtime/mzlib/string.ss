
(module string mzscheme
  (provide string-lowercase!
	   string-uppercase!
	   eval-string
	   read-from-string
	   read-from-string-all
	   expr->string
	   regexp-quote
	   regexp-replace-quote
	   regexp-match*
	   regexp-match-positions*
	   regexp-split
	   regexp-match-exact?)

  (require (lib "etc.ss"))

  (define make-string-do!
    (lambda (translate)
      (lambda (s)
	(let loop ([n (sub1 (string-length s))])
	  (unless (negative? n)
	    (string-set! s n
			 (translate (string-ref s n)))
	    (loop (sub1 n)))))))
  (define string-lowercase! (make-string-do! char-downcase))
  (define string-uppercase! (make-string-do! char-upcase))

  (define eval-string
    (let ([do-eval
	   (lambda (str)
	     (let ([p (open-input-string str)])
	       (apply
		values
		(let loop ()
		  (let ([e (read p)])
		    (if (eof-object? e)
			'()
			(call-with-values
			    (lambda () (eval e))
			  (case-lambda
			   [() (loop)]
			   [(only) (cons only (loop))]
			   [multi 
			    (append multi (loop))]))))))))])
      (case-lambda
       [(str) (eval-string str #f #f)]
       [(str error-display) (eval-string str error-display #f)]
       [(str error-display error-result)
	(if (or error-display error-result)
	    (with-handlers ([void
			     (lambda (exn)
			       ((or error-display (lambda (x)
						    ((error-display-handler) x exn)))
				(exn-message exn))
			       (if error-result
				   (error-result)
				   #f))])
	      (do-eval str))
	    (do-eval str))])))

  (define read-from-string-one-or-all
    (case-lambda
     [(k all? str) (read-from-string-one-or-all k all? str #f #f)]
     [(k all? str error-display) (read-from-string-one-or-all k all? str error-display #f)]
     [(k all? str error-display error-result)
      (let* ([p (open-input-string str)]
	     [go (lambda ()
		   (let loop ()
		     (let ([v (read p)])
		       (if (eof-object? v)
			   '()
			   (cons v
				 (if all?
				     (loop)
				     '()))))))])
	(if error-display
	    (with-handlers ([void
			     (lambda (exn)
			       ((or error-display (lambda (x)
						    ((error-display-handler) x exn)))
				(exn-message exn))
			       (k (if error-result
				      (error-result)
				      #f)))])
	      (go))
	    (go)))]))

  (define read-from-string
    (lambda args
      (let/ec k
	(let ([l (apply read-from-string-one-or-all k #f args)])
	  (if (null? l)
	      eof
	      (car l))))))
  
  (define read-from-string-all
    (lambda args
      (let/ec k
	(apply read-from-string-one-or-all k #t args))))
  
  (define expr->string
    (lambda (v)
      (let ([port (open-output-string)])
	(write v port)
	(get-output-string port))))
  
  (define regexp-quote
    (opt-lambda (s [case-sens? #t])
      (unless (string? s)
	(raise-type-error 'regexp-quote "string" s))
      (list->string
       (apply
	append
	(map
	 (lambda (c)
	   (cond 
	    [(memq c '(#\$ #\| #\\ #\[ #\] #\. #\* #\? #\+ #\( #\) #\^))
	     (list #\\ c)]
	    [(and (char-alphabetic? c)
		  (not case-sens?))
	     (list #\[ (char-upcase c) (char-downcase c) #\])]
	    [else (list c)]))
	 (string->list s))))))

  (define (regexp-replace-quote s)
    (unless (string? s)
      (raise-type-error 'regexp-replace-quote "string" s))
    (regexp-replace* "\\\\" s "\\\\\\\\"))


  ;; Helper function for the regexp functions below.
  (define (regexp-fn name success failure)
    (opt-lambda (pattern string [start 0] [end (and (string? string)
						    (string-length string))])

      (unless (or (string? pattern) (regexp? pattern))
	(raise-type-error name "regexp or string" pattern))
      (unless (string? string)
	(raise-type-error name "string" string))
      (unless (and (number? start) (exact? start) (integer? start) (start . >= . 0))
	(raise-type-error name "non-negative exact integer" start))
      (unless (and (number? end) (exact? end) (integer? end) (end . >= . 0))
	(raise-type-error name "non-negative exact integer" end))
      (unless (start . <= . (string-length string))
	(raise-mismatch-error
	 name
	 (format "starting offset index ~a out of range [0,~a] for string: " 
		 start
		 (string-length string))
	 string))
      (unless (<= start end (string-length string))
	(raise-mismatch-error
	 name
	 (format "ending offset index ~a out of range [~a,~a] for string: " 
		 end
		 start
		 (string-length string))
	 string))

      (let* ((expr (if (regexp? pattern) 
		       pattern 
		       (regexp pattern)))
	     (match (regexp-match-positions expr string start end)))
	(if match
	    (let ((match-start (caar match))
		  (match-end (cdar match)))
	      (when (= match-start match-end)
		(error name "pattern matched a zero-length substring"))
	      (success expr string start end match-start match-end))
	    (failure expr string start end)))))

  ;; Returns all the positions at which the pattern matched.
  (define regexp-match-positions*
    (regexp-fn 'regexp-match-positions*
	       (lambda (expr string start end match-start match-end)
		 (cons (cons match-start match-end)
		       (regexp-match-positions* expr string match-end end)))
	       (lambda (expr string start end)
		 null)))

  ;; Splits a string into a list by removing any piece which matches
  ;; the pattern.
  (define regexp-split
    (regexp-fn 'regexp-split
	       (lambda (expr string start end match-start match-end)
		 (cons
		  (substring string start match-start)
		  (regexp-split expr string match-end end)))
	       (lambda (expr string start end)
		 (list
		  (substring string start end)))))

  ;; Returns all the matches for the pattern in the string.
  (define regexp-match*
    (regexp-fn 'regexp-match*
	       (lambda (expr string start end match-start match-end)
		 (cons
		  (substring string match-start match-end)
		  (regexp-match* expr string match-end end)))
	       (lambda args
		 (list))))

  (define regexp-match-exact?
    (lambda (p s)
      (let ([m (regexp-match-positions p s)])
	(and m
	     (zero? (caar m))
	     (= (string-length s) (cdar m)))))))
