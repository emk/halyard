(define (char-scheme-sexp-initial? ch)
  (or (char-alphabetic? ch)
      (memv ch '(#\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\^ #\_ #\~ #\+ #\-))))

(define (char-scheme-sexp-subsequent? ch)
  (or (char-scheme-sexp-initial? ch)
      (char-numeric? ch)
      (memv ch '(#\+ #\- #\. #\@))))

(define scheme-sexp-parse-table
  `((scheme-sexp (/ scheme-sexp-list
		    scheme-sexp-atom
		    (q <- scheme-sexp-quotelike scheme-sexp-ws e <- scheme-sexp
		       ,(packrat-lambda (q e) (list q e)))
		    scheme-sexp-vector
		    ))

    (scheme-sexp-list (/ (#\( scheme-sexp-ws (heads <- scheme-sexp)+
			  (/ (#\. scheme-sexp-ws tail <- scheme-sexp #\) scheme-sexp-ws
			      ,(packrat-lambda (heads tail)
					       (fold-right cons tail heads)))
			     (#\) scheme-sexp-ws ,(packrat-lambda (heads) heads))))
			 (#\( scheme-sexp-ws #\) scheme-sexp-ws ,(packrat-lambda () '()))))
    (scheme-sexp-vector (#\# lst <- scheme-sexp-list
			 ,(packrat-lambda* ks kf (lst)
					   (if (list? lst)
					       (ks (list->vector lst))
					       (kf make-error-message
						   "Vectors must not be dotted")))))

    (scheme-sexp-quotelike (/ (#\' ,(packrat-lambda () 'quote))
			      (#\` ,(packrat-lambda () 'quasiquote))
			      (",@" ,(packrat-lambda () 'unquote-splicing))
			      (#\, ,(packrat-lambda () 'unquote))))

    (scheme-sexp-atom (a <- scheme-sexp-atom* scheme-sexp-ws ,(packrat-lambda (a) a)))

    (scheme-sexp-atom* (/ (#\# #\t ,(packrat-lambda () #t))
			  (#\# #\f ,(packrat-lambda () #f))
			  (#\# #\\ scheme-sexp-any-char)
			  (#\" s <- scheme-sexp-string-tail
			   ,(packrat-lambda (s) (list->string s)))
			  ((sign <- (/ #\+ #\-))? (d <- scheme-sexp-digit)+
			   ,(packrat-lambda (sign d)
					    (string->number (list->string (append sign d)))))
			  scheme-sexp-identifier
			  ))

    (scheme-sexp-identifier (/ ("..." ,(packrat-lambda () '...))
			       ((i <- scheme-sexp-initial) (s <- scheme-sexp-subsequent)*
				,(packrat-lambda (i s)
						 (string->symbol (list->string (cons i s)))))))

    (scheme-sexp-any-char (/: ,values "any character"))

    (scheme-sexp-digit (/: ,char-numeric? "digit"))

    (scheme-sexp-initial (/: ,char-scheme-sexp-initial? "scheme-sexp-initial"))
    (scheme-sexp-subsequent (/: ,char-scheme-sexp-subsequent? "scheme-sexp-subsequent"))
				
    (scheme-sexp-string-tail (/ (#\" ,(packrat-lambda () '()))
				(#\\ ch <- scheme-sexp-any-char s <- scheme-sexp-string-tail
				 ,(packrat-lambda (ch s) (cons ch s)))
				(ch <- scheme-sexp-any-char s <- scheme-sexp-string-tail
				    ,(packrat-lambda (ch s) (cons ch s)))))

    (scheme-sexp-ws (/ ((/: ,char-whitespace? "whitespace")+ scheme-sexp-ws)
		       ()))))

; (define parse-rule-parse-table
;   ;; Requires scheme-sexp rules.
;   ;;
;   `((parse-rule (name <- scheme-sexp-identifier #\: scheme-sexp-ws
; 		      rule <- parse-rule-alternation
; 		      ,(packrat-lambda (name rule) (list name rule))))

;     (parse-rule-alternation (/ (alt0 <- parse-rule-simple
; 				     (#\/ scheme-sexp-ws alts <- parse-rule-simple)*
; 				     ,(packrat-lambda (alt0 alts) `(/ ,alt0 ,@alts)))
; 			       () ,(packrat-lambda () '())))

;     (parse-rule-simple (/ (id <- scheme-sexp-identifier 
; 			  scheme-sexp-identifier
; 			  scheme-sexp-string

(define (read-scheme-sexp str)
  (let ((p ((packrat-parse scheme-sexp-parse-table) 'scheme-sexp)))
    (try-packrat-parse-pattern
     p '()
     (packrat-string-results "<str>" str)
     (lambda (bindings result) (values bindings (parse-result-semantic-value result)))
     (lambda (err)
       (list 'parse-error
	     (parse-position->string (parse-error-position err))
	     (parse-error-expected err)
	     (parse-error-messages err))))))
