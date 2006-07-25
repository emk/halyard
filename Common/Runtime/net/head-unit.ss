
(module head-unit mzscheme
  (require (lib "unitsig.ss"))

  (require "head-sig.ss")

  (provide net:head@)
  (define net:head@
    (unit/sig net:head^
      (import)

      (define empty-header (string #\return #\newline))

      (define (string->ci-regexp s)
	(list->string
	 (apply
	  append
	  (map
	   (lambda (c)
	     (cond 
	      [(memq c '(#\$ #\| #\\ #\[ #\] #\. #\* #\? #\+ #\( #\) #\^))
	       (list #\\ c)]
	      [(char-alphabetic? c)
	       (list #\[ (char-upcase c) (char-downcase c) #\])]
	      [else (list c)]))
	   (string->list s)))))

      (define re:field-start (regexp
			      (format "^[^~a~a~a~a~a:~a-~a]*:" 
				      #\space #\tab #\linefeed #\return #\vtab
				      (integer->char 1)
				      (integer->char 26))))
      (define re:continue (regexp (format "^[~a~a~a]" #\space #\tab #\vtab)))
      
      (define (validate-header s)
	(let ([len (string-length s)])
	  (let loop ([offset 0])
	    (cond
	     [(and (= (+ offset 2) len)
		   (string=? empty-header (substring s offset len)))
	      (void)] ; validated
	     [(= offset len) (error 'validate-header "missing ending CRLF")]
	     [(or (regexp-match re:field-start s offset)
		  (regexp-match re:continue s offset))
	      (let ([m (regexp-match-positions (string #\return #\linefeed) s offset)])
		(if m
		    (loop (cdar m))
		    (error 'validate-header "missing ending CRLF")))]
	     [else (error 'validate-header "ill-formed header at ~s" 
			  (substring s offset (string-length s)))]))))
      
      (define (make-field-start-regexp field)
	(format "(^|[~a][~a])(~a: *)" 
		#\return #\linefeed 
		(string->ci-regexp field)))

      (define (extract-field field header)
	(let ([m (regexp-match-positions 
		  (make-field-start-regexp field)
		  header)])
	  (and m
	       (let ([s (substring header 
				   (cdaddr m)
				   (string-length header))])
		 (let ([m (regexp-match-positions 
			   (format "[~a][~a][^: ~a~a]*:"
				   #\return #\linefeed
				   #\return #\linefeed)
			   s)])
		   (if m
		       (substring s 0 (caar m))
		       ;; Rest of header is this field, but strip trailing CRLFCRLF:
		       (regexp-replace (format "~a~a~a~a$" #\return #\linefeed #\return #\linefeed)
				       s
				       "")))))))

      (define (remove-field field header)
	(let ([m (regexp-match-positions 
		  (make-field-start-regexp field)
		  header)])
	  (if m
	      (let ([pre (substring header
				    0
				    (caaddr m))]
		    [s (substring header 
				  (cdaddr m)
				  (string-length header))])
		(let ([m (regexp-match-positions 
			  (format "[~a][~a][^: ~a~a]*:"
				  #\return #\linefeed
				  #\return #\linefeed)
			  s)])
		  (if m
		      (string-append pre (substring s (+ 2 (caar m)) 
						    (string-length s)))
		      pre)))
	      header)))

      (define (insert-field field data header)
	(let ([field (format "~a: ~a~a~a"
			     field
			     data
			     #\return #\linefeed)])
	  (string-append field header)))

      (define (append-headers a b)
	(let ([alen (string-length a)])
	  (if (> alen 1)
	      (string-append (substring a 0 (- alen 2)) b)
	      (error 'append-headers "first argument is not a header: ~a" a))))
      
      (define (extract-all-fields header)
	(let ([re (regexp (format "(^|[~a][~a])(([^~a~a:]*): *)" 
				  #\return #\linefeed #\return #\linefeed))]) 
	  (let loop ([start 0])
	    (let ([m (regexp-match-positions re header start)])
	      (if m
		  (let ([start (cdaddr m)]
			[field-name (substring header (caaddr (cdr m)) (cdaddr (cdr m)))])
		    (let ([m2 (regexp-match-positions 
			       (format "[~a][~a][^: ~a~a]*:"
				       #\return #\linefeed
				       #\return #\linefeed)
			       header
			       start)])
		      (if m2
			  (cons (cons field-name
				      (substring header start (caar m2)))
				(loop (caar m2)))
			  ;; Rest of header is this field, but strip trailing CRLFCRLF:
			  (list
			   (cons field-name
				 (regexp-replace (format "~a~a~a~a$" #\return #\linefeed #\return #\linefeed)
						 (substring header start (string-length header))
						 ""))))))
		  ;; malformed header:
		  null)))))
      
      (define (standard-message-header from tos ccs bccs subject)
	(let ([h (insert-field
		  "Subject" subject
		  empty-header)])
	  ;; NOTE: bccs don't go into the header; that's why
	  ;; they're "blind"
	  (let ([h (if (null? ccs)
		       h
		       (insert-field 
			"CC" (assemble-address-field ccs)
			h))])
	    (let ([h (if (null? tos)
			 h
			 (insert-field 
			  "To" (assemble-address-field tos)
			  h))])
	      (insert-field 
	       "From" from
	       h)))))

      (define (splice l sep)
	(if (null? l)
	    ""
	    (format "~a~a"
		    (car l)
		    (apply
		     string-append
		     (map
		      (lambda (n) (format "~a~a" sep n))
		      (cdr l))))))

      (define (data-lines->data datas)
	(splice datas (format "~a~a~a" #\return #\linefeed #\tab)))

      ;; Extracting Addresses ;;

      (define blank (format "[~a~a~a~a~a]" #\space #\tab #\newline #\return #\vtab))
      (define nonblank (format "[^~a~a~a~a~a]" #\space #\tab #\newline #\return #\vtab))
      (define re:all-blank (regexp (format "^~a*$" blank)))
      (define re:quoted (regexp "\"[^\"]*\""))
      (define re:comma (regexp ","))
      (define re:comma-separated (regexp "([^,]*),(.*)"))

      (define (extract-addresses s form)
	(unless (memq form '(name address full all))
	  (raise-type-error 'extract-addresses 
			    "form: 'name, 'address, 'full, or 'all"
			    form))
	(if (or (not s) (regexp-match re:all-blank s))
	    null
	    (let loop ([prefix ""][s s])
	      ;; Which comes first - a quote or a comma?
	      (let ([mq (regexp-match-positions re:quoted s)]
		    [mc (regexp-match-positions re:comma s)])
		(if (and mq mc (< (caar mq) (caar mc) (cdar mq)))
		    ;; Quote contains a comma
		    (loop (string-append 
			   prefix 
			   (substring s 0 (cdar mq)))
			  (substring s (cdar mq) (string-length s)))
		    ;; Normal comma parsing:
		    (let ([m (regexp-match re:comma-separated s)])
		      (if m
			  (let ([n (extract-one-name (string-append prefix (cadr m)) form)]
				[rest (extract-addresses (caddr m) form)])
			    (cons n rest))
			  (let ([n (extract-one-name (string-append prefix s) form)])
			    (list n)))))))))
      
      (define (select-result form name addr full)
	(case form
	  [(name) name]
	  [(address) addr]
	  [(full) full]
	  [(all) (list name addr full)]))

      (define (one-result form s)
	(select-result form s s s))

      (define re:quoted-name (regexp (format "^~a*(\"[^\"]*\")(.*)" blank)))
      (define re:parened-name (regexp (format "(.*)[(]([^)]*)[)]~a*$" blank)))
      (define re:simple-name (regexp (format "^~a*(~a.*)(<.*>)~a*$" blank nonblank blank)))
      (define re:normal-name (regexp (format "~a*<([^>]*)>~a*" blank blank)))
      (define re:double-less (regexp "<.*<"))
      (define re:double-greater (regexp ">.*>"))
      (define re:bad-chars (regexp "[,\"()<>]"))
      (define re:tail-blanks (regexp (format "~a+$" blank)))
      (define re:head-blanks (regexp (format "^~a+" blank)))

      (define (extract-one-name orig form)
	(let loop ([s orig][form form])
	  (cond
	   ;; ?!?!? Where does the "addr (name)" standard come from ?!?!?
	   [(regexp-match re:parened-name s)
	    => (lambda (m)
		 (let ([name (caddr m)]
		       [all (loop (cadr m) 'all)])
		   (select-result form 
				  (if (string=? (car all) (cadr all))
				      name
				      (car all))
				  (cadr all)
				  (format "~a (~a)" (caddr all) name))))]
	   [(regexp-match re:quoted-name s)
	    => (lambda (m)
		 (let ([name (cadr m)]
		       [addr (extract-angle-addr (caddr m) s)])
		   (select-result form name addr
				  (format "~a <~a>" name addr))))]
	   [(regexp-match re:simple-name s)
	    => (lambda (m)
		 (let ([name (regexp-replace (format "~a*$" blank) (cadr m) "")]
		       [addr (extract-angle-addr (caddr m) s)])
		   (select-result form name addr 
				  (format "~a <~a>" name addr))))]
	   [(or (regexp-match "<" s) (regexp-match ">" s))
	    (one-result form (extract-angle-addr s orig))]
	   [else
	    (one-result form (extract-simple-addr s orig))])))

      (define (extract-angle-addr s orig)
	(if (or (regexp-match re:double-less s) (regexp-match re:double-greater s))
	    (error 'extract-address "too many angle brackets: ~a" s)
	    (let ([m (regexp-match re:normal-name s)])
	      (if m
		  (extract-simple-addr (cadr m) orig)
		  (error 'extract-address "cannot parse address: ~a" orig)))))

      (define (extract-simple-addr s orig)
	(cond
	 [(regexp-match re:bad-chars s)
	  (error 'extract-address "cannot parse address: ~a" orig)]
	 [else 
	  ;; final whitespace strip
	  (regexp-replace
	   re:tail-blanks
	   (regexp-replace re:head-blanks s "")
	   "")]))

      (define (assemble-address-field addresses)
	(if (null? addresses)
	    ""
	    (let loop ([addresses (cdr addresses)]
		       [s (car addresses)]
		       [len (string-length (car addresses))])
	      (if (null? addresses)
		  s
		  (let* ([addr (car addresses)]
			 [alen (string-length addr)])
		    (if (<= 72 (+ len alen))
			(loop (cdr addresses)
			      (format "~a,~a~a~a~a" 
				      s #\return #\linefeed 
				      #\tab addr)
			      alen)
			(loop (cdr addresses)
			      (format "~a, ~a" s addr)
			      (+ len alen 2)))))))))))
