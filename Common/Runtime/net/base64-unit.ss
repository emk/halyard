

(module base64-unit mzscheme
  (require (lib "unitsig.ss"))

  (require "base64-sig.ss")

  (provide net:base64@)
  (define net:base64@
    (unit/sig net:base64^
      (import)

      (define base64-digit (make-vector 256))
      (let loop ([n 0])
	(unless (= n 256)
	  (cond
	   [(<= (char->integer #\A) n (char->integer #\Z)) 
	    (vector-set! base64-digit n (- n (char->integer #\A)))]
	   [(<= (char->integer #\a) n (char->integer #\z)) 
	    (vector-set! base64-digit n (+ 26 (- n (char->integer #\a))))]
	   [(<= (char->integer #\0) n (char->integer #\9)) 
	    (vector-set! base64-digit n (+ 52 (- n (char->integer #\0))))]
	   [(= (char->integer #\+) n)
	    (vector-set! base64-digit n 62)]
	   [(= (char->integer #\/) n)
	    (vector-set! base64-digit n 63)]
	   [else
	    (vector-set! base64-digit n #f)])
	  (loop (add1 n))))

      (define digit-base64 (make-vector 64))
      (let ([each-char (lambda (s e pos)
			 (let loop ([i (char->integer s)][pos pos])
			   (unless (> i (char->integer e))
			     (vector-set! digit-base64 pos (integer->char i))
			     (loop (add1 i) (add1 pos)))))])
	(each-char #\A #\Z 0)
	(each-char #\a #\z 26)
	(each-char #\0 #\9 52)
	(each-char #\+ #\+ 62)
	(each-char #\/ #\/ 63))

      (define (base64-decode-stream in out)
	(let loop ([waiting 0][waiting-bits 0])
	  (if (>= waiting-bits 8)
	      (begin
		(display (integer->char (arithmetic-shift waiting (- 8 waiting-bits)))
			 out)
		(let ([waiting-bits (- waiting-bits 8)])
		  (loop (bitwise-and waiting (sub1 (arithmetic-shift 1 waiting-bits)))
			waiting-bits)))
	      (let* ([c0 (read-char in)]
		     [c (if (eof-object? c0) #\= c0)]
		     [v (vector-ref base64-digit (char->integer c))])
		(cond
		 [v (loop (+ (arithmetic-shift waiting 6) v)
			  (+ waiting-bits 6))]
		 [(eq? c #\=) (void)] ; done
		 [else (loop waiting waiting-bits)])))))


      (define base64-encode-stream
	(case-lambda
	 [(in out) (base64-encode-stream in out #\newline)]
	 [(in out linesep)
	  ;; Process input 3 characters at a time, because 18 bits
	  ;;  is divisible by both 6 and 8, and 72 (the line length)
	  ;;  is divisible by 3.
	  (let ([three (make-string 3)]
		[outc (lambda (n)
			(display (vector-ref digit-base64 n) out))]
		[done (lambda (fill)
			(let loop ([fill fill])
			  (unless (zero? fill)
			    (display #\= out)
			    (loop (sub1 fill))))
			(display linesep out))])
	    (let loop ([pos 0])
	      (if (= pos 72)
		  ; Insert newline
		  (begin
		    (display linesep out)
		    (loop 0))
		  ;; Next group of 3
		  (let ([n (read-string-avail! three in)])
		    (cond
		     [(eof-object? n)
		      (unless (= pos 0)
			(done 0))]
		     [(= n 3)
		      ;; Easy case:
		      (let ([a (char->integer (string-ref three 0))]
			    [b (char->integer (string-ref three 1))]
			    [c (char->integer (string-ref three 2))])
			(outc (arithmetic-shift a -2))
			(outc (+ (bitwise-and #x3f (arithmetic-shift a 4))
				 (arithmetic-shift b -4)))
			(outc (+ (bitwise-and #x3f (arithmetic-shift b 2))
				 (arithmetic-shift c -6)))
			(outc (bitwise-and #x3f c))
			(loop (+ pos 4)))]
		     [else
		      ;; Hard case: n is 1 or 2
		      (let ([a (char->integer (string-ref three 0))])
			(outc (arithmetic-shift a -2))
			(let* ([next (if (= n 2)
					 (string-ref three 1)
					 (read-char in))]
			       [b (if (char? next)
				      (char->integer next)
				      0)])
			  (outc (+ (bitwise-and #x3f (arithmetic-shift a 4))
				   (arithmetic-shift b -4)))
			  (if (eof-object? next)
			      (done 2)
			      ;; More to go
			      (let* ([next (read-char in)]
				     [c (if (char? next)
					    (char->integer next)
					    0)])
				(outc (+ (bitwise-and #x3f (arithmetic-shift b 2))
					 (arithmetic-shift c -6)))
				(if (eof-object? next)
				    (done 1)
				    ;; Finish c, loop
				    (begin
				      (outc (bitwise-and #x3f c))
				      (loop (+ pos 4))))))))])))))]))

      (define (base64-decode src)
	(let ([s (open-output-string)])
	  (base64-decode-stream (open-input-string src) s)
	  (get-output-string s)))

      (define (base64-encode src)
	(let ([s (open-output-string)])
	  (base64-encode-stream (open-input-string src) s
				(string #\return #\newline))
	  (get-output-string s))))))

