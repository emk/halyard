;;=========================================================================
;;  Interpolation of Variables into Strings
;;=========================================================================
;;  Handy code by Brian Campbell.  A short summary, assuming 'bar' is bound
;;  to 3:
;;
;;    "foo $bar" -> "foo 3"
;;    "foo${bar}baz" -> "foo3baz"
;;    "foo$(+ bar 2)baz" -> "foo5baz"
;;    "$$" -> "$"

(module interpolate mzscheme
  (provide interpolating-#%datum #%datum)
  
  ;; Converts any object to a string, using "display". This means that a 
  ;; string will remain the same, characters will not be quoted, etc.
  (define (any->string obj)
    (let ((out (open-output-string)))
      (display obj out)
      (get-output-string out)))
  
  ;; Converts all arguments to strings and appends them together.
  (define (any-string-append . rest)
    (if (null? rest)
        ""
        (string-append (any->string (car rest)) 
                       (apply any-string-append (cdr rest)))))
  
  (define-syntax (interpolating-#%datum stx)
    (syntax-case stx ()
      ((_ . datum)

       ;; This is tested in addition to the pattern matching to see if this 
       ;; case is executed
       (let [[str (syntax-object->datum (syntax datum))]]
	 (and (string? str)
	      (regexp-match "\\$" str)))
       
       ;; Get some information on the source location of the string, and setup
       ;; the input port.
       (let* ((syn (syntax datum))
              (source (syntax-source syn))
              (string-pos (syntax-position syn))
              (in (open-input-string (syntax-object->datum syn))))
         
         ;; Finds the current location in the source file
         (define (curr-source-loc)
           (let-values (((line col pos) (port-next-location in)))
             (+ pos string-pos)))
         
         ;; Generates the syntax object representing a sequence of characters 
         ;; not captured by a dollar sign. We need to use a quasisyntax (#`) 
         ;; expression so that the #%datum in the current lexical environment 
         ;; (the normal one from mzscheme) is used rather than our custom one,
         ;; since that would create an infinite loop.
         (define (create-string str offset)
           #`(#%datum . #,(datum->syntax-object 
                           syn
                           str
                           (list source #f #f offset (string-length str))
                           syn)))
         
         ;; Read a string of characters from the input string up to the next $
         ;; Returns a syntax object with the source information properly set up.
         ;; We use peek char so when we encounter a dollar sign, we can leave it
         ;; on the port.
         (define (read-string)
           (let ((offset (curr-source-loc)))
             (let loop ((str "") (chr (peek-char in)))
               (cond
                 ((eof-object? chr) (create-string str offset))
                 ((eq? chr #\$) (create-string str offset))
                 (else (loop (string-append str (string (read-char in))) 
                             (peek-char in)))))))
         
         ;; Decide what to do after a dollar sign. If there are two dollar
         ;; signs in a row, it returns one dollar sign, and highlights the
         ;; second dollar sign. If there is a dollar sign at the end of a 
         ;; string, it returns it as a literal dollar sign. If there is a 
         ;; curly brace, it reads the expression, and then adds begin to 
         ;; allow a single variable without a delimiter after it in a string.
         ;; Otherwise, it just reads an expression after the dollar sign.
         (define (read-dollar)
           (read-char in)
           (let ((chr (peek-char in)))
             (cond
               ((eof-object? chr) (create-string "$" (- (curr-source-loc) 1)))
               ((eq? chr #\$) (read-char in) (create-string "$" (- (curr-source-loc) 1)))
               ((eq? chr #\{) (cons #'begin (read-expr)))
               (else (read-expr)))))
         
         ;; Lexically attaches syntax stx to the lexical environment of ctx. 
         ;; Does so by recursively descending through all list structure,
         ;; converting all syntax objects to list (or pair) objects, recursing
         ;; through those, and then turning them back into syntax objects at
         ;; the correct location.
         (define (lex-attach stx ctx)
           (let loop ((val stx))
             (cond
               ((pair? val) (cons (loop (car val))
                                  (loop (cdr val))))
               ((null? val) null)
               (else
                (let ((v (syntax-e val)))
                  (datum->syntax-object 
                   ctx
                   (cond 
                     ((pair? v) (loop v))
                     ((vector? v) (list->vector (loop (vector->list v))))
                     ((box? v) (box (loop (unbox v))))
                     (else v))
                   val
                   val))))))
         
         ;; Read an expression from the string. read-syntax returns a syntax
         ;; object with no lexical context, so we need to attach it to one 
         ;; using lex-attach. The offset in read-syntax is not the offset of 
         ;; the current character being read, but rather the offset of the 
         ;; beginning of the port from the beginning of the file.
         (define (read-expr)
           (lex-attach (read-syntax source in (list 0 0 string-pos))
                       syn))
         
         ;; The actuall body of the macro. Builds the call to 
         ;; any-string-append from the results of repeated calls to
         ;; read-string and read-dollar.
         (let loop ((out-list '()))
           (cond
             ((eof-object? (peek-char in))
              (datum->syntax-object syn
                                    (cons #'any-string-append
                                          (reverse out-list))))
               ((eq? (peek-char in) #\$) (loop (cons (read-dollar) out-list)))
               (else (loop (cons (read-string) out-list)))))))
      
      ((_ . datum) (syntax (#%datum . datum))))))            
