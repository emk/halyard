
(module thread mzscheme
  (require "spidey.ss")

  (provide consumer-thread
	  with-semaphore
	  
	  dynamic-disable-break
	  dynamic-enable-break
	  make-single-threader

	  merge-input
	  copy-port

	  run-server)
  
  #|
  t accepts a function, f, and creates a thread. It returns the thread and a
  function, g. When g is applied it passes it's argument to f, and evaluates
  the call of f in the time of the thread that was created. Calls to g do not
  block.
  |#
  
  (define consumer-thread
    (case-lambda
     [(f) (consumer-thread f void)]
     [(f init)
      (unless (procedure? f) (raise-type-error 'consumer-thread "procedure" f))
      (let ([sema (make-semaphore 0)]
	    [protect (make-semaphore 1)]
	    [front-state null]
	    [back-state null])
	(values 
	 (thread
	  (letrec ([loop
		    (lambda ()
		      (semaphore-wait sema)
		      (let ([local-state
			     (begin
			       (semaphore-wait protect)
			       (if (null? back-state)
				   (let ([new-front (reverse front-state)])
				     (set! back-state (cdr new-front))
				     (set! front-state null)
				     (semaphore-post protect)
				     (car new-front))
				   (begin0
				    (car back-state)
				    (set! back-state (cdr back-state))
				    (semaphore-post protect))))])
			(apply f local-state))
		      (loop))])
	    (lambda ()
	      (init)
	      (loop))))
	 (lambda new-state
	   (let ([num (length new-state)])
	     (unless (procedure-arity-includes? f num) 
		     (raise 
		      (make-exn:application:arity
		       (format "<procedure-from-consumer-thread>: consumer procedure arity is ~e; provided ~s argument~a"
			       (procedure-arity f) num (if (= 1 num) "" "s"))
		       #f #f
		       (current-continuation-marks)
		       num
		       (procedure-arity f)))))
	   (semaphore-wait protect)
	   (set! front-state (cons new-state front-state))
	   (semaphore-post protect)
	   (semaphore-post sema))))]))

  (define with-semaphore
    (lambda (s f)
      (semaphore-wait s)
      (begin0 (f)
	      (semaphore-post s))))
 
  (define dynamic-enable-break
    (polymorphic
     (lambda (thunk)
       (parameterize ([break-enabled #t])
	 (thunk)))))
  
  (define dynamic-disable-break
    (polymorphic
     (lambda (thunk)
       (parameterize ([break-enabled #f])
	 (thunk)))))
  
  (define make-single-threader
    (polymorphic
     (lambda ()
       (let ([sema (make-semaphore 1)])
	 (lambda (thunk)
	   (dynamic-wind
	    (lambda () (semaphore-wait sema))
	    thunk
	    (lambda () (semaphore-post sema))))))))

  (define (copy-port src dest . dests)
    (unless (input-port? src)
      (raise-type-error 'copy-port "input-port" src))
    (for-each
     (lambda (dest)
       (unless (output-port? dest)
	 (raise-type-error 'copy-port "output-port" dest)))
     (cons dest dests))
    (let ([s (make-string 4096)])
      (let loop ()
	(let ([c (read-string-avail! s src)])
	  (unless (eof-object? c)
	    (for-each
	     (lambda (dest)
	       (let loop ([start 0])
		 (unless (= start c)
		   (let ([c2 (write-string-avail s dest start c)])
		     (loop (+ start c2))))))
	     (cons dest dests))
	    (loop))))))
  
  (define merge-input
    (case-lambda
     [(a b) (merge-input a b 4096)]
     [(a b limit)
      (or (input-port? a)
	  (raise-type-error 'merge-input "input-port" a))
      (or (input-port? b)
	  (raise-type-error 'merge-input "input-port" b))
      (or (not limit)
	  (and (number? limit) (positive? limit) (exact? limit) (integer? limit))
	  (raise-type-error 'merge-input "positive exact integer or #f" limit))
      (let-values ([(rd wt) (make-pipe limit)]
		   [(other-done?) #f]
		   [(sema) (make-semaphore 1)])
	(let ([copy
	       (lambda (from)
                 (thread 
                  (lambda ()
                    (copy-port from wt)
                    (semaphore-wait sema)
                    (if other-done?
                        (close-output-port wt)
                        (set! other-done? #t))
                    (semaphore-post sema))))])
	  (copy a)
	  (copy b)
	  rd))]))
  
  (define (run-server port-number handler connection-timeout)
    (let ([l (tcp-listen port-number)]
	  [can-break? (break-enabled)])
      (dynamic-wind
       void
       (lambda ()
         ;; loop to handle connections
         (let loop ()
           (with-handlers ([not-break-exn? void])
             ;; Make a custodian for the next session:
             (let ([c (make-custodian)])
               (parameterize ([current-custodian c])
                 ;; disable breaks during session set-up...
                 (parameterize ([break-enabled #f])
                   ;; ... but enable breaks while blocked on an accept:
                   (let-values ([(r w) ((if can-break?
                                            tcp-accept/enable-break
                                            tcp-accept)
                                        l)])
                     ;; Handler thread:
                     (let ([t (thread (lambda () 
                                        (when can-break?
                                          (break-enabled #t))
                                        (handler r w)))])
                       ;; Clean-up and timeout thread:
                       (thread (lambda () 
                                 (object-wait-multiple connection-timeout t)
                                 (when (thread-running? t)
                                   ;; Only happens if connection-timeout is not #f
                                   (break-thread t))
                                 (object-wait-multiple connection-timeout t)
                                 (custodian-shutdown-all c)))))))))
           (loop)))
       (lambda () (tcp-close l))))))
