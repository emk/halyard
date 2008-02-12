(module trace-test mzscheme
  (require trace)
  
  (define (fact x)
    (if (= x 0)
        1
        (* x (fact (- x 1)))))
  
  (+ 1 (* 3 4))
  (display "Hi!\n")
  (display (fact (* 3 4)))
  (newline)
  (if (= 3 4)
       (display "uh-oh")
       (display "the world is good"))
  
  (with-tracing
   (+ 1 (* 3 4))
   (display "Hi!\n")
   (display (fact (* 3 4)))
   (newline)
   (if (= 3 4)
       (display "uh-oh")
       (display "the world is good")))
  
  (+ 1 (* 3 4))
  (display "Hi!\n")
  (display (fact (* 3 4)))
  (newline)
  (if (= 3 4)
       (display "uh-oh")
       (display "the world is good"))
  
  (define trace-str "")
  (set-trace-fn! (lambda (x) (set! trace-str 
                                   (string-append trace-str 
                                                  (string-append x "\n")))))
  (with-tracing
   (+ 1 (* 3 4))
   (display "Hi!\n")
   (display (fact (* 3 4)))
   (newline)
   (if (= 3 4)
       (display "uh-oh")
       (display "the world is good")))
  
  (display "\n\nTrace output:\n\n")
  (display trace-str))
