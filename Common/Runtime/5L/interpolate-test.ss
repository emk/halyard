
(module test-interpolate interpolate
  (define (fact x)
    (if (= x 0)
        1
        (* x (fact (- x 1)))))
  
  (define (perm x y)
    (/ (fact x) (fact y)))
  
  (define (binom x y)
    (/ (perm x y) (fact (- x y))))
  
  (let loop ((i 0))
    (if (= i 20)
        #t
        (begin
          (display "fact($i) = $(fact i) 
And here is a dollar sign: $$\n")
          (display "Wordwith${i}inside\n")
          (display "Dollar at end of string: $")
          (display "Just a string")
          (display "Oldline\nnewline")
          (display "$(fact \ni)stuff")
          (display "foo$$foo")
          (newline)
          (display "Testing let: $(let loop ((i i) (lst '()))
                                    (cond
                                      ((= i 0) lst)
                                      (else (loop (- i 1) 
                                                  (cons (fact i) lst))))) 
OK, done with the test...")
          (newline)
          (display "Testing profiling: $(let loop ((j i) (lst '()))
                                          (cond 
                                            ((< j 0) lst)
                                            (else (loop (- j 1) 
                                                        (cons (binom i j)
                                                              lst)))))
OK, done with the profiling test...")
          (newline)
          (loop (+ i 1))))))
