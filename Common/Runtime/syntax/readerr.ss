(module readerr mzscheme
  (provide raise-read-error)

  (define (raise-read-error msg source-name line col pos span)
    (let ([bad-type
           (lambda (which what)
             (raise-type-error 'raise-read-error
                               what
                               which
                               msg source-name line col pos span))]
          [ordinal? (lambda (x)
                      (or (not x) 
                          (and (number? x) (exact? x) (positive? x) (integer? x))))]
          [ordinal "positive exact integer"])
    
      (unless (string? msg)
        (bad-type 0 "string"))
      (unless (ordinal? line)
        (bad-type 2 ordinal))
      (unless (ordinal? col)
        (bad-type 3 ordinal))
      (unless (ordinal? pos)
        (bad-type 4 ordinal))
      (unless (or (not span)
                  (and (number? span) (exact? span) (not (negative? span)) (integer? span)))
        (bad-type 5 "non-negative exact integer"))
                  
      
      (raise
       (make-exn:read
        (string->immutable-string
         (format "~a~a"
                 (cond
                   [(not (error-print-source-location)) ""]
                   [(and line col)
                    (format "~a:~a:~a: " source-name line col)]
                   [pos
                    (format "~a::~a: " source-name pos)]
                   [else
                    (format "~a: " source-name)])
                 msg))
        (current-continuation-marks)
        source-name line col pos span)))))
