;;=========================================================================
;;  Tracing of Function Applications in the Debug Log
;;=========================================================================
;;  Handy code by Brian Campbell, who's promised to document how this
;;  works.

(module trace mzscheme
  (provide with-tracing
           set-trace-output-printer!)
  
  (define *trace-output-fn*
    (lambda (str)
      (display "Tracing: ")
      (display str)
      (newline)))
  
  (define (set-trace-output-printer! fun)
    (set! *trace-output-fn* fun))
  
  (define (trace-format proc . args)
    (let ((out (open-output-string)))
      (display ">>> " out)
      (display (object-name proc) out)
      (display ": " out)
      (for-each (lambda (x)
                  (print x out)
                  (display " " out))
                args)
      (*trace-output-fn* (get-output-string out))))
  
  (define-syntax (with-tracing stx)
    (syntax-case stx  ()
      ((_ body ...) 
       (quasisyntax/loc 
        stx
        (let-syntax ((#,(datum->syntax-object stx '#%app) 
                       (lambda (stx)
                         (syntax-case stx ()
                           ((_ proc . args) 
                            (let ((names (generate-temporaries (syntax args))))
                              #`(let ((p proc)
                                      #,@(map (lambda (name value)
                                                (datum->syntax-object (syntax args)
                                                                      (list name value)
                                                                      #f
                                                                      #f))
                                              names (syntax->list (syntax args))))
                                  (trace-format p #,@names)
                                  (p #,@names))))))))
           body ...))))))
