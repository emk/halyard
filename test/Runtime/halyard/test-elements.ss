(module test-elements (lib "halyard.ss" "halyard")
  (provide test-elements)
  
  (define (call-with-temporary-parent thunk)
    (let [[elem #f]]
      (dynamic-wind
          (lambda ()
            (set! elem (%box% .new :at (point 0 0) 
                                   :shape $screen-rect
                                   :name 'temporary-parent)))
          (lambda ()
            (with-default-element-parent elem
              (thunk)))
          (lambda ()
            (delete-element elem)))))
  
  (define-syntax with-temporary-parent
    (syntax-rules ()
      [(_ body ...)
       (call-with-temporary-parent (lambda () body ...))]))
  (define-syntax-indent with-temporary-parent 0)
  
  (define-syntax test-elements
    (syntax-rules ()
      [[_ description . body]
       (.add-test-method! description
         (method () 
           (with-temporary-parent . body)))]))
  (define-syntax-indent test-elements 1)
  )