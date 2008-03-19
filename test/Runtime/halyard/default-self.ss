(module default-self mzscheme
  (provide %default-self)
  
  (define %default-self 
    (lambda (stx)
      (raise-syntax-error #f "can only be used inside class or method." stx))))