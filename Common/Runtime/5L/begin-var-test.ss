(module begin-var-test mzscheme
  (require "begin-var.ss")
  
  (begin/var
    (var x 10)
    (set! x (+ x 10))
    (display x)
    x))