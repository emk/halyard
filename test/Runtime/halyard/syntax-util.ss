;;; Syntax helpers.  See also "capture.ss".
(module syntax-util (lib "swindle.ss" "swindle")

  (provide check-syntax-is-symbol)
  
  (define (check-syntax-is-symbol name sym-stx msg)
    (unless (symbol? (syntax-object->datum sym-stx))
      (raise-syntax-error name msg sym-stx)))
  )