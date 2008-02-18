(module initialize-slot (lib "language.ss" "5L")
  (require (lib "util.ss" "5L"))
        
  (with-instance %class%
    (def (initialize-slot name init-value)
      (.define-method (symcat "set-" name "!")
        (method (val)
          (if (.initialized?)
            (error "Tried to set slot " name " on " self " using the setter "
                   "after initialization.")
            (set! (slot name) val))))
      (.attr-initializer name (method () init-value) #f))))