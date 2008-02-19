(module initialize-slot (lib "language.ss" "5L")
  (require (lib "util.ss" "5L"))
        
  (with-instance %class%
    ;; TODO - Experimental code for .initialize-slot.  We need to think
    ;; about this, possibly redesign it, and--in any case--make it take a
    ;; method thunk as an initializer instead of a value.
    (def (initialize-slot name init-value)
      (.define-method (symcat "set-" name "!")
        (method (val)
          (if (.initialized?)
            (error "Tried to set slot " name " on " self " using the setter "
                   "after initialization.")
            (set! (slot name) val))))
      (.attr-initializer name (method () init-value) #f))))