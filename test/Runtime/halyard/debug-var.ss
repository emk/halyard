;;=========================================================================
;;  Debugging Support for Local Variables
;;=========================================================================
;;  This a nasty hack for accessing the values of local variables for
;;  debugging purposes.  A real debugger would be *much* better, but PLT
;;  doesn't really have one yet.
;;
;;  Use:
;;    (define foo 1)
;;    (define foo 2)
;;    (debug-vars foo bar)
;;    (dv foo)
;;    (set! (dv foo) 2)

(module debug-var (lib "5l.ss" "halyard")
  
  (provide *debug-vars* debug-vars dv set-dv!)

  (define *debug-vars* '())
  
  (defclass <debug-var> ()
    name
    getter
    setter)

  (define (register-debug-vars! vars)
    (set! *debug-vars* vars))

  (define (find-debug-var name vars)
    (cond
     [(null? vars)
      (error (cat name " is not registered as a debug-var"))]
     [(eq? name (debug-var-name (car vars)))
      (car vars)]
     [#t
      (find-debug-var name (cdr vars))]))

  (define (debug-var name)
    ((debug-var-getter (find-debug-var name *debug-vars*))))

  (define (set-debug-var! name value)
    ((debug-var-setter (find-debug-var name *debug-vars*)) value))

  (define-syntax debug-vars-helper
    (syntax-rules []
      [(debug-vars-helper)
       '()]
      [(debug-vars-helper var vars ...)
       (cons (make-debug-var 'var (fn () var) (fn (val) (set! var val)))
             (debug-vars-helper vars ...))]))

  ;;; Declare a set of debug variables.  Clears the previous set.
  (define-syntax debug-vars
    (syntax-rules []
      [(debug-vars vars ...)
       (register-debug-vars! (debug-vars-helper vars ...))]))

  ;;; Access the value of a local variable for debugging purposes.
  (define-syntax dv
    (syntax-rules []
      [(dv var)
       (debug-var 'var)]))

  ;;; Set the value of a local variable for debugging purposes.
  (define-syntax set-dv!
    (syntax-rules []
      [(set-dv! var value)
       (set-debug-var! 'var value)]))

  )