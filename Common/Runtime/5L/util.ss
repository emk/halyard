(module util (lib "lispish.ss" "5L")

  (require (lib "begin-var.ss" "5L"))


  ;;=======================================================================
  ;;  Error Messages
  ;;=======================================================================

  ;; Import %call-5l-prim from the engine.
  ;; DO NOT CALL ANYTHING BUT LOGGING FUNCTIONS USING '%CALL-5L-PRIM'--USE
  ;; 'CALL-5L-PRIM' INSTEAD.
  (require #%fivel-engine)

  (provide 5l-log debug-log caution debug-caution non-fatal-error
           fatal-error)

  (define (5l-log msg)
    (%call-5l-prim 'log '5L msg 'log))
  
  (define (debug-log msg)
    (%call-5l-prim 'log 'Debug msg 'log))
  
  (define (caution msg)
    (%call-5l-prim 'log '5L msg 'caution))
  
  (define (debug-caution msg)
    (%call-5l-prim 'log 'Debug msg 'caution))
  
  (define (non-fatal-error msg)
    (%call-5l-prim 'log '5L msg 'error))
  
  (define (fatal-error msg)
    (%call-5l-prim 'log '5L msg 'fatalerror))
  

  ;;=======================================================================
  ;;  Assertions
  ;;=======================================================================

  (provide assert)

  (define (%kernel-assert label value)
    (when (not value)
      (fatal-error (cat "Assertion failure: " label))))
  
  (define-syntax assert
    (syntax-rules ()
      [(assert cond)
       (%kernel-assert 'cond cond)]))


  ;;=======================================================================
  ;;  Utility Functions
  ;;=======================================================================

  (provide foreach member? value->string cat keyword-name
           hash-table-has-key?
           label with-errors-blocked with-values)

  ;;; Run a body once for each item in a list.
  ;;;
  ;;; @syntax (foreach [name list] body ...)
  ;;; @param NAME name The variable to use as the item name.
  ;;; @param LIST list The list from which to get the items.
  ;;; @param BODY body The code to run for each list item.
  (define-syntax foreach
    (syntax-rules ()
      [(foreach [name lst] body ...)
       (let loop [[remaining lst]]
         (unless (null? remaining)
           (let [[name (car remaining)]]
             (begin/var body ...))
           (loop (cdr remaining))))]))
  
  (define (member? item list)
    (if (null? list)
        #f
        (if (equal? item (car list))
            #t
            (member? item (cdr list)))))
  
  (define (value->string value)
    (if (string? value)
        value
        (let ((str-port (open-output-string)))
          (write value str-port)
          (get-output-string str-port))))
  
  (define (cat . values)
    (if (not (null? values))
        (string-append (value->string (car values)) (apply cat (cdr values)))
        ""))

  (define (keyword-name value)
    (assert (keyword? value))
    (let [[str (symbol->string value)]]
      (string->symbol (substring str 1 (string-length str)))))

  (define (hash-table-has-key? table key)
    (define result #t)
    (hash-table-get table key (lambda () (set! result #f)))
    result)

  (define-syntax label
    (syntax-rules ()
      [(label name body ...)
       (call-with-escape-continuation (lambda (name)
                                        (begin/var body ...)))]))

  (define (call-with-errors-blocked report-func thunk)
    (let* ((result (with-handlers ([void (lambda (exn) (cons #f exn))])
                     (cons #t (thunk))))
           (good? (car result))
           (exn-or-value (cdr result)))
      (if good?
          exn-or-value
          (begin
            (report-func (exn-message exn-or-value))
            #f))))

  (define-syntax with-errors-blocked
    (syntax-rules ()
      [(with-errors-blocked (report-func) body ...)
       (call-with-errors-blocked report-func
                                 (lambda () (begin/var body ...)))]))

  (define-syntax with-values
    (syntax-rules ()
      [(with-values values expr body ...)
       (call-with-values (lambda () expr) (lambda values body ...))]))
             

  )
