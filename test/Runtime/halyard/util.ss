(module util (lib "swindle.ss" "swindle")

  (require (lib "begin-var.ss" "halyard"))
  (require (lib "indent.ss" "halyard"))
  (require (lib "errortrace-lib.ss" "errortrace"))


  ;;=======================================================================
  ;;  Error Messages
  ;;=======================================================================

  ;; Import %call-prim from the engine.
  ;; DO NOT CALL ANYTHING BUT LOGGING FUNCTIONS USING '%CALL-5L-PRIM'--USE
  ;; 'CALL-5L-PRIM' INSTEAD.
  (require #%fivel-engine)

  (provide 5l-log debug-log caution debug-caution non-fatal-error
           fatal-error)

  ;;; Write a message to 5L.log.  This log is always present on a user's
  ;;; system, and is never deleted, so use this function sparingly.
  (define (5l-log msg)
    (%call-prim 'Log 'halyard msg 'log))
  
  ;;; Write a message to Debug.log, which is only present on developer
  ;;; systems (though the last hundred lines are always available in a
  ;;; crash report).  This is a very high-volume log, so feel free to be
  ;;; verbose.
  (define (debug-log msg)
    (%call-prim 'Log 'Debug msg 'log))
  
  ;;; Print a "Caution" message to 5L.log.  This should be used for very
  ;;; serious warnings only--see the note about 5L.log on 5L-LOG.
  (define (caution msg)
    (%call-prim 'Log 'halyard msg 'caution))
  
  ;;; Print a "Caution" message to Debug.log.  High-volume output is OK.
  (define (debug-caution msg)
    (%call-prim 'Log 'Debug msg 'caution))
  
  ;;; Show a non-fatal error dialog in developer mode, or quit the engine
  ;;; and send a crash report in runtime mode.
  (define (non-fatal-error msg)
    (%call-prim 'Log 'halyard msg 'error))
  
  ;;; Show a fatal error and quit the engine, regardless of mode.  Sends
  ;;; a crash report.
  (define (fatal-error msg)
    (%call-prim 'Log 'halyard msg 'fatalerror))
  

  ;;=======================================================================
  ;;  Assertions
  ;;=======================================================================

  (provide %assert assert)

  (define (%kernel-assert fatal? label value)
    (when (not value)
      (let [[message (cat "Assertion failure: " label)]]
        (if fatal?
            (fatal-error message)
            (error message)))))
  
  ;;; This is an ASSERT for engine developers: It crashes the engine and
  ;;; probably e-mails a bug report.  Don't use this to check for
  ;;; regular user errors; use it to check for things which should
  ;;; never happen no matter how broken the user's script is.
  (define-syntax %assert
    (syntax-rules ()
      [(%assert cond)
       (%kernel-assert #t 'cond cond)]))
  (define-syntax-indent %assert function)

  ;;; This is an ASSERT for scriptors: It doesn't crash the engine, and
  ;;; it lets them fix their problem.
  (define-syntax assert
    (syntax-rules ()
      [(assert cond)
       (%kernel-assert #f 'cond cond)]))
  (define-syntax-indent assert function)


  ;;=======================================================================
  ;;  Utility Functions
  ;;=======================================================================

  (provide foreach member? value->string cat symcat keyword-name
           hash-table-has-key?
           label with-errors-blocked with-values curry)

  ;;; Run a body once for each item in a list.
  ;;;
  ;;; @syntax (foreach [name list] body ...)
  ;;; @param NAME name The variable to use as the item name.
  ;;; @param LIST list The list from which to get the items.
  ;;; @param BODY body The code to run for each list item.
  (define-syntax foreach
    (syntax-rules (cons)
      [(foreach [(cons key value) alist] body ...)
       (foreach [pair alist]
         (let [[key (car pair)] [value (cdr pair)]]
           (begin/var body ...)))]
      [(foreach [[key value] hash] body ...)
       (hash-table-for-each hash (lambda (key value) body ...))]
      [(foreach [name lst] body ...)
       (let loop [[remaining lst]]
         (unless (null? remaining)
           (let [[name (car remaining)]]
             (begin/var body ...))
           (loop (cdr remaining))))]))
  (define-syntax-indent foreach 1)

  ;;; Return #f if and only if ITEM appears in LIST.  Uses EQUAL? to
  ;;; perform the comparison.
  (define (member? item list)
    (if (null? list)
        #f
        (if (equal? item (car list))
            #t
            (member? item (cdr list)))))
  
  ;;; Convert any Scheme value to a string.
  (define (value->string value)
    (cond 
      ((string? value) value)
      ((object? value) (object->string value))
      (else
       (let ((str-port (open-output-string)))
         (write value str-port)
         (get-output-string str-port)))))
  
  ;;; Convert VALUES to strings and concatencate the result.
  (define (cat . values)
    (if (not (null? values))
        (string-append (value->string (car values)) (apply cat (cdr values)))
        ""))

  ;;; Convert VALUES to strings, concatenate the result, and convert it
  ;;; to a symbol.
  (define (symcat . values)
    (string->symbol (apply cat values)))

  ;;; Given a Swindle keyword of the form ":foo", strip the leading colon
  ;;; and return a symbol.
  (define (keyword-name value)
    (assert (keyword? value))
    (let [[str (symbol->string value)]]
      (string->symbol (substring str 1 (string-length str)))))

  ;;; Return #t if and only if KEY appears in TABLE.
  (define (hash-table-has-key? table key)
    (define result #t)
    (hash-table-get table key (lambda () (set! result #f)))
    result)

  ;;; Define a function NAME which can be called from any point within the
  ;;; LABEL construct to exit immediately from the LABEL.  The function
  ;;; will remain valid only until the LABEL is exited, but may be passed
  ;;; to subroutines, stored in global variables, etc., during that time.
  ;;;
  ;;; @syntax (label name body ...)
  ;;; @param NAME name The name of the function which can be called to
  ;;;   exit the body.
  ;;; @param BODY body The code within which the exit procedure may be called.
  (define-syntax label
    (syntax-rules ()
      [(label name body ...)
       (call-with-escape-continuation (lambda (name)
                                        (begin/var body ...)))]))
  (define-syntax-indent label 1)

  ;;; Call THUNK, and if an error occurs, pass it to REPORT-FUNC.
  (define (call-with-errors-blocked report-func thunk)
    (let* ((result (with-handlers ([void (lambda (exn) (cons #f exn))])
                     (cons #t (thunk))))
           (good? (car result))
           (exn-or-value (cdr result)))
      (if good?
          exn-or-value
          (begin
            ;; Print the backtrace to the debug log, but don't throw
            ;; an exception if there are any errors in the printing
            ;; process.
            (with-handlers [[void (lambda (exn) #f)]]
              (define strport (open-output-string))
              (print-error-trace strport exn-or-value)
              (debug-log (cat "Backtrace: " (exn-message exn-or-value) "\n"
                              "=============\n"
                              (get-output-string strport)
                              "=============")))
            (report-func (exn-message exn-or-value))
            #f))))

  ;;; If an error occurs in BODY, pass it to REPORT-FUNC.
  (define-syntax with-errors-blocked
    (syntax-rules ()
      [(with-errors-blocked (report-func) body ...)
       (call-with-errors-blocked report-func
                                 (lambda () (begin/var body ...)))]))
  (define-syntax-indent with-errors-blocked 1)

  ;;; Bind the multiple return values of EXPR to VALUES (a parameter list),
  ;;; and call BODY.
  (define-syntax with-values
    (syntax-rules ()
      [(with-values [values expr] body ...)
       (call-with-values (lambda () expr) (lambda values body ...))]))
  (define-syntax-indent with-values 1)
  
  ;;; A Dylan-style "curry", not be confused with Haskell's version.
  ;;; Returns a new function, with the first arguments of F filled in with
  ;;; values from ARGS1.
  (define (curry f . args1)
    (lambda args2
      (apply f (append args1 args2))))

  )
