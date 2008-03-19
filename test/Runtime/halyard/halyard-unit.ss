(module halyard-unit (lib "halyard.ss" "halyard")
  (require-for-syntax (lib "capture.ss" "halyard"))

  (provide %test-suite%)
  
  (define-stylesheet $halyard-unit-style
    :family "Nimbus Sans L"
    :size 16
    :color (color 0 0 0)
    :highlight-color (color #x00 #x00 #xC0))
  
  (define-stylesheet $halyard-unit-title-style
    :base $halyard-unit-style
    :size 24)
  
  (define-stylesheet $halyard-unit-passed-style
    :base $halyard-unit-style
    :size 36
    :flags '(bold)
    :color (color #x00 #xC0 #x00))
  
  (define-stylesheet $halyard-unit-failed-style 
    :base $halyard-unit-passed-style
    :color (color #xC0 #x00 #x00))
  
  ;;; Display the results of a set of tests on a card.
  (define-class %test-suite% (%card%)
    (attr tests)

    (def (setup)
      (super)
      (clear-dc (color #xFF #xFF #xFF))
      ;; Draw a title on the card (making it easier to tell when each
      ;; test-suite card is loaded).
      (draw-text (rect 30 30 800 100) $halyard-unit-title-style
                 (cat "Card: " (.full-name))))

    (def (run)
      (super)
      (let [[report (%test-report% .new)]]
        (foreach [test-class (.tests)]
          (test-class .run-tests report))
        (.report-test-results report)))

    (def (report-test-results report)
      (define (draw-result style text)
      (draw-text (rect 100 100 700 175) style text))
      (if (report .success?)
          (draw-result $halyard-unit-passed-style "OK")
          (begin
            (draw-result $halyard-unit-failed-style "FAILED")
            (draw-text (rect 100 175 700 500) $halyard-unit-style
                       (apply string-append
                              (map (fn (failure)
                                     (string-append
                                      "<h><b>" (string->xml 
                                                (failure .title))
                                      "</b></h>\n"
                                      (string->xml 
                                       (failure .message))
                                      "\n\n"))
                                   (report .failures)))))))
    )

  ;;========================================================================

  (provide %test-failure%
           %test-report%
           %test-method%
           %test-case%
           test test-elements setup-test teardown-test
           with-captured-variable
           assert-equals assert-macro-expansion assert-raises 
           assert-raises-message
           fixture-dir)
  
  ;;; Data about a single failed test case.
  (define-class %test-failure% ()
    (attr test-case) 
    (attr exception)
    
    ;;; The title of the failed test case.
    (def (title)
      (((.test-case) .test-method) .title))
  
    ;;; The message associated with the failed test case.
    (def (message)
      (exn-message (.exception))))
  
  ;;; Data collected from an entire test run.
  (define-class %test-report% ()
    (attr success? #t :writable? #t)
    (attr success-count 0 :writable? #t)
    (attr failures '() :writable? #t)
    
    ;;; The number of failures in a test run.
    (def (failure-count)
      (length (.failures)))
  
    ;;; The number of tests executed in a test run.
    (def (run-count)
      (+ (.success-count) (.failure-count)))
  
    ;;; Add a failed test case to the report.
    (def (report-failure! test-case exception)
      (set! (.success?) #f)
      (set! (.failures) 
            (cons (%test-failure% .new :test-case test-case 
                                       :exception exception) 
                  (.failures))))
    
    ;;; Add a successful test case to the report.
    (def (report-success!)
      (set! (.success-count) 
            (1+ (.success-count)))))
  
  ;;; A single test method associated with a test case.
  (define-class %test-method% ()
    (attr title) 
    (attr method))
  
  ;;; A test case consists of a setup method, zero or more test
  ;;; methods, and a teardown method.
  (define-class %test-case% ()
    (attr test-method)

    (with-instance (.class)
      ;;; %test-case% objects are annotated with a list of associated
      ;;; test methods.
      (attr direct-test-methods '() :writable? #t)

      ;;; Find all test methods associated with this class and all 
      ;;; superclasses.  Does not support multiple inheritance.
      (def (test-methods)
        (define direct (.direct-test-methods))
        (if (eq? self %test-case%)
          direct
          (append direct ((.superclass) .test-methods))))
  
      ;;; Add a test method to a %test-case% class.
      (def (add-test-method! title function)
        (set! (.direct-test-methods)
              (cons (%test-method% .new
                      :title title 
                      :method function)
                    (.direct-test-methods))))
  
      ;;; Run each test method associated with a given %test-case% class.
      (def (run-tests report)
        (foreach [meth (.test-methods)]
          ((.new :test-method meth) .run-test-method report))))
  
    ;;; Prepare an instance of %test-case% to run a test method.
    ;;; Called once before each test method in the test case.
    (def (setup-test)
      (void))

    ;;; Clean up an instance of %test-case% after running a test method.
    ;;; Called once after each test method in the test case.
    (def (teardown-test)
      (void))

    ;;; Run a single test case method, handling setup, teardown and reporting.
    (def (run-test-method report)
      (define test-method (.test-method))
      (with-handlers [[exn:fail?
                       (fn (exn) 
                         (report .report-failure! self exn))]]
        (dynamic-wind
            (fn () (.setup-test))
            (fn ()
              (instance-exec self (test-method .method))
              (report .report-success!))
            (fn () (.teardown-test))))))
  
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
  
  ;;; Execute BODY, capturing VAR and binding it to the result of
  ;;; EXPR.  Used to implement SELF in test cases.
  (define-syntax (with-captured-variable stx)
    (syntax-case stx ()
      [(_ [var expr] . body)
       (let* [[new-var-name (syntax-object->datum #'var)]
              [new-var (make-capture-var/ellipsis #'body new-var-name)]]
         (quasisyntax/loc stx
           (let ((#,new-var expr)) . body)))]))
  (define-syntax-indent with-captured-variable 1)
  
  ;; TODO - factor out the common code
  (define-syntax test
    (syntax-rules ()
      [[_ description . body]
       (.add-test-method! description
         (method () . body))]))
  (define-syntax test-elements
    (syntax-rules ()
      [[_ description . body]
       (.add-test-method! description
         (method () 
           (with-temporary-parent . body)))]))
  (define-syntax setup-test
    (syntax-rules ()
      [[_ . body]
       (.define-method 'setup-test
         (method () 
           (super) 
           (instance-exec self (method () . body))))]))
  (define-syntax teardown-test
    (syntax-rules ()
      [[_ . body]
       (.define-method 'teardown-test
         (method () 
           (super) 
           (instance-exec self (method () . body))))]))

  (define-syntax-indent setup-test 0)
  (define-syntax-indent teardown-test 0)
  (define-syntax-indent test 1)
  (define-syntax-indent test-elements 1)
  
  ;;; Assert that an expression returns the expected value.
  (define-syntax assert-equals 
    (syntax-rules ()
      [(_ expected value)
       (let [[e expected] [v value]]
         (unless (equals? e v)
           (error (cat "Expected <" e ">, got <" v "> in <" 'value ">"))))]))
  (define-syntax-indent assert-equals function)
  
  ;;; Assert that the specified macro, expanded once, returns the
  ;;; expected source code.  This is most useful for macros which
  ;;; have a well-defined mapping to a public API.
  (define-syntax assert-macro-expansion
    (syntax-rules ()
      [(_ expansion source)
       (assert-equals
        'expansion
        (syntax-object->datum (expand-once #'source)))]))
  (define-syntax-indent assert-macro-expansion function)

  ;;; Assert that CODE raises an exception matching PREDICATE.
  (define-syntax assert-raises
    (syntax-rules ()
      [(_ predicate code)
       (unless (with-handlers [[predicate (lambda (exn) #t)]]
                 code
                 #f)
         (error (cat "Expected " 'code " to raise " 'predicate)))]))
  (define-syntax-indent assert-raises 1)
  
  ;;; Assert that CODE raises an exception matching PREDICATE with exception
  ;;; message matching MSG-REGEXP.
  ;;; NOTE: this assertion only catches exceptions of type exn:fail.
  (define-syntax assert-raises-message
    (syntax-rules ()
      [(_ predicate msg-regexp code)
       (let* [[exn #f]
              [result         
               (with-handlers [[exn:fail? (lambda (e) (set! exn e))]]
                              code
                              #f)]
              [error-out
               (lambda (text)
                 (error (cat "Expected " 'code " to raise " 'predicate 
                             " with message matching '" msg-regexp "'; "
                             text)))]
              [exn-and-message
               (lambda ()
                 (cat "Got exception " exn " with message '" 
                      (exn-message exn) "' instead."))]]
         (cond
          ;; If there is no exception:
          [(not result)
           (error-out "No error was raised.")]
          ;; If there is an incorrect exception
          [(not (predicate exn))
           (error-out (cat "Incorrect exception! " (exn-and-message)))]
          ;; If there is the correct exception, but with an incorrect message
          [(not (regexp-match msg-regexp (exn-message exn)))
           (error-out (cat "Incorrect message! " (exn-and-message)))]
          ;; If there is the correct exception, with the correct message, we
          ;; succeed:
          [else
           #t]))]))
  (define-syntax-indent assert-raises-message 1)
  
  (define (fixture-dir name)
    (build-path (current-directory) "Runtime" "halyard" (cat name "-fixtures")))
  )
