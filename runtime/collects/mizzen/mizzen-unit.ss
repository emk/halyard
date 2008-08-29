;; @BEGIN_LICENSE
;;
;; Mizzen - Scheme object system
;; Copyright 2006-2008 Trustees of Dartmouth College
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as
;; published by the Free Software Foundation; either version 2.1 of the
;; License, or (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;; 
;; You should have received a copy of the GNU Lesser General Public
;; License along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
;; USA.
;;
;; @END_LICENSE

(module mizzen-unit "mizzen.ss"
  (require-for-syntax "capture.ss")
  
  ;;=======================================================================
  ;;  Text-based test result reporting
  ;;=======================================================================
  
  (provide run-tests)
  
  (define (display-line &rest args)
    (display (apply cat (append args '("\n")))))
  
  (define (run-tests test-cases)
    (let [[report (%test-report% .new)]]
      (foreach [test-class test-cases]
        (test-class .run-tests report))
      (if (report .success?)
        (display-line "OK")
        (begin
          (display-line "FAILED")
          (foreach [failure (report .failures)]
            (display-line (failure .title))
            (display-line (failure .message))
            (display-line))))))
  
  
  ;;=======================================================================
  ;;  Test cases and supporting classes
  ;;=======================================================================
    
  (provide %test-failure%
           %test-report%
           %test-method%
           %test-case%
           test setup-test teardown-test
           with-captured-variable
           assert-equals assert-macro-expansion assert-raises 
           assert-raises-message)
  
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
            (append (.failures)
                    (list 
                     (%test-failure% .new :test-case test-case 
                                     :exception exception)))))
    
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
      (with-handlers [[exn:fail?
                       (fn (exn) 
                         (report .report-failure! self exn))]]
        (dynamic-wind
            (fn () (.setup-test))
            (fn ()
              (.run-test-method-inner)
              (report .report-success!))
            (fn () (.teardown-test)))))

    ;;; Run the actual test method itself, with setup and teardown handled
    ;;; elsewhere.
    (def (run-test-method-inner)
      (instance-exec self ((.test-method) .method))))
  
  ;;; Execute BODY, capturing VAR and binding it to the result of
  ;;; EXPR.  Used to implement SELF in test cases.
  (define-syntax (with-captured-variable stx)
    (syntax-case stx ()
      [(_ [var expr] . body)
       (let* [[new-var-name (syntax-object->datum #'var)]
              [new-var (make-capture-var/ellipsis #'body new-var-name)]]
         (quasisyntax/loc stx
           (let ((#,new-var expr)) . body)))]))
  
  ;; TODO - factor out the common code
  (define-syntax test
    (syntax-rules ()
      [[_ description . body]
       (.add-test-method! description
         (method () . body))]))
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

  ;;; Assert that an expression returns the expected value.
  (define-syntax assert-equals 
    (syntax-rules ()
      [(_ expected value)
       (let [[e expected] [v value]]
         (unless (equals? e v)
           (error (cat "Expected <" e ">, got <" v "> in <" 'value ">"))))]))
  
  ;;; Assert that the specified macro, expanded once, returns the
  ;;; expected source code.  This is most useful for macros which
  ;;; have a well-defined mapping to a public API.
  (define-syntax assert-macro-expansion
    (syntax-rules ()
      [(_ expansion source)
       (assert-equals
        'expansion
        (syntax-object->datum (expand-once #'source)))]))

  ;;; Assert that CODE raises an exception matching PREDICATE.
  (define-syntax assert-raises
    (syntax-rules ()
      [(_ predicate code)
       (unless (with-handlers [[predicate (lambda (exn) #t)]]
                 code
                 #f)
         (error (cat "Expected " 'code " to raise " 'predicate)))]))
  
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
  )