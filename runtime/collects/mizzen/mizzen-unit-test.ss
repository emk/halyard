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

(module mizzen-unit-test "mizzen.ss"
  (require "mizzen-unit.ss")
  
  (provide make-and-run-nth-test make-and-run-first-test
           assert-report-successful
           %was-run-test% %setup-invoked-test%
           %define-test-case-helper-test%
           %teardown-invoked-test%
           %teardown-invoked-if-test-fails-test%
           %run-test-method-inner-test%
           %test-methods-test%
           %test-report-test%
           %inheritence-test%
           $all-mizzen-unit-tests)
  
  (define (make-and-run-nth-test test-case-class n report)
    (define test-method (nth (test-case-class .test-methods) n))
    (define test (test-case-class .new :test-method test-method))
    (test .run-test-method report)
    test)
  
  (define (make-and-run-first-test test-case-class report)
    (assert-equals 1 (length (test-case-class .test-methods)))
    (make-and-run-nth-test test-case-class 0 report))
  
  ;; Asserts that a report is successful, and errors with the first
  ;; failure message if not.
  (define (assert-report-successful report)
    (unless (report .success?)
      (error ((nth (report .failures) 0) .message))))

  (define-class %was-run-inner% (%test-case%)
    (attr was-run? #f :writable? #t)
    (test "Mark that we were run."
      (set! (.was-run?) #t)))
  
  (define-class %was-run-test% (%test-case%) 
    (test "Test case method should be run."
      (let [[report (%test-report% .new)]]
        (define test (make-and-run-first-test %was-run-inner% report))
        (assert-report-successful report)
        (assert (test .was-run?)))))

  (define-class %setup-invoked-inner% (%test-case%)
    (attr setup-invoked? #f :writable? #t)
    (setup-test
      (set! (.setup-invoked?) #t))
    (test "Setup should be run before test methods."
      (assert (.setup-invoked?))))
  
  (define-class %setup-invoked-test% (%test-case%) 
    (test "Setup should be invoked before test method."
      (let [[report (%test-report% .new)]]
        (define test
          (make-and-run-first-test %setup-invoked-inner% report))
        (assert-report-successful report)
        (assert (test .setup-invoked?)))))
  
  (define-class %teardown-invoked-inner% (%test-case%)
    (attr teardown-invoked? #f :writable? #t)
    (teardown-test
      (set! (.teardown-invoked?) #t))
    (test "Teardown should not be invoked yet."
      (assert (not (.teardown-invoked?)))))
  
  (define-class %teardown-invoked-test% (%test-case%) 
    (test "Teardown should be invoked after test method."
      (let [[report (%test-report% .new)]]
        (define test
          (make-and-run-first-test %teardown-invoked-inner% report))
        (assert-report-successful report)
        (assert (test .teardown-invoked?)))))
  
  (define-class %teardown-invoked-if-test-fails-inner% (%test-case%)
    (attr was-run? #f :writable? #t)
    (attr teardown-invoked? #f :writable? #t)
    (teardown-test
      (set! (.teardown-invoked?) #t))
    (test "Cause an error in our test case to trigger teardown."
      (assert (not (.teardown-invoked?)))
      (set! (.was-run?) #t)
      (error "Expected to fail.")))
  
  (define-class %teardown-invoked-if-test-fails-test% (%test-case%) 
    (test "Teardown should be invoked even if test fails."
      (let [[report (%test-report% .new)]]
        (define test
          (make-and-run-first-test %teardown-invoked-if-test-fails-inner%
                                   report))
        (assert-equals #f (report .success?))
        (assert (test .was-run?))
        (assert (test .teardown-invoked?)))))

  (define-class %run-test-method-inner-inner% (%test-case%)
    (attr setup-invoked? #f :writable? #t)
    (attr teardown-invoked? #f :writable? #t)
    (setup-test
      (set! (.setup-invoked?) #t))
    (teardown-test
      (set! (.teardown-invoked?) #t))
    (test "Setup and teardown should not have been called." 
      (assert (not (.setup-invoked?)))
      (assert (not (.teardown-invoked?)))))

  (define-class %run-test-method-inner-test% (%test-case%)
    (test "Setup/teardown should not be called from .RUN-TEST-METHOD-INNER."
      (define test (%run-test-method-inner-inner% .new 
                     :test-method (first (%run-test-method-inner-inner% 
                                          .test-methods))))
      (test .run-test-method-inner)
      (assert (not (test .setup-invoked?)))
      (assert (not (test .teardown-invoked?)))))
  
  (define-class %test-methods-inner-1% (%test-case%) 
    (test "Blah." #f))
  
  (define *test-methods-inner-2-method-count* 0)
  (define-class %test-methods-inner-2% (%test-case%) 
    (test "Different blah." 
      (inc! *test-methods-inner-2-method-count*))
    (test "Frobozz."
      (inc! *test-methods-inner-2-method-count*)))
  
  (define-class %test-methods-test% (%test-case%) 
    (test ".test-methods should contain our test methods"
      (let [[report (%test-report% .new)]]
        (define (assert-test-method-titles titles test-case-class)
          (define methods (test-case-class .test-methods))
          (assert-equals titles (map (fn (meth) (meth .title))
                                    (reverse methods))))
        (assert-test-method-titles '("Blah.") %test-methods-inner-1%)
        (assert-test-method-titles '("Different blah." "Frobozz.")
                                   %test-methods-inner-2%)
        
        (set! *test-methods-inner-2-method-count* 0)
        (%test-methods-inner-2% .run-tests report)
        (assert-report-successful report)
        (assert-equals 2 *test-methods-inner-2-method-count*)
        )))
  
  (define *failure-order* '())
  
  (define-class %test-report-inner% (%test-case%) 
    (def (record-failure message)
      (set! *failure-order* (append *failure-order* (list message)))
      (error message))
    (test "#1" #f)
    (test "#2" (.record-failure "Failed #2"))
    (test "#3" (.record-failure "Failed #3")))
  
  (define-class %test-report-test% (%test-case%) 
    (test "Test report should include successes and failures."
      (let [[report (%test-report% .new)]
            [failures #f]]
        (fluid-let [[*failure-order* '()]]
          (%test-report-inner% .run-tests report)
          (assert-equals 3 (report .run-count))
          (assert-equals 1 (report .success-count))
          (assert-equals 2 (report .failure-count))
          (set! failures (report .failures))
          (assert-equals '("#2" "#3")
                         (sort (map (fn (f) (f .title)) failures) string<?))
          (assert-equals '("Failed #2" "Failed #3")
                         (sort (map (fn (f) (f .message)) failures) string<?))
          (assert-equals *failure-order* (map (fn (f) (f .message)) failures))
        ))))
  
  ;; TODO - Reuse throughout.
  (define (push-event! event test)
    (set! (test .test-events) (cons event (test .test-events))))
  
  (define-class %inheritance-inner-1% (%test-case%)
    (attr test-events '() :writable? #t)
    (setup-test
      (push-event! 'setup self))
    (teardown-test
      (push-event! 'teardown self))
    (test "Test B."
      (push-event! 'test-b self)))
  
  (define-class %inheritance-inner-2% (%inheritance-inner-1%) 
    (test "Test A."
      (push-event! 'test-a self)))
  
  (define-class %inheritence-test% (%test-case%) 
    (test "Test case should include parent's tests."
      (let [[report (%test-report% .new)]]
        (define (assert-events-for-nth events n)
          (define test
            (make-and-run-nth-test %inheritance-inner-2% n report))
          (assert-equals events (reverse (test .test-events))))
        (assert-events-for-nth '(setup test-a teardown) 0)
        (assert-events-for-nth '(setup test-b teardown) 1)
        (assert-report-successful report))))
  
  (define-class %define-test-case-helper-test% (%test-case%) 
    (test "WITH-CAPTURED-VARIABLE should expand to LET."
      (assert-macro-expansion
       (let [[var expr]] body1 body2)
       (with-captured-variable [var expr] body1 body2)))
    (test "TEST should expand to .ADD-TEST-METHOD!"
      (assert-macro-expansion
       (.add-test-method! "Add one to two." (method () (+ 1 2)))
       (test "Add one to two." (+ 1 2))))
    (test "SETUP-TEST should expand to .DEFINE-METHOD"
      (assert-macro-expansion
       (.define-method 'setup-test
         (method () 
           (super) 
           (instance-exec self (method () 'foo))))
       (setup-test 'foo)))
    (test "TEARDOWN-TEST should expand to .DEFINE-METHOD"
      (assert-macro-expansion
       (.define-method 'teardown-test
         (method () 
           (super) 
           (instance-exec self (method () 'bar))))
       (teardown-test 'bar))))
  
  (define-class %warning-capture-inner% (%test-case%)
    (test "WARNING"
      ((current-warning-handler) "Oops.")))

  (define-class %warning-capture-test% (%test-case%)
    (test "Warning should cause test failure."
      (let [[report (%test-report% .new)]]
        (define test
          (make-and-run-first-test %warning-capture-inner% report))
        (assert-equals #f (report .success?))
        (let [[failure (first (report .failures))]]
          (assert-equals "WARNING" (failure .title))
          (assert-equals "Oops." (failure .message))))))

  (define $all-mizzen-unit-tests 
    (list %was-run-test% %setup-invoked-test%
          %define-test-case-helper-test%
          %teardown-invoked-test%
          %teardown-invoked-if-test-fails-test%
          %run-test-method-inner-test%
          %test-methods-test%
          %test-report-test%
          %inheritence-test%
          %warning-capture-test%))
  )