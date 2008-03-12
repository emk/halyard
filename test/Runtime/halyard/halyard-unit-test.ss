(module halyard-unit-test (lib "halyard.ss" "halyard")
  (require (lib "halyard-unit.ss" "halyard"))
  
  (define (make-and-run-nth-test test-case-class n report)
    (define test-method (nth (test-case-test-methods test-case-class) n))
    (define test (make test-case-class :test-method test-method))
    (run-test-method test report)
    test)
  
  (define (make-and-run-first-test test-case-class report)
    (assert-equals 1 (length (test-case-test-methods test-case-class)))
    (make-and-run-nth-test test-case-class 0 report))
  
  (define-test-case <was-run-inner> ()
      [[was-run? #f]]
    (test "Mark that we were run."
      (set! (was-run? self) #t)))
  
  (define-test-case <was-run-test> () []
    (test "Test case method should be run."
      (let [[report (make-test-report)]]
        (define test (make-and-run-first-test <was-run-inner> report))
        (assert (was-run? test)))))

  (define-test-case <setup-invoked-inner> ()
      [[setup-invoked? #f]]
    (setup
      (set! (setup-invoked? self) #t))
    (test "Setup should be run before test methods."
      (assert (setup-invoked? self))))
  
  (define-test-case <setup-invoked-test> () []
    (test "Setup should be invoked before test method."
      (let [[report (make-test-report)]]
        (define test
          (make-and-run-first-test <setup-invoked-inner> report))
        (assert (setup-invoked? test)))))
  
  (define-test-case <teardown-invoked-inner> ()
      [[teardown-invoked? #f]]
    (teardown
      (set! (teardown-invoked? self) #t))
    (test "Teardown should not be invoked yet."
      (assert (not (teardown-invoked? self)))))
  
  (define-test-case <teardown-invoked-test> () []
    (test "Teardown should be invoked after test method."
      (let [[report (make-test-report)]]
        (define test
          (make-and-run-first-test <teardown-invoked-inner> report))
        (assert (teardown-invoked? test)))))
  
  (define-test-case <teardown-invoked-if-test-fails-inner> ()
      [[was-run? #f] [teardown-invoked? #f]]
    (teardown
      (set! (teardown-invoked? self) #t))
    (test "Cause an error in our test case to trigger teardown."
      (assert (not (teardown-invoked? self)))
      (set! (was-run? self) #t)
      (error "Expected to fail.")))
  
  (define-test-case <teardown-invoked-if-test-fails-test> () []
    (test "Teardown should be invoked even if test fails."
      (let [[report (make-test-report)]]
        (define test
          (make-and-run-first-test <teardown-invoked-if-test-fails-inner>
                                   report))
        (assert-equals #f (test-report-success? report))
        (assert (was-run? test))
        (assert (teardown-invoked? test)))))
  
  (define-test-case <test-methods-inner-1> () []
    (test "Blah." #f))
  
  (define *test-methods-inner-2-method-count* 0)
  (define-test-case <test-methods-inner-2> () []
    (test "Different blah." 
      (inc! *test-methods-inner-2-method-count*))
    (test "Frobozz."
      (inc! *test-methods-inner-2-method-count*)))
  
  (define-test-case <test-methods-test> () []
    (test "test-case-test-methods should contain our test methods"
      (let [[report (make-test-report)]]
        (define (assert-test-method-titles titles test-case-class)
          (define methods (test-case-test-methods test-case-class))
          (assert-equals titles (map (fn (meth) (test-method-title meth))
                                    (reverse methods))))
        (assert-test-method-titles '("Blah.") <test-methods-inner-1>)
        (assert-test-method-titles '("Different blah." "Frobozz.")
                                   <test-methods-inner-2>)
        
        (set! *test-methods-inner-2-method-count* 0)
        (run-tests <test-methods-inner-2> report)
        (assert-equals 2 *test-methods-inner-2-method-count*)
        )))
  
  (define-test-case <test-report-inner> () []
    (test "#1" #f)
    (test "#2" (error "Failed #2"))
    (test "#3" (error "Failed #3")))
  
  (define-test-case <test-report-test> () []
    (test "Test report should include successes and failures."
      (let [[report (make-test-report)]]
        (run-tests <test-report-inner> report)
        (assert-equals 3 (test-report-run-count report))
        (assert-equals 1 (test-report-success-count report))
        (assert-equals 2 (test-report-failure-count report))
        (define failures (test-report-failures report))
        (assert-equals '("#2" "#3")
                       (sort (map test-failure-title failures) string<?))
        (assert-equals '("Failed #2" "Failed #3")
                       (sort (map test-failure-message failures) string<?))
        )))
  
  ;; TODO - Reuse throughout.
  (define (push-event! event test)
    (set! (test-events test) (cons event (test-events test))))
  
  (define-test-case <inheritance-inner-1> ()
      [[test-events '()]]
    (setup
      (push-event! 'setup self))
    (teardown
      (push-event! 'teardown self))
    (test "Test B."
      (push-event! 'test-b self)))
  
  (define-test-case <inheritance-inner-2> (<inheritance-inner-1>) []
    (test "Test A."
      (push-event! 'test-a self)))
  
  (define-test-case <inheritence-test> () []
    (test "Test case should include parent's tests."
      (let [[report (make-test-report)]]
        (define (assert-events-for-nth events n)
          (define test
            (make-and-run-nth-test <inheritance-inner-2> n report))
          (assert-equals events (reverse (test-events test))))
        (assert-events-for-nth '(setup test-a teardown) 0)
        (assert-events-for-nth '(setup test-b teardown) 1))))
  
  (define-test-case <define-test-case-helper-test> () []
    (test "Helper macros should expand to defmethods"
      (assert-macro-expansion
       (let [[var expr]] body1 body2)
       (with-captured-variable [var expr] body1 body2))
      (assert-macro-expansion 
       (defmethod (setup-test (self <my-class>))
         (call-next-method)
         (with-captured-variable [self self] body1 body2))
       (define-test-case-helper <my-class> (setup body1 body2)))
      (assert-macro-expansion 
       (defmethod (teardown-test (self <my-class>))
         (with-captured-variable [self self] body1 body2)
         (call-next-method))
       (define-test-case-helper <my-class> (teardown body1 body2)))
      (assert-macro-expansion
       (add-test-method! <my-class> "foo"
                         (fn (self)
                           (with-captured-variable [self self]
                             body1 body2)))
       (define-test-case-helper <my-class> (test "foo" body1 body2)))))
  
  (define-test-case <define-test-case-test> () []
    (test "define-test-case should expand to defclass and helpers."
      (assert-macro-expansion
       (begin
         (defclass <my-test> (<my-parent>)
           (a :initvalue 1 :accessor a)
           (b :initvalue 2 :accessor b)
           :metaclass <test-case-metaclass>)
         (define-test-case-helper <my-test> (setup body1))
         (define-test-case-helper <my-test> (test "Foo!" body2)))
       (define-test-case <my-test> (<my-parent>) [[a 1] [b 2]]
         (setup body1) (test "Foo!" body2)))
      (assert-macro-expansion
       (define-test-case <my-child> (<test-case>) [[a 1]] (setup body1))
       (define-test-case <my-child> () [[a 1]] (setup body1)))))
  
  (card halyard-unit-test
      (%test-suite%
       :tests (list <was-run-test> <setup-invoked-test>
                    <define-test-case-helper-test>
                    <define-test-case-test>
                    <teardown-invoked-test>
                    <teardown-invoked-if-test-fails-test>
                    <test-methods-test>
                    <test-report-test>
                    <inheritence-test>)))
  )