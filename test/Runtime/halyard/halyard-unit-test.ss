(module halyard-unit-test (lib "halyard.ss" "halyard")
  (require (lib "halyard-unit.ss" "halyard"))
  (require (lib "test-elements.ss" "halyard"))
  (require (lib "mizzen-unit-test.ss" "mizzen"))
  
  (define-class %test-elements-inner% (%test-case%)
    (attr box-name 'foo :writable? #t)
    (setup
      (set! (.box-name) 'bar))
    (test-elements "Element test."
      (%box% .new :bounds (rect 0 0 10 10) :name (.box-name))))
  
  (define-class %test-elements-test% (%test-case%)
    (test "TEST-ELEMENTS should expand to .ADD-TEST-METHOD!"
      (assert-macro-expansion
       (.add-test-method! "Do nothing." 
         (method () (with-temporary-parent (void))))
       (test-elements "Do nothing." (void))))
    (test "TEST-ELEMENTS should leave the current card empty."
      (let [[report (%test-report% .new)]]
        (make-and-run-first-test %test-elements-inner% report)
        (assert-report-successful report)
        (assert-equals '() (node-elements (current-card))))))
  
  (card halyard-unit-test
      (%test-suite%
       :tests (list* %test-elements-test% $all-mizzen-unit-tests)))
  )