(module data-file-test (lib "halyard.ss" "halyard")
  (require (lib "halyard-unit.ss" "halyard"))
  (require (lib "data-file.ss" "halyard"))

  (define-user-pref *example-user-pref-for-testing* 'default-value)
  
  (define-class %with-temporary-user-data-test% (%test-case%)
    (test "with-temporary-user-data should use brand-new data file"
      (with-temporary-user-data ['other-fake-user-for-testing]
        (set! *example-user-pref-for-testing* 'not-default-value)
        (with-temporary-user-data []
          (assert-equals 'default-value *example-user-pref-for-testing*))
        (assert-equals 'not-default-value *example-user-pref-for-testing*))))
  
  (define-class %wrapping-run-test-method-test% (%test-case%)
    (def (run-test-method report)
      (with-temporary-user-data []
        (super)))
    (test "Wrapping run-test-method with with-temporary-user-data should work"
      (assert-equals 'default-value *example-user-pref-for-testing*)))
  
  (card /tests/data-file
      (%test-suite%
       :tests (list %with-temporary-user-data-test%
                    %wrapping-run-test-method-test%)))
  
  )
