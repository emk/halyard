;; PORTED
(module ruby-objects-test (lib "halyard.ss" "halyard")

  (require (lib "halyard-unit.ss" "halyard"))
  (require (lib "test-elements.ss" "halyard"))
  (require (lib "ruby-objects-test.ss" "mizzen"))

  (group /ruby-objects-test)
  
  
  ;;=======================================================================
  ;;  Test runner cards for mizzen test cases
  ;;=======================================================================
  
  (card /ruby-objects-test/classes
      (%test-suite%
       :tests (list %swindle-class-operators%
                    %universal-class-operators%)))

  (card /ruby-objects-test/objects
      (%test-suite%
       :tests (list %ruby-object-test%
                    %ruby-new-test%
                    %ruby-responds-to-test%
                    %ruby-metaclass-test%)))
  
  (card /ruby-objects-test/advise-test (%test-suite%)
    (value tests (list %advise-test%)))
  
  (card /ruby-objects-test/error-message-test (%test-suite%)
    (value tests (list %error-message-test-case%)))


  ;;=======================================================================
  ;;  Initialize without SUPER
  ;;=======================================================================
  ;;  This used to be a nasty little bug where we threw an infinite number
  ;;  of errors while trying to handle the previous error.
  
  (define-class %initialize-without-super% (%custom-element%)
    (def (initialize &rest args)
      (debug-log "Calling the bad initialize method.")))
  
  (define-class %initialize-without-super-test% (%element-test-case%)
    (test "Failing to call SUPER shouldn't cause an infinite loop"
      (assert-raises exn:fail?
                     (%initialize-without-super% .new
                        :name 'bad-init
                        :at (point 0 0)
                        :shape (shape 200 300)))))
  
  (card /ruby-objects-test/initialize-without-super-test (%element-test-suite%)
    (value tests (list %initialize-without-super-test%)))
  )
