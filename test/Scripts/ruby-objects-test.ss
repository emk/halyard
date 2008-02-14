(module ruby-objects-test (lib "5l.ss" "5L")

  (require (lib "tamale-unit.ss" "5L"))

  (define-class %advised% ()
    (attr events '() :writable? #t)
    (def (record-event! name)
      (set! (.events) (append (.events) (list name))))

    (def (foo)
      (.record-event! 'foo)
      'foo-result)
    (advise before (foo)
      (.record-event! 'before-foo))
    (advise after (foo)
      (.record-event! 'after-foo))
    
    (def (call-super-from-advice)
      (void))
    (advise after (call-super-from-advice)
      (super))
    
    ;; If we _do_ decide that ADVISE AROUND is worth the trouble, here is our
    ;; proposed syntax.  But this actually takes a bit more machinery to
    ;; implement, and offers a greater opportunity for evil.  So we'll try to
    ;; live without it for now.
    
    ;;(def (double x)
    ;;  (.record-event! 'double)
    ;;  (* 2 x))

    ;;(advise around (double x)
    ;;  (.record-event! 'before-double)
    ;;  (let [[result (original x)]]
    ;;    (.record-event! 'after-double)
    ;;    result))

    (def (not-overridden)
      (.record-event! 'not-overridden))
    )

  (define-class %another-advised% (%advised%)
    (def (foo)
      (.record-event! 'another-foo-1)
      (super)
      (.record-event! 'another-foo-2))
    (advise before (foo)
      (.record-event! 'before-another-foo))
    (advise after (foo)
      (.record-event! 'after-another-foo))
    
    (advise after (not-overridden)
      (.record-event! 'after-not-overridden))
    )
  
  (define-test-case <advise-test> () []
    (test "ADVISE should run code before and after the original method"
      (define advised (%advised% .new))
      (assert-equals '() (advised .events))
      (advised .foo)
      (assert-equals '(before-foo foo after-foo) (advised .events)))
    
    (test "ADVISE should not affect the return value"
      (define advised (%advised% .new))
      (assert-equals 'foo-result (advised .foo)))
    
    (test "ADVISE should interleave with method calls when using SUPER"
      (define another (%another-advised% .new))
      (another .foo)
      (assert-equals '(before-another-foo another-foo-1
                       before-foo foo after-foo
                       another-foo-2 after-another-foo)
                     (another .events)))
    
    (test "ADVISE AFTER should not pass SUPER to advice"
      (define advised (%advised% .new))
      (assert-raises exn:fail? (advised .call-super-from-advice)))
    
    (test "ADVISE AFTER should work in the absense of an actual method"
      (define advised (%advised% .new))
      (advised .not-overridden)
      (assert-equals '(not-overridden) (advised .events))
      
      (define another (%another-advised% .new))
      (another .not-overridden)
      (assert-equals '(not-overridden after-not-overridden) (another .events)))
    )
  
  (card advise-test (%test-suite%)
    (value tests (list <advise-test>)))
  
  (define-class %initialize-without-super% (%custom-element%)
    (def (initialize &rest args)
      (debug-log "Calling the bad initialize method.")))
  
  (define-test-case <initialize-without-super-test> () []
    (test-elements "Failing to call SUPER shouldn't cause an infinite loop"
      (assert-raises exn:fail?
                     (%initialize-without-super% .new
                        :name 'bad-init
                        :at (point 0 0)
                        :shape (shape 200 300)))))
  
  (card initialize-without-super-test (%test-suite%)
    (value tests (list <initialize-without-super-test>)))
  
  )
