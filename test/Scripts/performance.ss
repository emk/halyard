(module performance (lib "halyard.ss" "halyard")
  (require (file "base.ss"))
  (sequence performance)
  
  (define-class %parent% ()
    (def (test-method arg1 arg2 &rest args)
      (void)))
  
  (define-class %child% (%parent%)
    (def (test-method arg1 arg2 &rest args)
      (super)))
  
  (define-class %grandchild% (%child%)
    (def (test-method arg1 arg2 &rest args)
      (super)))
  
  ;; TODO - factor out into a performance testing class.
  (card performance/dispatch (%fancy-white-test-card% :title "Not yet run")
    (run 
      ;; NOTE - GC seems to dominate this test; if you remove this call to 
      ;; collect-garbage, the results will vary widly by a factor of 100, 
      ;; apparently based on whether GC has happened or not. If you leave the 
      ;; collec-garbage call in, this will always run fast with 10,000 
      ;; iterations, and always run 100 times slower with 100,000 iterations.
      (collect-garbage)
      (define start (current-milliseconds))
      (define grandchild (%grandchild% .new))
      (define count 10000)
      (while (> count 0)
        (grandchild
         .test-method 'foo 'bar 'baz 'quuz 'zot 'tiddle 'hello 
                      'goodbye 'something 'something-else 'lorem 'ipsum 'dolor
                      'sit 'amet 1 2 3 4 5 6 7 8 9 10)
        (set! count (- count 1)))
      (define end (current-milliseconds))
      (set! ((.title-elem) .text) (cat "Test took " (- end start) 
                                       " milliseconds."))))
  )