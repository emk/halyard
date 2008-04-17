(module paths-test "halyard.ss"
  (require "halyard-unit.ss")
  
  ;; NOTE - this test case must be run on a card within the /tests group, as 
  ;; it has several tests that assume that the /tests group is currently a 
  ;; running node.
  (define-class %paths-test% (%test-case%)
    (test "A path should resolve to a running node by default."
      (assert ((@tests .resolve-path) .instance-of? %card-group%)))
    (test "A path should resolve to a static node when requested."
      (assert ((@start .resolve-path :running? #f) 
               .instance-of? (%card% .class))))
    (test "A path should act as an instance of %node-path%."
      (assert (@start .instance-of? %node-path%)))
    (test "A path should proxy .instance-of? to its running node."
      (assert (@tests .instance-of? %card-group%)))
    (test "A path should match its .to-string value."
      (assert-equals "@not-a-group/card" (@not-a-group/card .to-string)))
    (test "A path should proxy methods to its running node."
      ;; Test just that .node-state is defined, and doesn't give an error.
      (@tests .node-state)))
  
  (card /tests/paths (%test-suite% :tests (list %paths-test%)))
  )