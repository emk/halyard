;; @BEGIN_LICENSE
;;
;; Halyard - Multimedia authoring and playback system
;; Copyright 1993-2009 Trustees of Dartmouth College
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

(module paths-test (lib "halyard.ss" "halyard")
  (require (lib "halyard-unit.ss" "halyard"))
  
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
      (assert-equals "@not-a-group/card" (@not-a-group/card .to-string))
      (assert-equals "@/start" (@/start .to-string)))
    (test "A path should properly report its name in errors."
      (assert-raises-message exn:fail? "relative path.*@not-a-path/at-all"
        (@not-a-path/at-all .resolve-path))
      (assert-raises-message exn:fail? 
        "absolute path.*@/not-really-a-path/sorry"
        (@/not-really-a-path/sorry .resolve-path))
      ;; We have a special error message if the running node doesn't exist,
      ;; but a corresponding static node does.
      (assert-raises-message exn:fail?
        "@/start.*static node"
        (@/start .instance-of? %card%)))
    (test "A path should proxy methods to its running node."
      ;; Test just that .node-state is defined, and doesn't give an error.
      (@tests .node-state))
    (test "A path with a leading / should resolve as an absolue path."
      (assert-equals /tests (@/tests .resolve-path :running? #f))))

  (define-class %static-node-test% (%test-case%)
    (test "A bare node path should resolve to a static node."
      (assert (static-node? /start))
      (assert (static-node? /tests))
      (assert (static-node? /tests/paths)))
    (test "Trying to use a bare path for a non-existant node should fail."
      (assert-raises-message exn:fail? "/frobnozzle/gunkstuff"
        /frobnozzle/gunkstuff)))

  (define-class %resolve-static-node-path-test% (%test-case%)
    (test "Calling resolve-static-node on a path should return underlying node"
      (assert-equals /start (@/start .resolve-static-node))
      (assert-equals /start (@start .resolve-static-node))))

  (define-class %resolve-running-node-path-test% (%test-case%)
    (test "Calling resolve-running-node on a path should return underlying node"
      (assert-equals (current-card) (@/tests/paths .resolve-running-node))
      (assert-equals (current-card) (@tests/paths .resolve-running-node))))
  
  (card /tests/paths
      (%test-suite% :tests (list %paths-test% 
                                 %static-node-test%
                                 %resolve-static-node-path-test%
                                 %resolve-running-node-path-test%)))
  )