;; @BEGIN_LICENSE
;;
;; Halyard - Multimedia authoring and playback system
;; Copyright 1993-2008 Trustees of Dartmouth College
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

(module halyard-unit-test (lib "halyard.ss" "halyard")
  (require (lib "kernel.ss" "halyard/private"))
  (require (lib "halyard-unit.ss" "halyard"))
  (require (lib "mizzen-unit-test.ss" "mizzen"))
  
  (define-class %assert-jumps-nowhere-inner% (%test-case%)
    (test "JUMP NOWHERE"
      (assert-jumps /start (void))))

  (define-class %assert-jumps-incorrectly-inner% (%test-case%)
    (test "JUMP INCORRECTLY"
      (assert-jumps /start (jump /tests/run-all))))

  (define-class %assert-jumps-test% (%test-case%)
    (test "ASSERT-JUMPS should fail if no jump occurs"
      (assert-test-fails %assert-jumps-nowhere-inner% "JUMP NOWHERE"
                         "Expected jump to /start, but no jump occurred"))
    (test "ASSERT-JUMPS should fail if jump goes to wrong card"
      (assert-test-fails %assert-jumps-incorrectly-inner% "JUMP INCORRECTLY"
                         (cat "Expected jump to /start, but jumped to "
                              "/tests/run-all")))
    (test "ASSERT-JUMPS should succeed if jump goes to correct card"
      (assert-jumps /start (jump /start)))
    (test "ASSERT-JUMPS should evaluate its argument"
      (let [[dest /start]]
        (assert-jumps dest (jump /start)))))

  (define-class %jump-in-test-case-inner% (%test-case%)
    (test "UNEXPECTED JUMP"
      (jump /start)))

  (define-class %jump-in-test-case-test% (%test-case%)
    (test "Jumping out of a test case should cause it to fail"
      (call-with-jump-handler
       (fn (c)
         (error (cat "Untrapped jump to " c)))
       (fn ()
         (assert-test-fails %jump-in-test-case-inner% "UNEXPECTED JUMP"
                            "Unexpected jump to /start")))))

  (card /tests/halyard-unit-test
      (%test-suite%
       :tests (list %assert-jumps-test%
                    %jump-in-test-case-test%)))
  )