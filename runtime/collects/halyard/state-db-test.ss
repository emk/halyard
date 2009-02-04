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

(module state-db-test (lib "halyard.ss" "halyard")
  (require (lib "halyard-unit.ss" "halyard"))
  (require (lib "state-db.ss" "halyard"))

  (define-class %state-db-debug-test% (%test-case%) 
    (setup
      (clear-state-db!))
    (test "state-db-debug should return values directly from the state-db"
      (set! (state-db '/test/debug) 'foo)
      (assert-equals 'foo (state-db-debug '/test/debug)))
    (test "state-db-debug should propagate errors"
      (assert-raises exn:fail? (state-db-debug '/test/no-such))))
  
  (define-class %clear-state-db-test% (%test-case%) 
    (test "Clearing state-db should remove most fields"
      (set! (state-db '/test/key) 'value)
      (assert-equals 'value (state-db-debug '/test/key))
      (clear-state-db!)
      (assert-raises exn:fail? (state-db-debug '/test/key)))
    (test "Clearing stateb-db should not remove /system/clock fields"
      (clear-state-db!)
      (assert (number? (state-db-debug '/system/clock/seconds)))
      (assert (number? (state-db-debug '/system/clock/milliseconds))))
    )
  
  (card /tests/state-db
      (%test-suite%
       :tests (list %state-db-debug-test%
                    %clear-state-db-test%)))
  
  )
