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