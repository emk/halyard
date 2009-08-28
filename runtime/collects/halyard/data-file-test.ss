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
  
  (define-class %loading-preferences-from-path-test% (%test-case%)
    (def (run-test-method report)
      (with-temporary-user-data ['another_fake]
        (super)))
    (test "Reading preferences directly should return a hash table"
      (set! *example-user-pref-for-testing* 'another-value)
      (define path (build-path (script-user-data-directory) "another_fake.dat"))
      (define prefs (data-file->hash-table path))
      (assert-equals 'another-value
                     (hash-table-get prefs '*example-user-pref-for-testing*))))

  (card /tests/data-file
      (%test-suite%
       :tests (list %with-temporary-user-data-test%
                    %wrapping-run-test-method-test%
                    %loading-preferences-from-path-test%)))
  
  )
