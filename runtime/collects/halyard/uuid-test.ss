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

(module uuid-test (lib "halyard.ss" "halyard")
  (require (lib "halyard-unit.ss" "halyard"))
  (require (lib "uuid.ss" "halyard"))

  (define-class %uuid-test% (%test-case%)
    (test "UUIDs should have the correct format"
      (assert (regexp-match (pregexp (cat "^[0-9a-fA-F]{8}-([0-9a-fA-F]{4}-){3}"
                                          "[0-9a-fA-F]{12}$"))
                            (uuid))))
    (test "UUIDs should not match"
      (assert (not (equals? (uuid) (uuid))))))

  (card /tests/uuid
      (%test-suite%
       :tests (list %uuid-test%)))

  )
  
