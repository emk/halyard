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

(module util-test (lib "halyard.ss" "halyard")
  (require (lib "halyard-unit.ss" "halyard"))

  (define-class %logging-test% (%test-case%)
    (test "trace should not crash"
      (trace 'halyard.log-test "Testing TRACE function"))
    (test "debug should not crash"
      (debug 'halyard.log-test "Testing DEBUG function"))
    (test "logger 'warn should issue a warning"
      (assert-warns (logger 'warn 'halyard.log-test "Testing LOGGER 'warn")))
    (test "warn should issue a warning"
      (assert-warns (warn 'halyard.log-test "Testing WARN function"))))

  (define-class %with-exceptions-blocked-test% (%test-case%)
    (test "with-exceptions-blocked should block exceptions"
      (define caught-exn #f)
      (define (handler exn)
        (set! caught-exn exn))
      (with-exceptions-blocked (handler)
        (void))
      (assert-equals #f caught-exn)
      (with-exceptions-blocked (handler)
        (error "foo"))
      (assert-equals "foo" (exn-message caught-exn))))

  (card /tests/util
      (%test-suite%
       :tests (list %logging-test%
                    %with-exceptions-blocked-test%)))

  )
