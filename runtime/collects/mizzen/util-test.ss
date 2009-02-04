;; @BEGIN_LICENSE
;;
;; Mizzen - Scheme object system
;; Copyright 2006-2009 Trustees of Dartmouth College
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

(module util-test "mizzen.ss"
  (require "mizzen-unit.ss")
  

  ;;=======================================================================
  ;;  Split and Join
  ;;=======================================================================

  (define-class %split-test% (%test-case%)
    (test "empty string should split into a list with 1 empty element"
      ;; This differs from both Perl and Ruby, which return the empty list
      ;; in this case.  Why?  It results in a much simpler and more
      ;; consistent implementation.
      (assert-equals '("") (split (regexp " ") ""))
      (assert-equals '("") (split (regexp "/") "")))
    (test "string with no matches should split into list with 1 element"
      (assert-equals '("a") (split (regexp " ") "a"))
      (assert-equals '("b") (split (regexp "/") "b")))
    (test "string with one delimiter should split into two strings"
      (assert-equals '("a" "b") (split (regexp " ") "a b"))
      ;; Again, another different from Perl and Ruby's default behavior: We
      ;; preserve trailing empty elements.
      (assert-equals '("a" "") (split (regexp "/") "a/")))
    (test "strings with two delimiters should split into three strings"
      (assert-equals '("a" "b" "c") (split (regexp "/") "a/b/c")))
    (test "regexp matches should be greedy"
      (assert-equals '("a" "b" "") (split (regexp "/+") "a//b///")))
    ;; Note that we do not define the behavior of zero-width matches yet,
    ;; but that we may do so at some point in the future.  We also do not
    ;; define the result of passing a string as the first argument of
    ;; SPLIT.
    )

  (define-class %join-test% (%test-case%)
    (test "Joining the empty list should return an empty string"
      (assert-equals "" (join "" '()))
      (assert-equals "" (join "," '())))
    (test "Joining a one-element list should return that element"
      (assert-equals "a" (join "" '("a")))
      (assert-equals "b" (join "," '("b"))))
    (test "Joining a multiple-element list should join using delimiter"
      (assert-equals "abc" (join "" '("a" "b" "c")))
      (assert-equals "d,e,f" (join "," '("d" "e" "f"))))
    )


  ;;=======================================================================
  ;;  Exported Test Cases
  ;;=======================================================================

  (provide $all-util-tests)

  (define $all-util-tests
    (list %join-test%))

  )
