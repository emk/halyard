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

(module electric-gibbon-test (lib "halyard.ss" "halyard")
  (require (lib "halyard-unit.ss" "halyard"))
  (require (lib "electric-gibbon.ss" "halyard"))
  (require (lib "test-elements.ss" "halyard"))

  (define-class %test-button% (%basic-button%)
    (value bounds (rect 0 0 100 20))

    (attr clicked? #f :writable? #t)
    (attr frobbed? #f :writable? #t)

    (def (click)
      (set! (.clicked?) #t))

    (def (frob)
      (set! (.frobbed?) #t))

    (test-action frob-action
      (.frob))

    (def (draw)
      (void)))

  (define (find-action element name)
    (let recurse [[actions (element .test-actions)]]
      (cond
       [(null? actions)
        (error (cat "Can't find action " name " in " element))]
       [(eq? name ((car actions) .name))
        (car actions)]
       [else
        (recurse (cdr actions))])))

  (define-class %electric-gibbon-test% (%element-test-case%)
    (test "A button should have a click test action."
      (define b (%test-button% .new))
      (define a1 (find-action b 'click))
      (assert-equals b (a1 .node))
      (assert-equals 'click (a1 .name))
      (a1 .run)
      (assert (b .clicked?)))
    (test "We should be able to define our own test actions."
      (define b (%test-button% .new))
      ((find-action b 'frob-action) .run)
      (assert (b .frobbed?)))
    )

  (card /tests/electric-gibbon
      (%element-test-suite%
       :tests (list %electric-gibbon-test%)))
  
  )