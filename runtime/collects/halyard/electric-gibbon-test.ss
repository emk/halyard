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

    ;; We inherit some test actions from our parent.
    (attr clicked? #f :writable? #t)
    (def (click)
      (set! (.clicked?) #t))

    ;; We can also define local, named test actions.  Note that we would
    ;; not normally include "-action" in the name, but we want to make
    ;; it differ from the underlying method name that we're testing, just
    ;; to keep the implementation honest.
    (attr frobbed? #f :writable? #t)
    (def (frob)
      (set! (.frobbed?) #t))
    (test-action frob-action
      (.frob))

    ;; If we encounter a situation where some test actions can only be
    ;; determined at runtime, we can also create them explicitly.
    (attr munged? #f :writable? #t)
    (def (test-actions)
      (cons (%test-action% .new :node self :name 'munge
                           :method (method () (set! (.munged?) #t)))
            (super)))

    ;; We can just ignore this.
    (def (draw)
      (void))
    )

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
    (test "We should be able to define test actions manually."
      (define b (%test-button% .new))
      ((find-action b 'munge) .run)
      (assert (b .munged?)))
    )

  (card /tests/electric-gibbon
      (%element-test-suite%
       :tests (list %electric-gibbon-test%)))
  
  )