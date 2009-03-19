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

    ;; We should be able to recursively walk our child element test
    ;; actions.
    (elem child (%custom-element% :bounds (rect 0 0 10 10))
      (value wants-cursor? #t)
      (attr clicked? #f :writable? #t)
      (def (child-click)
        (set! (.clicked?) #t))
      (test-action child-click
        (.child-click)))

    ;; We can just ignore this.
    (def (draw)
      (void))
    )

  (define (find-action element name)
    (let recurse [[actions (element .all-test-actions)]]
      (cond
       [(null? actions)
        (error (cat "Can't find action " name " in " element))]
       [(eq? name ((car actions) .name))
        (car actions)]
       [else
        (recurse (cdr actions))])))

  (define-class %electric-gibbon-test% (%element-test-case%)
    (attr b #f :writable? #t)
    (setup-test
      ;; TODO - This is stupid, and test-elements.ss needs to be fixed.
      (with-default-element-parent (.element-parent)
        (set! (.b) (%test-button% .new :name 'b))))

    (test "A button should have a click test action."
      (define a1 (find-action (.b) 'click))
      (assert-equals (.b) (a1 .node))
      (assert-equals 'click (a1 .name))
      (a1 .run)
      (assert (.b.clicked?)))
    (test "We should be able to define our own test actions."
      ((find-action (.b) 'frob-action) .run)
      (assert (.b.frobbed?)))
    (test "We should be able to define test actions manually."
      ((find-action (.b) 'munge) .run)
      (assert (.b.munged?)))
    (test "We should be able to get test actions for our child elements."
      ((find-action (.b) 'child-click) .run)
      (assert (.b.child.clicked?)))
    (test "We should be able to get test elements for parents of card."
      ;; TODO - This test case is insufficient, but we don't want to
      ;; actually mess with our parent group.  So just make sure that
      ;; it doesn't crash, and that it sees our regular child elements.
      (find-action (current-card) 'child-click))
    (test "A test action should have a unique key."
      (assert-equals (symcat (.b.full-name) " " 'click)
                     ((find-action (.b) 'click) .key)))

    )

  (define-class %stable-element-name-test% (%element-test-case%)
    (def (create-nested-element)
      (define parent (%custom-element% .new :bounds (rect 0 0 100 100)))
      (%custom-element% .new :name #f :parent parent :bounds (rect 0 0 10 10)))

    (test "Element names should be stable relative to their parents."
      (define name-1 (.create-nested-element.name))
      (define name-2 (.create-nested-element.name))
      (assert-equals name-1 name-2))

    (test "Creating element names with (gensym) should give a warning."
      (assert-warns
        (%custom-element% .new :name (gensym) :bounds (rect 0 0 10 10))))
    )

  (define-class %do-not-click-button% (%basic-button%)
    (value bounds (rect 0 0 10 10))
    (def (click) (error (cat "Accidentally clicked " self)))
    (def (draw) (void)))

  (define-class %test-action-set% (%custom-element%)
    (value bounds (rect 0 0 100 20))
    (attr action-mask 0 :writable? #t)
    (def (supports-user-interaction?)
      #t)
    (def (set-bit! n)
      (define new-mask (arithmetic-shift 1 n))
      (assert-equals 0 (bitwise-and (.action-mask) new-mask))
      (set! (.action-mask) (bitwise-ior (.action-mask) new-mask)))
    (test-action bit0 (.set-bit! 0))
    (test-action bit1 (.set-bit! 1))
    (test-action bit2 (.set-bit! 2))
    (def (done?)
      (= (.action-mask) #b111))

    ;; These items should be excluded from our test plan.  Yes, we could
    ;; test these above as part of our action list, but none of those test
    ;; cases actually attempt to run everything.
    (elem do-not-click-1 (%do-not-click-button% :enabled? #f))
    (elem do-not-click-2 (%do-not-click-button% :skip-test-actions? #t))
    (elem do-not-click-3 (%do-not-click-button% :shown? #f))
    )

  (define-class %deep-test-planner-basic-test% (%element-test-case%)
    (test "A test planner should run all test actions for current card."
      (define s (%test-action-set% .new))
      (define p (%deep-test-planner% .new))
      (p .run-test-actions)
      (assert (s .done?)))
    (test "A test planner should remember what card it was created on."
      (define p (%deep-test-planner% .new))
      (assert-equals ((current-card) .static-node) (p .card)))
    )

  (define-class %depedant-button% (%basic-button%)
    (attr clicked? #f :type <boolean> :writable? #t)
    (attr y)
    (value at (point 0 (.y)))
    (value shape (shape 100 20))
    (def (draw) (void))
    (def (click)
      (assert (not (.clicked?)))
      (set! (.enabled?) #f)
      (set! (.clicked?) #t)))

  ;; Button 1 enables button 2, which enables buttons 3a and 3b.  But 3a
  ;; and 3b disable each other, so it's impossible to test all the
  ;; buttons in one pass.
  (define-class %depedant-buttons% (%custom-element%)
    (value bounds (rect 0 0 100 100))

    (elem button-1 (%depedant-button% :y 0)
      (def (click)
        (super)
        (set! (.parent.button-2.enabled?) #t)))

    (elem button-2 (%depedant-button% :y 20 :enabled? #f)
      (def (click)
        (super)
        (set! (.parent.button-3a.enabled?) #t)
        (set! (.parent.button-3b.enabled?) #t)))

    (elem button-3a (%depedant-button% :y 40 :enabled? #f)
      (def (click)
        (super)
        (set! (.parent.button-3b.enabled?) #f)))

    (elem button-3b (%depedant-button% :y 60 :enabled? #f)
      (def (click)
        (super)
        (set! (.parent.button-3a.enabled?) #f)))
    )

  ;; Exclusive logical or.
  (define (xor a b)
    (and (or a b)
         (not (and a b))))
    
  (define-class %deep-test-planner-deep-test% (%element-test-case%)
    (test "%deep-test-planner% should find actions hidden by other actions"
      ;; First pass.
      (define e (%depedant-buttons% .new :name 'buttons))
      (define p (%deep-test-planner% .new))
      (p .run-test-actions)
      (assert (e .button-1.clicked?))
      (assert (e .button-2.clicked?))
      (define button-3a-clicked-1st-time? (e .button-3a.clicked?))
      (define button-3b-clicked-1st-time? (e .button-3b.clicked?))
      (assert (xor button-3a-clicked-1st-time? button-3b-clicked-1st-time?))
      (assert (not (p .done?)))

      ;; Second pass.  We may need to do this several times if we choose
      ;; a poor test plan.
      (define button-3a-clicked-2nd-time? #f)
      (define button-3b-clicked-2nd-time? #f)
      (while (not (p .done?))
        (delete-element e)
        (set! e (%depedant-buttons% .new :name 'buttons))
        (p .notify-card-restarted)
        (p .run-test-actions)
        (set! button-3a-clicked-2nd-time?
              (or button-3a-clicked-2nd-time? (e .button-3a.clicked?)))
        (set! button-3b-clicked-2nd-time?
              (or button-3b-clicked-2nd-time? (e .button-3b.clicked?))))
      (assert (xor button-3a-clicked-2nd-time? button-3b-clicked-2nd-time?))
      (assert (xor button-3a-clicked-1st-time? button-3a-clicked-2nd-time?))
      (assert (xor button-3b-clicked-1st-time? button-3b-clicked-2nd-time?))
      ))

  (card /tests/electric-gibbon
      (%element-test-suite%
       :tests (list %electric-gibbon-test%
                    %stable-element-name-test%
                    %deep-test-planner-basic-test%
                    %deep-test-planner-deep-test%)))
  
  )