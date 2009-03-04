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

(module electric-gibbon (lib "halyard.ss" "halyard")


  ;;=======================================================================
  ;;  Test actions
  ;;=======================================================================

  (provide test-action %test-action%)

  (define (test-action-method-name name)
    (symcat 'test-action- name))

  ;;; This represents an action that can be performed on a specific node.
  ;;; Note that .name (and .node.name) must be stable from one run of the
  ;;; program to the next, because it will be used to remember which
  ;;; actions have been performed during previous visits to a card.
  (define-class %test-action% ()
    (attr node :type %node%)
    (attr name :type <symbol>)
    (attr method)

    ;;; Return a unique identifier (we hope) for this test action.  This
    ;;; should be stable across multiple jumps to the same card within
    ;;; a given run of the program.
    (def (key)
      (symcat (.node.full-name) " " (.name)))

    ;;; Run the test action.
    (def (run)
      (instance-exec (.node) (.method))))

  ;;; Define a test action for the containing node class.
  (define-syntax test-action
    (syntax-rules ()
      [(_ name body ...)
       (.define-test-action 'name (method () body ...))]))


  ;;=======================================================================
  ;;  Finding all test actions
  ;;=======================================================================

  (with-instance %node%
    (with-instance (.class)
      (attr test-action-names '() :writable? #t)
      (def (define-test-action name meth)
        (.define-method (test-action-method-name name) meth)
        (set! (.test-action-names)
              (cons name (.test-action-names))))
      )

    ;;; Get all the test actions local to this specific node.
    (def (test-actions)
      ;; Build a list of all declared test action names anywhere in
      ;; the class hierarchy.
      (define name-table (make-hash-table))
      (let recurse [[klass (self .class)]]
        (foreach [key (klass .test-action-names)]
          (hash-table-put! name-table key #t))
        (unless (eq? klass %node%)
          (recurse (klass .superclass))))
     
      ;; Build a %test-action% for each declared %test-action%.
      (hash-table-map 
       name-table
       (fn (name _)
         (%test-action% .new 
           :node self :name name
           :method (method () 
                     (send self (test-action-method-name name)))))))

    ;;; Get all the test actions for this node, its elements, and (in the
    ;;; case of cards) for any containing groups.
    (def (all-test-actions)
      (apply append
             (.test-actions)
             (map (fn (e) (e .all-test-actions)) (.elements))))
    )

  (with-instance %group-member%
    (def (all-test-actions)
      (if (.parent)
        (append (super) (.parent.all-test-actions))
        (super)))
    )


  ;;=======================================================================
  ;;  Test planner
  ;;=======================================================================

  (provide %test-planner%)

  (define-class %test-planner% ()
    ;;; Remember what card we're associated with.
    (attr card ((current-card) .static-node))

    (attr %available (make-hash-table))
    (def (initialize &rest keys)
      (super)
      ;; For now, we only perform the test actions whose keys appear when
      ;; we are initially constructed.  This helps prevent infinite loops.
      ;; Eventually, we'll want to go deeper into a card.
      (define available (.%available))
      (foreach [action ((current-card) .all-test-actions)]
        (hash-table-put! available (action .key) #t)))

    ;;; Return the first available test action that we haven't already
    ;;; peformed, or #f if there's nothing left to do.
    (def (next-test-action)
      (define available (.%available))
      (let recurse [[candidates ((current-card) .all-test-actions)]]
        (cond
         [(null? candidates) #f]
         [(hash-table-get available ((car candidates) .key) #f)
          (let [[action (car candidates)]]
            (hash-table-put! available (action .key) #f)
            action)]
         [else
          (recurse (cdr candidates))])))

    ;;; Run the next available test action, if one is available.  Return
    ;;; true iff an action was run.
    (def (run-next-test-action)
      (define action (.next-test-action))
      (if action
        (begin
          (action .run)
          #t)
        #f))
    )


  ;;=======================================================================
  ;;  Standard test actions
  ;;=======================================================================

  (with-instance %basic-button%
    (test-action click (.click)))

  (with-instance %clickable-zone%
    (test-action click (.click)))

  )