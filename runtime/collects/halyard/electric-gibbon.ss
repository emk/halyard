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

(module electric-gibbon (lib "mizzen.ss" "mizzen")
  (require (lib "api.ss" "halyard/private"))
  (require (lib "elements.ss" "halyard/private"))


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

  (with-instance %element%
    (attr skip-test-actions? #f)
    (def (all-test-actions)
      (if (.skip-test-actions?)
        '()
        (super)))
    )

  (with-instance %group-member%
    (def (all-test-actions)
      (if (.parent)
        (append (super) (.parent.all-test-actions))
        (super)))
    )


  ;;=======================================================================
  ;;  Abstract test planner interface
  ;;=======================================================================

  (provide %test-planner%)

  ;;; A %test-planner% knows how to test an individual card.  Currently, to
  ;;; use a %test-planner%, you must jump to the card, wait for the card
  ;;; body to finish, and then create a subclass of %test-planner%.  It may
  ;;; be periodically necessary to jump back to the current card as the
  ;;; %test-planner% runs.  See jump-to-each-card.ss for an example.
  ;;;
  ;;; Note that the API of this class is unstable, and may change as the
  ;;; new %test-planner% subclasses are written, and as old ones get
  ;;; more intelligent.
  (define-class %test-planner% ()
    ;;; Run the next available test action, if one is available at the
    ;;; current point in time.  Note that this function may jump off the
    ;;; card at any point, or may temporarily return #f because the card
    ;;; has reached a dead-end with no more available actions, and must
    ;;; be restarted to continue.
    (def (run-next-test-action)
      (error "Must override .run-next-test-action"))

    ;;; Is this test planner completely done with the current card (and
    ;;; not merely done with the actions it can perform right now)?  Note
    ;;; that if .run-next-test-action returns #f, and .done? returns #f,
    ;;; it will generally be necessary to restart the current card and
    ;;; resume calling run-next-test-action.
    (def (done?)
      (error "Must override .done?"))
    )


  ;;=======================================================================
  ;;  Null test planner
  ;;=======================================================================

  (provide %null-test-planner%)

  ;;; A %null-test-planner% makes no attempt to run test actions.  It is
  ;;; intended to reproduce old-style 'rake halyard:jump_each' beahvior,
  ;;; and give applications a migration path to more advanced
  ;;; %test-planner% objects without immediately breaking
  ;;; halyard:jump_each.
  (define-class %null-test-planner% (%test-planner%)
    (def (run-next-test-action)
      #f)
    (def (done?)
      #t))


  ;;=======================================================================
  ;;  Shallow test planner
  ;;=======================================================================

  (provide %shallow-test-planner%)

  ;;; A %shallow-test-planner% attempts to run all test actions that
  ;;; appeared on a card when the %shallow-test-planner% was created.  It
  ;;; can't reach actions which only become available as the result of
  ;;; choosing other actions, and it may get stuck if the available
  ;;; actions on card aren't there the second time a card is visited.
  ;;;
  ;;; Right now, this class knows two tricks: It can survive the deletion
  ;;; and recreation of elements, and it should never run an action that
  ;;; has become unavailable.
  (define-class %shallow-test-planner% (%test-planner%)
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
    ;;; peformed, or #f if there's nothing left to do.  Note that this
    ;;; marks the action it returns as unavailable.
    (def (%next-test-action)
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
      (define action (.%next-test-action))
      (if action
        (begin
          ;; TODO - Temporarily print action names until we have code to
          ;; intercept error messages.  See also jump-to-each-card, which
          ;; has a similar use of COMMAND-LINE-MESSAGE.
          (command-line-message (cat "  " (symbol->string (action .key))))
          (action .run)
          #t)
        #f))

    ;;; Have we performed all the test actions we wanted to perform?  The
    ;;; answer is based on our test plan, not on what actions are currently
    ;;; available, because some actions may have become unavailable at the
    ;;; current time as the result of other actions being run.
    (def (done?)
      (not (ormap identity
                  (hash-table-map (.%available) (fn (k v) v)))))
    )


  ;;=======================================================================
  ;;  Standard test actions
  ;;=======================================================================

  (with-instance %basic-button%
    ;; TODO - We don't generate any test actions if mouse clicks aren't
    ;; allowed.  We can find a better way to do this.
    (def (test-actions)
      (if (and (.wants-cursor?) (.enabled?))
        (super)
        '()))
    (test-action click (.click)))

  (with-instance %clickable-zone%
    ;; TODO - We don't generate any test actions if mouse clicks aren't
    ;; allowed.  We can find a better way to do this.
    (def (test-actions)
      (if (.wants-cursor?)
        (super)
        '()))
    (test-action click (.click)))

  )