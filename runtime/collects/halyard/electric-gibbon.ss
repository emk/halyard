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
  (require (only (lib "search.ss" "srfi/1") find))


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
    ;;; Does this element allow any kind of interaction with the user?  If
    ;;; not, we won't generate any test actions.
    (def (supports-user-interaction?)
      (.shown?))

    (def (test-actions)
      (if (.supports-user-interaction?)
        (super)
        '()))
     
    ;;; Set this attribute to recursively skip all test actions for this
    ;;; element and its children.
    (attr skip-test-actions? #f)

    (def (all-test-actions)
      (if (.skip-test-actions?)
        '()
        (super)))
    )

  (with-instance %custom-element%
    (def (supports-user-interaction?)
      ;; .wants-cursor? can also be 'auto, so be careful.
      (and (super) (eq? (.wants-cursor?) #t)))
    )

  (with-instance %basic-button%
    (def (supports-user-interaction?)
      ;; No point in test buttons until we can actually enable them.
      (and (super) (.enabled?)))
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
    ;;; What should we do after running each action?  This attr will
    ;;; generally be used to run any deferred callbacks that were created
    ;;; by running the previous action.
    (attr after-each-action void :type <function>)

    ;;; The card which we're in charge of testing.
    (attr card ((current-card) .static-node))

    (def (initialize &rest keys)
      (super)
      (.notify-card-restarted))

    ;;; What actions have been performed so far on the current visit to
    ;;; this card?  Reset by .at-end-of-card-body.
    (def (actions-taken)
      (reverse (slot 'actions-taken)))

    ;;; When running a %test-planner%, it may be necessary to periodically
    ;;; restart the card.  When this becomes necessary, you must call
    ;;; .notify-card-restarted after reach the end of the main card body,
    ;;; and before running any test actions.
    (def (notify-card-restarted)
      (set! (slot 'actions-taken) '()))

    ;;; Is this test planner completely done with the current card?
    (def (done?)
      (error "Must override .done?"))

    ;;; Run as many test actions as possible at the current time.  Once
    ;;; this function returns, call .done?.  If .done? returns #f, it will
    ;;; be necessary to jump back to .card and call .notify-card-restarted
    ;;; and .run-test-actions.
    (def (run-test-actions)
      (error "Must override .run-test-actions"))


    ;;---------------------------------------------------------------------
    ;;  For use by subclasses

    ;;; Run ACTION, and log it appropriately.  This function should only be
    ;;; called by subclasses of %test-planner%, and it should be used
    ;;; instead of %action% .run.  If ACTION has already been tested once,
    ;;; but must be run again for some reason, pass :rerun? #t.  This will
    ;;; result in a slightly different log message.
    (def (run-action action &key rerun?)
      (set! (slot 'actions-taken)
            (cons (action .key) (slot 'actions-taken)))
      ;; TODO - Temporarily print action names until we have code to
      ;; intercept error messages.  See also jump-to-each-card, which
      ;; has a similar use of COMMAND-LINE-MESSAGE.
      (command-line-message
       (cat "  " (if rerun? "Rerun: " "") (symbol->string (action .key))))
      (action .run)
      ((.after-each-action)))

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
    (def (run-test-actions)
      (void))
    (def (done?)
      #t))


  ;;=======================================================================
  ;;  Deep test planner
  ;;=======================================================================

  (provide %deep-test-planner%)

  ;;; A %deep-test-planner% attempts to run all test actions that are
  ;;; available at the end of a card body.  It keeps track of actions that
  ;;; are only available after other actions are run, and it attempts to
  ;;; find its way back to them.
  ;;;
  ;;; Worth noting: We haven't spent much time thinking about possible
  ;;; infinite loops in this code, aside from one case that it mentioned in
  ;;; the comments below.  We'll worry about infinite loops when we
  ;;; encounter one in the wild.
  (define-class %deep-test-planner% (%test-planner%)
    ;; This table maps %test-action% .key values to either:
    ;;  - A (possibly empty) list of actions which had already been
    ;;    performed when this action first became visible.  This is used
    ;;    to find test actions which we miss the first time through, and
    ;;    have to go back for later.
    ;;  - #f if the action has been performed, and is no longer available.
    (attr %uncompleted (make-hash-table))

    (def (initialize &rest keys)
      (super)
      ;; Build our table immediately so that we can call .done? before
      ;; calling .next-test-action.
      (.%update-uncompleted-actions ((current-card) .all-test-actions)))

    ;;; Have we performed all the test actions we wanted to perform?  The
    ;;; answer is based on our test plan, not on what actions are currently
    ;;; uncompleted, because some actions may have become unavailable at the
    ;;; current time as the result of other actions being run.
    (def (done?)
      (not (ormap identity (hash-table-map (.%uncompleted) (fn (k v) v)))))

    ;;; Run as many test actions as possible.
    (def (run-test-actions)
      (assert (null? (.actions-taken)))

      ;; If we have no available actions right now, check to see if we some
      ;; uncompleted actions "deeper" into the card.  If we do, we can try
      ;; to return to a state where some of those actions become available.
      (define candidates (.%available-actions))
      (when (null? candidates)
        (.%try-to-return-to-a-state-with-test-actions)
        (set! candidates (.%available-actions))
        (when (and (null? candidates) (not (.done?)))
          ;; Avoid an infinite loop.
          (fatal-error (cat "Can't find a way to perform a previously seen "
                            "test action"))))

      ;; Perform as many actions as possible.
      (while (not (null? candidates))
        (let [[action (car candidates)]]
          (hash-table-put! (.%uncompleted) (action .key) #f)
          (.run-action action))
        (set! candidates (.%available-actions))))

    ;; Given a list of actions that are currently available, filter out any
    ;; that we've already completed.
    (def (%available-actions)
      (define actions ((current-card) .all-test-actions))
      (define uncompleted (.%uncompleted))
      (.%update-uncompleted-actions actions)
      (filter (fn (action) (hash-table-get uncompleted (action .key) #f))
              actions))

    ;; Given a list of actions, add any newly-visible actions to our
    ;; .%uncompleted table.
    (def (%update-uncompleted-actions actions)
      (define uncompleted (.%uncompleted))
      (define actions-taken (.actions-taken))
      (foreach [action actions]
        (define key (action .key))
        (unless (hash-table-has-key? uncompleted key)
          (unless (null? actions-taken)
            (command-line-message (cat "    New: " key)))
          (hash-table-put! uncompleted key actions-taken))))

    ;; Whenever we see an action for the first time, we record the current
    ;; value of (.actions-taken), just in case we need to find our way back
    ;; to that point in the card.  This function attempts to find a state
    ;; that we previously missed, and replay whatever test actions allowed
    ;; us to see it last time.  Note that this is a bit of an odd
    ;; code-path, and it won't be triggered at all for many simple cards.
    (def (%try-to-return-to-a-state-with-test-actions)
      ;; Take our list of uncompleted test actions, and generate a list of
      ;; non-'() states that still have a uncompleted test action.
      (define states-with-test-actions
        (filter (fn (v) (and v (not (null? v))))
                (hash-table-map (.%uncompleted) (fn (k v) v))))
      ;; If we've found some states to try, pick one and run test actions
      ;; until we return to it.
      (unless (null? states-with-test-actions)
        ;; This choice is pretty arbitrary.  Can we do better?
        (let [[goal-state (car states-with-test-actions)]]
          ;; Using the saved action keys, replay our previous series of
          ;; actions as best we can.
          (let loop [[keys goal-state]]
            (unless (null? keys)
              (let* [[key (car keys)]
                     [actions ((current-card) .all-test-actions)]
                     [action (find (fn (action) (eq? key (action .key)))
                                   actions)]]
                (if action
                  (begin
                    (.run-action action :rerun? #t)
                    (loop (cdr keys)))
                  (warning (cat "Action " key
                                " has mysteriously disappeared")))))))))

    )


  ;;=======================================================================
  ;;  Standard test actions
  ;;=======================================================================

  (define (element-center elem)
    (local->card elem (shape-center (elem .shape))))

  (with-instance %custom-element%
    (test-action click
      (define center (element-center self))
      ;; Do a quick mouse-enter/mouse-leave pair before actually clicking
      ;; on the element, just to make sure we actually test mouse-leave for
      ;; elements which perform a non-local exit.  This used to be a
      ;; separate test action, but that typically caused us to return
      ;; several more times to given state when running
      ;; %deep-test-planner%.
      (.mouse-enter (%mouse-event% .new :position center))
      (.mouse-leave (%mouse-event% .new :position (point 0 0)))
      ;; Simulate a mouse click.
      (.mouse-enter (%mouse-event% .new :position center))
      (.mouse-down (%mouse-event% .new :position center))
      (.mouse-up (%mouse-event% .new :position center))
      (.mouse-leave (%mouse-event% .new :position (point 0 0))))
    )

  )