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

(module events (lib "mizzen.ss" "mizzen")
  (require (lib "types.ss" "halyard/private"))
  (require (lib "nodes.ss" "halyard/private"))
  (require (lib "util.ss" "mizzen"))
  (require (lib "util.ss" "halyard/private"))

  ;; Functions intended solely for use by kernel.ss and other internal
  ;; parts of Halyard.
  (provide dispatch-event-to-current-group-member
           make-node-event-dispatcher)

  (provide %event% %update-ui-event% %char-event%
           %mouse-event% %url-event% %text-event% %browser-navigate-event%
           %progress-changed-event% %media-caption-event%)

  (define-class %event% ()
    (attr %handled? #t :type <boolean> :writable? #t)

    ;;; Whenever we finish processing an event, we mark any other queued
    ;;; events (of certain types) as stale.  This allows callers to
    ;;; optionally ignore events that may have been obsoleted by previous
    ;;; events.  This is supposedly a boolean, but it may actually contain
    ;;; some odd true values.
    (attr stale? #f)

    ;;; A few types of events can be vetoed, preventing the action
    ;;; described by the event from actually occurring.
    (def (vetoed?)
      #f)

    ;; It's not clear whether or not this should actually be public.
    (def (mark-as-not-handled!)
      (set! (.%handled?) #f))
    )

  (define (add-event-veto-mixin! klass)
    (with-instance klass
      (def (vetoed?)
        (if (has-slot? 'vetoed?)
          (slot 'vetoed?)
          #f))
      ;;; Veto this event.
      (def (veto!)
        (set! (slot 'vetoed?) #t))))

  (define-class %update-ui-event% (%event%)
    (attr command :type <symbol>))

  (define-class %char-event% (%event%)
    (attr character :type <char>)
    (attr modifiers '() :type <list>)
    ;;; This is an easier-to-use version of .modifiers and .characters that
    ;;; returns a single list.
    (def (modifiers-and-character)
      (append (.modifiers) (list (.character)))))

  (define-class %mouse-event% (%event%)
    (attr position :type <point>)
    (attr double-click? #f :type <boolean>))

  (define-class %url-event% (%event%)
    (attr url :type <string>))

  (define-class %text-event% (%event%)
    (attr text :type <string>))

  (define-class %browser-navigate-event% (%url-event%)
    (add-event-veto-mixin! self))

  (define-class %progress-changed-event% (%event%)
    ;;; Value is 0.0 to 1.0, inclusive.
    (attr value :type <number>)
    (attr done? (= (.value) 1.0) :type <boolean>))

  (define-class %media-caption-event% (%event%)
    (attr caption :type <string>))

  ;; TODO - should this be moved into nodes.ss and made public?
  (define (for-each-active-node func)
    ;; Walk up our hierarchy of groups and sequences.
    (let walk-up [[group-member (current-group-member)]]
      (when group-member
        ;; Walk down into each element within our current card, group, or 
        ;; sequence.
        (let walk-down [[node group-member]]
          (func node)
          (foreach [elem (node-elements node)]
            (walk-down elem)))
        (walk-up (node-parent group-member)))))
  
  (define (dispatch-idle-event-to-active-nodes)
    (for-each-active-node (lambda (node) (node .idle))))

  (define (dispatch-event-to-current-group-member name . args)
    (when (*engine* .current-group-member)
      (if (eq? name 'idle)
          (dispatch-idle-event-to-active-nodes)
          ((current-group-member) .dispatch-event-to-node name args))))

  (define (make-node-event-dispatcher node)
    (lambda (name . args)
      (node .dispatch-event-to-node name args)))

  (define (expensive-event-name? name)
    ;; Some events are sent almost constantly, and cause us to allocate
    ;; memory too quickly.  This causes a performance loss.  To avoid
    ;; this performance loss, we only enable the sending of these events
    ;; if we believe there is a handler to receive them.
    (case name
      [[idle mouse-moved] #t]
      [else #f]))

  (define (mouse-event-name? name)
    (case name
      [[mouse-moved mouse-down mouse-up mouse-enter mouse-leave] #t]
      [else #f]))

  (with-instance %node%
    (with-instance (.class)
      ;; A helper method which walks up the class hierarchy and sees if a
      ;; given method ever returns true.  This has the same short-circuit
      ;; semantics as OR.
      (def (recursive-or-of-method name stop-at-class)
        (or (send self name)
            (if (eq? self stop-at-class)
              #f
              ((.superclass) .recursive-or-of-method name stop-at-class))))

      ;; Does this class define any methods for handling expensive events?
      (attr defines-expensive-event-methods? #f :writable? #t)

      ;; Does this class or any of its superclasses define methods for
      ;; handling expensive events?
      (def (has-expensive-event-methods?)
        (.recursive-or-of-method 'defines-expensive-event-methods? %node%))

      ;; Does this class define any methods for handling mouse events?
      (attr defines-mouse-event-methods? #f :writable? #t)

      ;; Does this class or any of its superclasses define methods for
      ;; handling mouse events?
      (def (has-mouse-event-methods?)
        (.recursive-or-of-method 'defines-mouse-event-methods? %node%))
   
      (advise after (define-method name impl)
        ;; If we define methods corresponding to certain types of events,
        ;; record that information in our class.  Note that we ignore the
        ;; default handlers declared on %node% itself, because they don't
        ;; actually do anything.
        (unless (eq? self %node%)
          (when (expensive-event-name? name)
            (set! (.defines-expensive-event-methods?) #t))
          (when (mouse-event-name? name)
            (set! (.defines-mouse-event-methods?) #t))))

      ;;; Automatically propopagate all events in the list NAMES to their
      ;;; parent nodes.
      (def (always-propagate-events names)
        (foreach [name names]
          (.always-propagate name
            :if-not-handled (method (event)
                              (event .mark-as-not-handled!)))))
        
      )

    (attr has-expensive-event-methods?  #f :type <boolean> :writable? #t)

    (advise after (initialize &rest keys)
      ;; Deal with any class-level flags we set up in .DEFINE-METHOD.
      (when ((.class) .has-expensive-event-methods?)
        ;; Keep track of whether we're handling expensive events.  We call
        ;; ENABLE-EXPENSIVE-EVENTS here, which is sufficient for %card% and
        ;; %element% nodes.  But since %card-group%s and %card-sequence%s
        ;; stay alive longer than a single card, we need to set
        ;; .HAS-EXPENSIVE-EVENT-METHODS?, which is used by
        ;; MAYBE-ENABLE-EXPENSIVE-EVENTS-FOR-CARD (on behalf of RUN-CARD)
        ;; to do the rest of our bookkeeping.
        (set! (.has-expensive-event-methods?) #t)
        (*engine* .enable-expensive-events #t))
      (when (and (eq? (.wants-cursor?) 'auto)
                 ((.class) .has-mouse-event-methods?))
        (set! (.wants-cursor?) #t)))
    
    (advise after (notify-reached-trunk)
      ;; At this point, we've exited all the cards we're going to exit, and
      ;; we're about to start entering new cards.  This is a good time to
      ;; recompute our global expensive event state from scratch.
      (maybe-enable-expensive-events-for-node self))

    ;;; Override this method, and it will be called many times a second.
    ;;; Tends to have a negative impact on heap size, and thus garbage
    ;;; collector performance.
    (def (idle)
      (void))

    (.always-propagate-events
     '(update-ui char mouse-down mouse-up mouse-enter mouse-leave mouse-moved
       text-changed text-enter browser-navigate browser-page-changed
       browser-title-changed status-text-changed progress-changed
       media-finished media-local-error media-network-error
       media-network-timeout playback-timer media-caption cursor-moved
       cursor-shown cursor-hidden))

    (def (dispatch-event-to-node name args)
      (debug-log (cat (.full-name) ": " name " event: " args))
      (define event
        (case name
          [[update-ui]
           (%update-ui-event% .new :command (car args))]
          [[char]
           (%char-event% .new
             :character (string-ref (car args) 0)
             :modifiers (cadr args)
             :stale? (caddr args))]
          [[mouse-down]
           (%mouse-event% .new
             :position (point (car args) (cadr args))
             :double-click? (caddr args)
             :stale? (cadddr args))]
          [[mouse-up mouse-enter mouse-leave mouse-moved]
           (%mouse-event% .new
             :position (point (car args) (cadr args))
             :stale? (cadr args))]
          [[text-changed text-enter]
           (%event% .new)]
          [[browser-navigate]
           (%browser-navigate-event% .new :url (car args))]
          [[browser-page-changed]
           (%url-event% .new :url (car args))]
          [[browser-title-changed]
           (%text-event% .new :text (car args))]
          [[status-text-changed]
           (%text-event% .new :text (car args))]
          [[progress-changed]
           (%progress-changed-event% .new
             :done? (car args)
             :value (cadr args))]
          [[media-finished]
           (%event% .new)]
          [[media-local-error media-network-error
                              media-network-timeout playback-timer]
           (%event% .new)]
          [[media-caption]
           (%media-caption-event% .new :caption (car args))]
          [[cursor-moved]
           (%mouse-event% .new
             :position (point (car args) (cadr args))
             :stale? (cadr args))]
          [[cursor-shown cursor-hidden]
           (%event% .new)]
          [else
           (report-error (cat "Unsupported event type: " name))]))
      (send self name event)
      (set! (*engine* .event-vetoed?) (event .vetoed?))
      (set! (*engine* .event-handled?) (event .%handled?)))
    )

  (define (node-or-elements-have-expensive-handlers? node)
    ;; See if NODE or any of its elements have expensive handlers.
    ;; <<<< FORWARD REF >>>>
    (or (node .has-expensive-event-methods?)
        (let recurse [[elements (node .elements)]]
          (if (null? elements)
              #f
              (or (node-or-elements-have-expensive-handlers? (car elements))
                  (recurse (cdr elements)))))))
  
  (define (maybe-enable-expensive-events-for-node node)
    ;; REGISTER-EVENT-HANDLER attempts to turn on expensive events whenever
    ;; a matching handler is installed.  But we need to reset the
    ;; expensive event state when changing cards.  This means we need
    ;; to pay close attention to any nodes which live longer than a card.
    ;;
    ;; TODO - We could think of much better ways of handling this, I think.
    (let [[enable? #f]]
      (let recurse [[node node]]
        (when node
          (if (node-or-elements-have-expensive-handlers? node)
              (set! enable? #t)
              (recurse (node .parent)))))
      (*engine* .enable-expensive-events enable?)))

  )
