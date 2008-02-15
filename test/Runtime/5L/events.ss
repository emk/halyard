(module events (lib "language.ss" "5L")
  (require (lib "nodes.ss" "5L"))

  ;; Functions intended solely for use by kernel.ss.
  (provide dispatch-event-to-current-group-member)

  ;; TODO - Port event classes from Swindle to ruby-object.ss.
  (provide <event> event? event-stale?
           <vetoable-event> veto-event! event-vetoed?
           <update-ui-event> update-ui-event? event-command
           <char-event> char-event? event-character event-modifiers
           <mouse-event> mouse-event? event-position event-double-click?
           <url-event> url-event? event-url
           <text-event> text-event? event-text
           <browser-navigate-event> browser-navigate-event?
           <progress-changed-event> event-progress-done? event-progress-value
           make-node-event-dispatcher ; semi-private
           <media-event> <media-finished-event>
           <media-caption-event> event-caption
           )

  (defclass <event> ()
    (handled? :accessor event-handled? :initvalue #t)
    (stale?   :accessor event-stale?   :initvalue #f))

  (defclass <vetoable-event> (<event>)
    (vetoed? :accessor event-vetoed? :initvalue #f))

  (defclass <update-ui-event> (<event>)
    (command :accessor event-command))

  (defclass <char-event> (<event>)
    (character :accessor event-character)
    (modifiers :accessor event-modifiers))

  (defclass <mouse-event> (<event>)
    (position :accessor event-position)
    (double-click? :accessor event-double-click? :initvalue #f))

  (defclass <edit-box-event> (<event>))

  (defclass <url-event> (<event>)
    (url :accessor event-url))

  (defclass <text-event> (<event>)
    (text :accessor event-text))

  (defclass <browser-navigate-event> (<url-event> <vetoable-event>))

  (defclass <progress-changed-event> (<event>)
    (done? :accessor event-progress-done?)
    ;; value is 0.0 to 1.0, inclusive.
    (value :accessor event-progress-value))

  (defclass <media-event> (<event>))
  (defclass <media-finished-event> (<media-event>))
  (defclass <media-caption-event> (<media-event>)
    (caption :accessor event-caption))

  (defclass <cursor-event> (<event>))

  (define (veto-event! event)
    (set! (event-vetoed? event) #t))

  ;; A local helper.
  (defgeneric (was-vetoed? (event <event>)))
  (defmethod (was-vetoed? (event <event>))
    #f)
  (defmethod (was-vetoed? (event <vetoable-event>))
    (event-vetoed? event))

  (define (dispatch-idle-event-to-active-nodes)
    ;; TODO - This code is wrong, because it does not propagate idle events
    ;; to elements parented to other elements.  See case 2316.
    (let loop [[node (current-group-member)]]
      (when node
        (node .idle)
        (foreach [elem (node-elements node)]
          (elem .idle))
        (loop (node-parent node)))))

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
        (or (.send name '())
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
                              (set! (event-handled? event) #f)))))
        
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
      (debug-log (cat (node-full-name self) ": " name " event: " args))
      (define event
        (case name
          [[update-ui]
           (make <update-ui-event> :command (car args))]
          [[char]
           (make <char-event>
             :character (string-ref (car args) 0)
             :modifiers (cadr args)
             :stale? (caddr args))]
          [[mouse-down]
           (make <mouse-event>
             :position (point (car args) (cadr args))
             :double-click? (caddr args)
             :stale? (cadddr args))]
          [[mouse-up mouse-enter mouse-leave mouse-moved]
           (make <mouse-event>
             :position (point (car args) (cadr args))
             :stale? (cadr args))]
          [[text-changed text-enter]
           (make <edit-box-event>)]
          [[browser-navigate]
           (make <browser-navigate-event> :url (car args))]
          [[browser-page-changed]
           (make <url-event> :url (car args))]
          [[browser-title-changed]
           (make <text-event> :text (car args))]
          [[status-text-changed]
           (make <text-event> :text (car args))]
          [[progress-changed]
           (make <progress-changed-event>
             :done? (car args)
             :value (cadr args))]
          [[media-finished]
           (make <media-finished-event>)]
          [[media-local-error media-network-error
                              media-network-timeout playback-timer]
           (make <media-event>)]
          [[media-caption]
           (make <media-caption-event> :caption (car args))]
          [[cursor-moved]
           (make <mouse-event>
             :position (point (car args) (cadr args))
             :stale? (cadr args))]
          [[cursor-shown cursor-hidden]
           (make <cursor-event>)]
          [else
           (non-fatal-error (cat "Unsupported event type: " name))]))
      (.send name (list event))
      (set! (*engine* .event-vetoed?) (was-vetoed? event))
      (set! (*engine* .event-handled?) (event-handled? event)))
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
