(module drag (lib "5l.ss" "5L")
  (require (lib "animate.ss" "5L"))

  (provide %simple-draggable-object% %draggable-object% %drag-target% 
           point-in-element?)
  
  
  ;;=======================================================================
  ;;  Simple Draggable Objects
  ;;=======================================================================
  ;;  This is the most basic draggable object. You can drag it to anywhere
  ;;  on the screen, and it will just sit there.

  ;; Make a moveable object -- one that can be dragged around the screen
  ;;  using the mouse.
  (define-class %simple-draggable-object% (%custom-element%)
    (attr drag-bounds $screen-rect :type <rect>
          :label "Boundary for region of motion")
    (attr float-when-dragging? #t :type <boolean>
          :label "Float above other items when dragging")
    
    (setup
      (set! (slot 'max-x)
            (- (rect-right (.drag-bounds))
               (rect-width (.shape))))
      (set! (slot 'max-y)
            (- (rect-bottom (.drag-bounds))
               (rect-height (.shape))))
      (set! (slot 'min-x) (rect-left (.drag-bounds)))
      (set! (slot 'min-y) (rect-top (.drag-bounds)))
      (set! (slot 'offset-x) 0)
      (set! (slot 'offset-y) 0))

    (def (%apply-drag-offset p)
      (let [[new-point (point (+ (point-x p) (slot 'offset-x)) 
                              (+ (point-y p) (slot 'offset-y)))]]
        (when (> (slot 'min-x) (point-x new-point))
          (set! (point-x new-point) (slot 'min-x)))
        (when (>=  (point-x new-point) (slot 'max-x))
          (set! (point-x new-point) (slot 'max-x)))
        (when (> (slot 'min-y)  (point-y new-point))
          (set! (point-y new-point) (slot 'min-y)))
        (when (>=  (point-y new-point) (slot 'max-y))
          (set!  (point-y new-point) (slot 'max-y)))
        new-point))

    (def (mouse-down event)
      (define p (event-position event))
      (define bounds (offset-by-point (.shape) (.at)))
      (define at (point (rect-left bounds) 
                        (rect-top bounds)))
      (set! (slot 'offset-x) (- (point-x at) (point-x p)))
      (set! (slot 'offset-y) (- (point-y at) (point-y p)))
      (grab-mouse self)
      (.drag-started event))
    (def (mouse-moved event)
      (when (mouse-grabbed-by? self)
        (set! (.at) (.%apply-drag-offset (event-position event)))))
    (def (mouse-up event)
      (when (mouse-grabbed-by? self)
        (ungrab-mouse self)
        (.drag-finished event)))

    ;; Functions to be overridden by subclasses.
    (def (drag-started event)
      (when (.float-when-dragging?)
        (set! (.dragging?) #t)))
    (def (drag-finished event)
      (when (.float-when-dragging?)
        (set! (.dragging?) #f))))


  ;;=======================================================================
  ;;  Draggable Objects
  ;;=======================================================================
  ;;  These objects may be dragged only to specified targets on the screen.
  ;;  If you drag them anywhere else, they snap back to their home point.

  (define (call-with-dragging-disabled obj thunk)
    (let [[wants-cursor? #f]]
      (dynamic-wind
       (fn ()
         (set! wants-cursor? (obj .wants-cursor?))
         (set! (obj .wants-cursor?) #f))
       thunk
       (fn ()
         (set! (obj .wants-cursor?) wants-cursor?)))))

  (define-syntax with-dragging-disabled
    (syntax-rules ()
      [(with-dragging-disabled obj . body)
       (call-with-dragging-disabled obj (fn () . body))]))

  ;; TODO Is this really the best way to choose a drop target?
  ;; I think the Mac probably works this way...
  (define (point-in-element? p elem)
    (define shape (offset-by-point (elem .shape)
                                   (local->card elem (point 0 0))))
    ;; TODO - We really need POINT->ELEMENT.
    (point-in-shape? p shape))
  
  (define-class %draggable-object% (%simple-draggable-object%)
    (attr home-point :type <point> :label "Home Point" :writable? #t)
    (value at (.home-point))

    (def (drag-finished event)
      (super)
      (define p (event-position event))
      (let recurse [[targets *drag-targets*]]
        (cond
         [(null? targets)
          (.drag-failed event)]
         [(and (point-in-element? p (car targets))
               ((car targets) .drag-allowed? event self))
          ((car targets) .drag-succeeded event self)
          (.drag-succeeded event (car targets))]
         [#t
          (recurse (cdr targets))])))

    ;; Helpful public functions.
    (def (go-to-point point &key (ms 100))
      (run-deferred
       (callback
         (with-dragging-disabled self
           (animate ms (ease-in/out (slide self point)))))))
    (def (go-home)
      (.go-to-point (.home-point)))

    ;; Functions to be overridden by subclasses.
    (def (drag-failed event)
      (.go-home))
    (def (drag-succeeded event target)
      (void)))


  ;;=======================================================================
  ;;  Drag Targets
  ;;=======================================================================
  ;;  Drag targets can receive objects dropped on top of them.  What
  ;;  happens next is the drag target's responsibility.
  
  (provide *drag-targets*)

  (define *drag-targets* '())

  (define-class %drag-target% (%custom-element%)
    (setup
      ;; Add ourselves to the list of active drag targets.
      (set! *drag-targets* (append! *drag-targets* (list self))))

    (def (exit)
      (super)
      (set! *drag-targets* (remove self *drag-targets* eq?)))

    ;; Functions to be overridden by subclasses.
    (def (drag-allowed? event draggable)
      #f)
    (def (drag-succeeded event draggable)
      (void))
    )

  )