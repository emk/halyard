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
  (define-element-template %simple-draggable-object%
      [[bounds :type <rect> :default $screen-rect
               :label "Boundary for region of motion"]
       [float-when-dragging? :type <boolean> :default #t
                             :label "Float above other items when dragging"]]
      (%custom-element%)
    (define max-x (- (rect-right bounds) 
                     (rect-width (prop self shape))))
    (define max-y (- (rect-bottom bounds) 
                     (rect-height (prop self shape))))
    (define min-x (rect-left bounds))
    (define min-y (rect-top bounds))
    (define offset-x 0)
    (define offset-y 0)
    (define (apply-drag-offset p)
      (let [[new-point (point (+ (point-x p) offset-x) 
                              (+ (point-y p) offset-y))]]
        (when (> min-x (point-x new-point))
          (set! (point-x new-point) min-x))
        (when (>=  (point-x new-point) max-x)
          (set! (point-x new-point) max-x))
        (when (> min-y  (point-y new-point))
          (set! (point-y new-point) min-y))
        (when (>=  (point-y new-point) max-y)
          (set!  (point-y new-point) max-y))
        new-point))
    (define (move-shape p)      
      (set! (prop self at) p))
    (on mouse-down (event)
      (define p (event-position event))
      (define bounds (offset-by-point (prop self shape) (prop self at)))
      (define at (point (rect-left bounds) 
                        (rect-top bounds)))
      (set! offset-x (- (point-x at) (point-x p)))
      (set! offset-y (- (point-y at) (point-y p)))
      (grab-mouse self)
      (send self drag-started event))
    (on mouse-moved (event)
      (when (mouse-grabbed-by? self)
        (move-shape (apply-drag-offset (event-position event)))))
    (on mouse-up (event)
      (when (mouse-grabbed-by? self)
        (ungrab-mouse self)
        (send self drag-finished event)))

    ;; Functions to be overridden by subclasses.
    (on drag-started (event)
      (when float-when-dragging?
        (set! (prop self dragging?) #t)))
    (on drag-finished (event)
      (when float-when-dragging?
        (set! (prop self dragging?) #f))))


  ;;=======================================================================
  ;;  Draggable Objects
  ;;=======================================================================
  ;;  These objects may be dragged only to specified targets on the screen.
  ;;  If you drag them anywhere else, they snap back to their home point.

  (define (call-with-dragging-disabled obj thunk)
    (let [[wants-cursor? #f]]
      (dynamic-wind
       (fn ()
         (set! wants-cursor? (prop obj wants-cursor?))
         (set! (prop obj wants-cursor?) #f))
       thunk
       (fn ()
         (set! (prop obj wants-cursor?) wants-cursor?)))))

  (define-syntax with-dragging-disabled
    (syntax-rules ()
      [(with-dragging-disabled obj . body)
       (call-with-dragging-disabled obj (fn () . body))]))

  ;; TODO Is this really the best way to choose a drop target?
  ;; I think the Mac probably works this way...
  (define (point-in-element? p elem)
    (define shape (offset-by-point (prop elem shape)
                                   (local->card elem (point 0 0))))
    ;; TODO - We really need POINT->ELEMENT.
    (point-in-shape? p shape))
  
  (define-element-template %draggable-object%
      [[home-point :type <point> :label "Home Point"]]
      (%simple-draggable-object% :at home-point)

    ;; Allow callers to (SET! (PROP ... HOME-POINT) ...).
    (on prop-change (name value prev veto)
      (case name
        [[home-point]
         #f]
        [else
         (call-next-handler)]))

    (on drag-finished (event)
      (call-next-handler)
      (define p (event-position event))
      (let recurse [[targets *drag-targets*]]
        (cond
         [(null? targets)
          (send self drag-failed event)]
         [(and (point-in-element? p (car targets))
               (send (car targets) drag-allowed? event self))
          (send (car targets) drag-succeeded event self)
          (send self drag-succeeded event (car targets))]
         [#t
          (recurse (cdr targets))])))

    ;; Helpful public functions.
    (on go-to-point (point &key (ms 100))
      (run-deferred
       (callback
         (with-dragging-disabled self
           (animate ms (ease-in/out (slide self point)))))))
    (on go-home ()
      (send self go-to-point home-point))

    ;; Functions to be overridden by subclasses.
    (on drag-failed (event)
      (send self go-home))
    (on drag-succeeded (event target)
      (void)))


  ;;=======================================================================
  ;;  Drag Targets
  ;;=======================================================================
  ;;  Drag targets can receive objects dropped on top of them.  What
  ;;  happens next is the drag target's responsibility.
  
  (provide *drag-targets*)

  (define *drag-targets* '())

  (define-element-template %drag-target% [] (%custom-element%)
    ;; Add (and remove) ourselves from the list of active drag targets.
    (set! *drag-targets* (append! *drag-targets* (list self)))
    (on exit ()
      (set! *drag-targets* (remove self *drag-targets* eq?)))

    ;; Functions to be overridden by subclasses.
    (on drag-allowed? (event draggable)
      #f)
    (on drag-succeeded (event draggable)
      (void))
    )

  )