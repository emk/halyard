(module widgets (lib "5L.ss" "5L")
  (require (lib "tamale.ss" "5L"))
  (require (lib "shapes.ss" "5L"))
  (provide %simple-toggle% %fancy-toggle% %toggle-base% %rect-drawing%
           <graphic> make-graphic graphic? draw-graphic
           <picture> make-picture picture? picture-path picture-rect
                     picture-offset
           <saved-region> make-saved-region saved-region? 
                          saved-region-rect saved-region-offset
           <rect-graphic> make-rect-graphic rect-graphic? 
                          rect-graphic-rect rect-graphic-color 
                          rect-graphic-width
           <null-graphic> make-null-graphic null-graphic?
           <widget-states> make-widget-states widget-states?)


  ;;; =====================================================================
  ;;; Graphics utilities
  ;;; =====================================================================

  (define (offset-point point1 point2)
    (point (+ (point-x point1)
              (point-x point2))
           (+ (point-y point1)
              (point-y point2))))

  (define (offset-rect rct point)
    (let ((x-offset (point-x point))
          (y-offset (point-y point)))
      (rect (+ x-offset (rect-left rct))
            (+ y-offset (rect-top rct))
            (+ x-offset (rect-right rct))
            (+ y-offset (rect-bottom rct)))))

  (defclass <graphic> ())

  (defclass <picture> (<graphic>)
    path 
    (rect :initvalue #f)
    offset)

  (defclass <saved-region> (<graphic>)
    (rect :initvalue $screen-rect)
    offset)

  (defclass <rect-graphic> (<graphic>)
    rect
    color
    width)
  
  (defclass <null-graphic> (<graphic>))

  (defgeneric (draw-graphic (graphic <graphic>) offset))

  (defmethod (draw-graphic (pict <picture>) offset)
    (if (picture-rect pict)
        (load-picture (picture-path pict) 
                      (offset-point offset (picture-offset pict))
                      :rect (picture-rect pict))
        (load-picture (picture-path pict)
                      (offset-point offset (picture-offset pict)))))

  ;;; XXX: Offset is being ignored. Need to figure out if we should just 
  ;;; ignore it, or if we should add a way of blitting saved regions
  ;;; to arbitrary places on screen. Actually, need to figure out if 
  ;;; we have too many redundant offsets, also.
  (defmethod (draw-graphic (pict <saved-region>) offset)
    (restore-graphics :bounds (saved-region-rect pict)))

  (defmethod (draw-graphic (graphic <rect-graphic>) offset)
    (draw-box-outline (offset-rect (rect-graphic-rect graphic) offset)
                      (rect-graphic-color graphic)
                      (rect-graphic-width graphic)))
    
  (defmethod (draw-graphic (graphic <graphic>) offset)
    #f)

  (defclass <widget-states> ()
    (normal :initvalue #f)
    (prelight :initvalue #f)
    (active :initvalue #f)
    (insensitive :initvalue #f)
    (selected :initvalue #f))

  (define (draw-state states state pos)
    (define (draw-if-defined graphic)
      (if graphic
          (draw-graphic graphic pos)
          (draw-state states 'normal pos)))
    (case state
      ((normal) (when (widget-states-normal states)
                  (draw-graphic (widget-states-normal states) pos)))
      ((prelight) (draw-if-defined (widget-states-prelight states)))
      ((active) (draw-if-defined (widget-states-active states)))
      ((insensitibe) (draw-if-defined (widget-states-insensitive states)))
      ((selected) (draw-if-defined (widget-states-selected states)))))

  ;;; =====================================================================
  ;;; Audio
  ;;; =====================================================================

  (define (play-audio path &key (loop? #f) (name 'audio))
    (when path
      (delete-element name)
      (vorbis-audio name path :loop? loop?)))


  ;;; =====================================================================
  ;;; Widget definitions
  ;;; =====================================================================

  ;;; TODO - add external model
  ;;; TODO - add view superclass
  (define-element-template %toggle-base% 
      [on-states off-states] (:template %zone%)    
    (define on? #f)

    (on draw (state)
      (if on?
          (draw-state on-states state (point 0 0))
          (draw-state off-states state (point 0 0))))

    (on set-value (new-on? state)
      (when (not (eq? new-on? on?))
        (set! on? new-on?)
        (send self draw state)))

    (on toggle (state)
      (send self set-value (not on?) state)))

  (define-element-template %simple-toggle%
      [sound] (:template %toggle-base%)

    (on mouse-down (event)
      (send self toggle 'normal)
      (refresh)
      (play-audio sound))

    (send self draw 'normal))

  (define-element-template %fancy-toggle%
      [sound] (:template %toggle-base%)

    (define (draw/refresh state)
      (send self draw state)
      (refresh))
    (define (up-state)
      (if in? 'prelight 'normal))
    
    (define in? #f)
    (send self draw 'normal)

    (on mouse-enter (event)
      (if (mouse-grabbed?)
          (draw/refresh 'active)
          (draw/refresh 'prelight))
      (set! in? #t))
    (on mouse-leave (event)
      (draw/refresh 'normal)
      (set! in? #f))
    (on mouse-down (event)
      (grab-mouse self)
      (draw/refresh 'active))
    (on mouse-up (event)
      (draw/refresh (up-state)) 
      (when (mouse-grabbed?)
        (ungrab-mouse self)
        (when in?
          (send self toggle (up-state))
          (refresh)
          (play-audio sound)))))

;;   (define (point->boring-button-rectangle at text)
;;     (define bounds (move-rect-center-to (measure-text $login-button-style text)
;;                                         at))
;;     (inset-rect bounds -5))
  
;;   (define-element-template %boring-button%
;;       [at text action]
;;       (:template %zone% :shape (point->boring-button-rectangle at text))
;;     (define bounds (prop self bounds))
;;     (draw-box bounds $color-white)
;;     (draw-text $login-button-style (inset-rect bounds 5) text)
;;     (on mouse-down (event)
;;       (action)))

(define (points->rect p1 p2)
  (rect (min (point-x p1) (point-x p2))
        (min (point-y p1) (point-y p2))
        (max (point-x p1) (point-x p2))
        (max (point-y p1) (point-y p2))))

(define-element-template %rect-drawing%
    [[border :default 2]
     [rect-width :default 2]] 
    (:template %zone%)
  
  (define active-color (color #x00 #xFF #x00))
  (define drawn-color (color #xFF #x00 #x00))
  (define my-bounds (bounds (prop self shape)))
  (define start-point (point 0 0))
  (define end-point (point 0 0))
  (save-graphics my-bounds)

  (draw-box-outline my-bounds (color #x00 #x00 #x00) border) 

  (on get-end-and-draw (my-color)
    (let ((pos (mouse-position)))
      (set! end-point (point (min (max (rect-left my-bounds) 
                                       (point-x pos))
                                  (rect-right my-bounds))
                             (min (max (rect-top my-bounds)
                                       (point-y pos))
                                  (rect-bottom my-bounds)))))
    (restore-graphics my-bounds)
    (draw-box-outline my-bounds (color #x00 #x00 #x00) border) 
    (draw-box-outline (points->rect start-point end-point) 
                      my-color rect-width))

  (on mouse-down (event)
    (grab-mouse self)
    (set! start-point (event-position event)))
  (on mouse-up (event)
    (when (mouse-grabbed?)
      (ungrab-mouse self)
      (send self get-end-and-draw drawn-color)))
  (on idle (event)
    (when (mouse-grabbed?)
      (send self get-end-and-draw active-color))))

;;   (define-element-template %selector%
;;       [choices sound] ()

;;     (define choice-elements 
;;       (map (fn (x) (create (cdr x))) choices))
    
;;     (define selected (car choice-elements))
)