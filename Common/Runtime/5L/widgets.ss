(module widgets (lib "5L.ss" "5L")
  (require (lib "tamale.ss" "5L"))
  (require (lib "shapes.ss" "5L"))
  (provide %simple-toggle% %fancy-toggle% %toggle-base% %rect-drawing%
           %text-box% %slider%
           <graphic> make-graphic graphic? draw-graphic
           <picture> make-picture picture? picture-path picture-rect
                     picture-offset
           <saved-region> make-saved-region saved-region? 
                          saved-region-rect saved-region-offset
           <rect-graphic> make-rect-graphic rect-graphic? 
                          rect-graphic-rect rect-graphic-color 
                          rect-graphic-width
           <null-graphic> make-null-graphic null-graphic?
           <widget-states> make-widget-states widget-states?
           element-exists?)


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

  ;; TODO - need to find the right place for these to live. 

  (define (element-exists? name)
    (memq name (map node-name (group-children (current-card)))))

  (define (play-audio path &key (loop? #f) (name 'audio))
    (when path
      (when (element-exists? name)
        (delete-element (@-by-name name)))
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
          (send self toggle 'prelight)
          (refresh)
          (play-audio sound)))))

;; TODO - move these elsewhere
  (define $color-black (color #x00 #x00 #x00))
  (define $color-white (color #xFF #xFF #xFF))
  (define $color-paper (color #xE4 #xDD #xD2))
  (define $color-offwhite (color #xF0 #xF0 #xF0))
  (define $color-highlight (color #xFF #xD8 #x45))
  (define $color-slider (color #x88 #x88 #x88))
  (define $color-slider-prelight (color #x00 #x00 #xAA))
  (define-stylesheet $my-base-style
    :family "Nimbus Roman No9 L"
    :size 12
    :flags '()
    :justification 'left
    :shadow-offset 0
    :color $color-white
    :shadow-color $color-black
    :highlight-color $color-highlight
    :highlight-shadow-color $color-black
    :height-adjustment (percent -20))
  (define-stylesheet $my-login-style
    :base $my-base-style
    :size 16
    :flags '()
    :justification 'center
    :color $color-black
    :highlight-color (color #xFF #xFF #xCC))
  (define-stylesheet $my-login-button-style
    :base $my-login-style
    :flags '()
    :justification 'left
    :size 18)

  (define (point->text-rectangle 
           at text &key (padding -5) (style $my-login-button-style))
    (define bounds (move-rect-center-to 
                     (measure-text style
                                  text)
                     at))
    (inset-rect bounds padding))

  (define-element-template %text-box% 
      [at text 
       [color :default (color #xFF #xFF #xFF)]] 
      (:template %zone% :shape (rect 0 0 0 0))
    
    (define my-text text)
    (define (my-bounds) (point->text-rectangle at my-text))
    
    (on update-text (new-text)
      (set! my-text new-text))
    (on draw (style)
      (draw-box (my-bounds) color)
      (draw-text $my-login-button-style (inset-rect (my-bounds) 5) text))
    
    (send self draw 'normal))

;; (define-element-template %horizontal-layout%
;;     [at children] ()
;;   (

;;   (define-element-template %boring-button%
;;       [at text action]
;;       (:template %zone% :shape (point->boring-button-rectangle at text))
;;     (define bounds (prop self bounds))
;;     (draw-box bounds $color-white)
;;     (draw-text $login-button-style (inset-rect bounds 5) text)
;;     (on mouse-down (event)
;;       (action)))

  (define (clamp value min-val max-val)
    (max min-val (min max-val value)))

  (define-element-template %slider%
      [rectangle min-value max-value 
       [update-function :default #f]
       init-value]
      (:template %zone% :shape rectangle)
    
    (define value init-value)
    (define grabbed-by-me? #f)
    
    (define left-x (rect-left rectangle))
    (define right-x (rect-right rectangle))
    (define scale (/ (- max-value min-value) (- right-x left-x)))
    (define (x->value x)
      (+ min-value (* scale (- x left-x))))
    (define (value->x val)
      (+ left-x (/ (- val min-value) scale)))
    (define (curr-fill-rect)
      (rect (rect-left rectangle) (rect-top rectangle)
            (value->x value) (rect-bottom rectangle)))
    (define (curr-empty-rect)
      (rect (value->x value) (rect-top rectangle)
            (rect-right rectangle) (rect-bottom rectangle)))
    (define (set-value-from-point! pt)
      (set! value (clamp (x->value (point-x pt)) min-value max-value)))
    
    (on draw (style)
      (draw-box (curr-fill-rect)
                (case style
                  ((prelight active) $color-slider-prelight)
                  (else $color-slider))) 
      (draw-box (curr-empty-rect) $color-white)
      (draw-box-outline rectangle $color-black 1))
    
    (send self draw 'normal)

    (on mouse-down (event)
      (grab-mouse self)
      (set! grabbed-by-me? #t)
      (set-value-from-point! (event-position event))
      (send self draw 'active))
    (on mouse-up (event)
      (when (mouse-grabbed?)
        (set-value-from-point! (event-position event))
        (ungrab-mouse self)
        (set! grabbed-by-me? #f)
        (send self draw (if (point-in-rect? (event-position event) 
                                            rectangle)
                            'prelight
                            'normal))))
    (on mouse-enter (event)
      (send self draw 'prelight))
    (on mouse-leave (event)
      (unless grabbed-by-me? 
        (send self draw 'normal)))
    (on idle (event)
      (when (and grabbed-by-me? (mouse-grabbed?))
        (set-value-from-point! (mouse-position))
        (send self draw 'active))))
    
  (define (points->rect p1 p2)
    (rect (min (point-x p1) (point-x p2))
          (min (point-y p1) (point-y p2))
          (max (point-x p1) (point-x p2))
          (max (point-y p1) (point-y p2))))
  
  ;; Note - you shouldn't ever create two of these on the same screen,
  ;; or use them on screens where it needs to save the background,
  ;; because there is only one save buffer, so each one will clobber
  ;; it. 
  (define-element-template %rect-drawing%
      [[border :default 2]
       [rect-width :default 2]] 
      (:template %zone% :cursor 'cross)
    
    (define active-color (color #x00 #xFF #x00))
    (define drawn-color (color #xFF #x00 #x00))
    (define my-bounds (bounds (prop self shape)))
    (define start-point (point 0 0))
    (define end-point (point 0 0))
    (define grabbed-by-me? #f)
    (save-graphics :bounds my-bounds)
    
    (draw-box-outline my-bounds (color #x00 #x00 #x00) border) 
    
    (on get-end-and-draw (my-color)
      (let ((pos (mouse-position)))
        (set! end-point (point (clamp (point-x pos) 
                                      (rect-left my-bounds)
                                      (rect-right my-bounds))
                               (clamp (point-y pos)
                                      (rect-top my-bounds)
                                      (rect-bottom my-bounds)))))
      (restore-graphics :bounds my-bounds)
      (draw-box-outline my-bounds (color #x00 #x00 #x00) border) 
      (draw-box-outline (points->rect start-point end-point) 
                        my-color rect-width))
    
    (on cleanup ()
      (restore-graphics :bounds my-bounds))
    
    (on mouse-down (event)
      (grab-mouse self)
      (set! grabbed-by-me? #t)
      (set! start-point (event-position event)))
    (on mouse-up (event)
      (when (mouse-grabbed?)
        (ungrab-mouse self)
        (set! grabbed-by-me? #f)
        (send self get-end-and-draw drawn-color)))
    (on idle (event)
      (when (and (mouse-grabbed?) grabbed-by-me?)
        (send self get-end-and-draw active-color))))



  ;;   (define-element-template %selector%
  ;;       [choices sound] ()

  ;;     (define choice-elements 
  ;;       (map (fn (x) (create (cdr x))) choices))
  
  ;;     (define selected (car choice-elements))
  )