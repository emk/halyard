(module types (lib "lispish.ss" "5L")

  ;;=======================================================================
  ;;  Built-in Types
  ;;=======================================================================
  ;;  These methods implement various "built-in" types that are known to
  ;;  the 5L engine.  They should *never* raise errors, because they're
  ;;  called directly from C++ code that isn't prepared to cope with Scheme
  ;;  errors.

  (provide <point> (rename make-point point) point?
           point-x set-point-x! point-y set-point-y!

           <rect> (rename make-rect rect) rect?
           rect-left set-rect-left! rect-top set-rect-top!
           rect-right set-rect-right! rect-bottom set-rect-bottom!

           <color> (rename make-color-opt-alpha color) color?
           color-red set-color-red! color-green set-color-green!
           color-blue set-color-blue! color-alpha set-color-alpha!
           color->hex-string

           <percent> (rename make-percent percent) percent? percent-value
           
           <polygon> polygon polygon? 
           polygon-vertices polygon-bounds

           <shape> shape? shape-origin

           <realtime-state-db-listener> realtime-state-db-listener?
           realtime-state-db-listener-getter-name
           realtime-state-db-listener-bindings
           realtime-state-db-listener-code
           )
  
  (define (bare-class-name class)
    ;; Get a class's name without the extra <>.
    (define name (symbol->string (class-name class)))
    (substring name 1 (- (string-length name) 1)))

  (define (simple-printer object escape? port)
    (define (print-slot slot)
      (display " " port)
      (print (slot-ref object (car slot)) port))
    (display "(" port)
    (display (bare-class-name (class-of object)) port)
    (for-each print-slot (class-slots (class-of object)))
    (display ")" port))

  (defclass <point> ()
    x y
    :printer simple-printer)

  (make-equals?-compare-class+slots <point>)

  (defclass <shape> ())
  
  (defgeneric (shape-origin (shape <shape>)))

  (defclass <rect> (<shape>)
    left top right bottom
    :printer simple-printer)
  
  (defmethod (shape-origin (shape <rect>))
    (rect-left-top shape))

  (make-equals?-compare-class+slots <rect>)

  (defclass <color> ()
    red green blue alpha
    :printer simple-printer)

  (define (make-color-opt-alpha r g b &opt (a 255))
    (make-color r g b a))

  (make-equals?-compare-class+slots <color>)

  (define (channel->hex-string value)
    (let recurse [[str (format "~x" value)]]
      (if (< (string-length str) 2)
          (recurse (string-append "0" str))
          str)))

  (define (color->hex-string c)
    (string-append (channel->hex-string (color-red c))
                   (channel->hex-string (color-green c))
                   (channel->hex-string (color-blue c))
                   (channel->hex-string (color-alpha c))))

  (defclass <percent> () 
    value
    :printer simple-printer)

  (make-equals?-compare-class+slots <percent>)

  (define (polygon-printer object escape? port)
    (define (print-value value)
      (display " " port)
      (print value port))
    (display "(" port)
    (display (bare-class-name (class-of object)) port)
    (for-each print-value (polygon-vertices object))
    (display ")" port))

  (defclass <polygon> (<shape>)
    vertices bounds
    :printer polygon-printer)
  
  (defmethod (shape-origin (shape <polygon>))
    (rect-left-top (polygon-bounds shape)))

  (make-equals?-compare-class+slots <polygon>)

  (define (polygon &rest args)
    (make-polygon args (calculate-polygon-bounds args)))
  
  (define (calculate-polygon-bounds verts)
    (if (null? verts)
      (make-rect 0 0 0 0)
      (foldl (lambda (point bounds)
               (let ((px (point-x point))
                     (py (point-y point))
                     (minx (rect-left bounds))
                     (miny (rect-top bounds))
                     (maxx (rect-right bounds))
                     (maxy (rect-bottom bounds)))
                 (make-rect (min px minx)
                            (min py miny)
                            (max px maxx)
                            (max py maxy))))
             (make-rect (point-x (first verts)) (point-y (first verts))
                        (point-x (first verts)) (point-y (first verts)))
             (rest verts))))
 
  (defclass <realtime-state-db-listener> ()
    getter-name bindings code)


  ;;;======================================================================
  ;;;  Geometric Primitives
  ;;;======================================================================

  (provide point-offset point-difference rect-offset
           rect-width rect-height rect-left-top rect-left-bottom
           rect-right-top rect-right-bottom

           copy-rect rect-horizontal-center rect-vertical-center
           rect-center move-rect-left-to move-rect-top-to
           move-rect-horizontal-center-to move-rect-vertical-center-to
           move-rect-center-to)

  ;;; Move a point by the specified amount.
  ;;;
  ;;; @param POINT p The point to move.  Not modified.
  ;;; @param POINT by The offset by which to move the point.
  ;;; @return POINT The moved point.
  (define (point-offset p by)
    (make-point (+ (point-x p) (point-x by))
                (+ (point-y p) (point-y by))))

  ;;; Subtract p2 from p1.
  ;;;
  ;;; @param POINT p1 A point.
  ;;; @param POINT p2 The point to subtract.
  ;;; @return POINT The result of the subtraction.
  (define (point-difference p1 p2)
    (make-point (- (point-x p1) (point-x p2))
           (- (point-y p1) (point-y p2))))

  ;;; Move a rectangle by the specified amount.
  ;;;
  ;;; @param POINT r The rectangle to move.  Not modified.
  ;;; @param POINT by The offset by which to move the rectangle.
  ;;; @return POINT The moved rectangle.
  (define (rect-offset r by)
    (make-rect (+ (rect-left r) (point-x by))
               (+ (rect-top r) (point-y by))
               (+ (rect-right r) (point-x by))
               (+ (rect-bottom r) (point-y by))))

  ;;; @return INTEGER The width of the rectangle.
  (define (rect-width r)
    (- (rect-right r) (rect-left r)))
  
  ;;; @return INTEGER The height of the rectangle.
  (define (rect-height r)
    (- (rect-bottom r) (rect-top r)))
  
  ;;; @return POINT The left top corner of the rectangle.
  (define (rect-left-top r)
    (make-point (rect-left r) (rect-top r)))
  
  ;;; @return POINT The left bottom corner of the rectangle.
  (define (rect-left-bottom r)
    (make-point (rect-left r) (rect-bottom r)))
  
  ;;; @return POINT The right top corner of the rectangle.
  (define (rect-right-top r)
    (make-point (rect-right r) (rect-top r)))
  
  ;;; @return POINT The right bottom corner of the rectangle.
  (define (rect-right-bottom r)
    (make-point (rect-right r) (rect-bottom r)))

  (define (copy-rect r)
    (make-rect (rect-left r) (rect-top r)
               (rect-right r) (rect-bottom r)))  

  (define (rect-horizontal-center r)
    (+ (rect-left r) (round (/ (- (rect-right r) (rect-left r)) 2))))
  
  (define (rect-vertical-center r)
    (+ (rect-top r) (round (/ (- (rect-bottom r) (rect-top r)) 2))))
  
  (define (rect-center r)
    (make-point (rect-horizontal-center r) (rect-vertical-center r)))
  
  (define (move-rect-left-to r h)
    (make-rect h (rect-top r) (+ h (rect-width r)) (rect-bottom r)))

  (define (move-rect-top-to r v)
    (make-rect (rect-left r) v (rect-right r) (+ v (rect-height r))))

  (define (move-rect-horizontal-center-to r x)
    (move-rect-left-to r (- x (round (/ (rect-width r) 2)))))

  (define (move-rect-vertical-center-to r y)
    (move-rect-top-to r (- y (round (/ (rect-height r) 2)))))

  (define (move-rect-center-to r p)
    (move-rect-horizontal-center-to (move-rect-vertical-center-to r
                                                                  (point-y p))
                                    (point-x p)))
  
  )
