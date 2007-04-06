(module types (lib "lispish.ss" "5L")

  ;;=======================================================================
  ;;  Built-in Types
  ;;=======================================================================
  ;;  These methods implement various "built-in" types that are known to
  ;;  the 5L engine.  They should *never* raise errors, because they're
  ;;  called directly from C++ code that isn't prepared to cope with Scheme
  ;;  errors.

  (provide elem-map elem-map-2

           <point> (rename make-point point) point?
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

           <shape> shape?
           )
  
  (define (bare-class-name class)
    ;; Get a class's name with the extra <>.
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

  ;;; Map F over each of the elements in COLLECTION.
  (defgeneric (elem-map f collection))

  ;;; Map F over each pair of elements (E1,E2), where E1 is from
  ;;; COLLECTION1 and E2 is from COLLECTION2.
  (defgeneric (elem-map-2 f collection1 collection2))


  ;;=======================================================================
  ;;  Points
  ;;=======================================================================

  ;;; A geometrical point on a plane.  X and Y must be integers.
  (defclass <point> ()
    x y
    :printer simple-printer)

  (make-equals?-compare-class+slots <point>)

  (defmethod (elem-map f (p <point>))
    (make-point (f (point-x p)) (f (point-y p))))

  (defmethod (elem-map-2 f (p1 <point>) (p2 <point>))
    (make-point (f (point-x p1) (point-x p2))
                (f (point-y p1) (point-y p2))))

  ;;; The abstract superclass of all two-dimensional shapes.
  (defclass <shape> ())
  

  ;;=======================================================================
  ;;  Rectangles
  ;;=======================================================================

  ;;; A rectangle.  LEFT and TOP are inclusive, RIGHT and BOTTOM are
  ;;; exclusive.
  (defclass <rect> (<shape>)
    left top right bottom
    :printer simple-printer)
  
  (make-equals?-compare-class+slots <rect>)

  (defmethod (elem-map f (r <rect>))
    (make-rect (f (rect-left   r))
               (f (rect-top    r))
               (f (rect-right  r))
               (f (rect-bottom r))))
  
  (defmethod (elem-map-2 f (r1 <rect>) (r2 <rect>))
    (make-rect (f (rect-left   r1) (rect-left   r2))
               (f (rect-top    r1) (rect-top    r2))
               (f (rect-right  r1) (rect-right  r2))
               (f (rect-bottom r1) (rect-bottom r2))))


  ;;=======================================================================
  ;;  Colors
  ;;=======================================================================

  ;;; An RGB color.  Values are 0 to 255 for each component, with an
  ;;; alpha value of 255 being completely opaque.
  (defclass <color> ()
    red green blue alpha
    :printer simple-printer)

  ;;; Make a color, optionally specifying the alpha value (which defaults
  ;;; to fully opaque).  Exported as COLOR.
  (define (make-color-opt-alpha r g b &opt (a 255))
    (make-color r g b a))

  (make-equals?-compare-class+slots <color>)

  (defmethod (elem-map f (c <color>))
    (make-color (f (color-red   c))
                (f (color-green c))
                (f (color-blue  c))
                (f (color-alpha c))))

  (defmethod (elem-map-2 f (c1 <color>) (c2 <color>))
    (make-color (f (color-red   c1) (color-red   c2))
                (f (color-green c1) (color-green c2))
                (f (color-blue  c1) (color-blue  c2))
                (f (color-alpha c1) (color-alpha c2))))
  
  (define (channel->hex-string value)
    (let recurse [[str (format "~x" value)]]
      (if (< (string-length str) 2)
          (recurse (string-append "0" str))
          str)))

  ;;; Convert a color to an RRGGBBAA hexadecimal string.
  (define (color->hex-string c)
    (string-append (channel->hex-string (color-red c))
                   (channel->hex-string (color-green c))
                   (channel->hex-string (color-blue c))
                   (channel->hex-string (color-alpha c))))


  ;;=======================================================================
  ;;  Percentages
  ;;=======================================================================

  ;;; A percentage.  Used only in a few special font-related APIs.
  (defclass <percent> () 
    value
    :printer simple-printer)

  (make-equals?-compare-class+slots <percent>)


  ;;=======================================================================
  ;;  Polygons
  ;;=======================================================================

  (define (polygon-printer object escape? port)
    (define (print-value value)
      (display " " port)
      (print value port))
    (display "(" port)
    (display (bare-class-name (class-of object)) port)
    (for-each print-value (polygon-vertices object))
    (display ")" port))

  ;;; A closed polygon.
  (defclass <polygon> (<shape>)
    vertices bounds
    :printer polygon-printer)
  
  (make-equals?-compare-class+slots <polygon>)

  ;;; Create a new polygon.
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
 
  )
