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
           polygon-vertices set-polygon-vertices!

           <shape> shape?

           <realtime-state-db-listener> realtime-state-db-listener?
           realtime-state-db-listener-getter-name
           realtime-state-db-listener-bindings
           realtime-state-db-listener-code
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

  (defclass <point> ()
    x y
    :printer simple-printer)

  (make-equals?-compare-class+slots <point>)

  (defclass <shape> ())

  (defclass <rect> (<shape>)
    left top right bottom
    :printer simple-printer)

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
    vertices
    :printer polygon-printer)

  (make-equals?-compare-class+slots <polygon>)

  (define (polygon &rest args)
    (make-polygon args))

  (defclass <realtime-state-db-listener> ()
    getter-name bindings code)
  
  )
