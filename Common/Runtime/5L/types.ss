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

           <percent> (rename make-percent percent) percent? percent-value
           
           <polygon> polygon polygon? 
           polygon-vertices set-polygon-vertices!

           <shape> shape?

           <realtime-state-db-listener> realtime-state-db-listener?
           realtime-state-db-listener-getter-name
           realtime-state-db-listener-bindings
           realtime-state-db-listener-code
           )
  
  (defclass <point> ()
    x y)

  (make-equals?-compare-class+slots <point>)

  (defclass <shape> ())

  (defclass <rect> (<shape>)
    left top right bottom)

  (make-equals?-compare-class+slots <rect>)

  (defclass <color> ()
    red green blue alpha)

  (define (make-color-opt-alpha r g b &opt (a 255))
    (make-color r g b a))

  (make-equals?-compare-class+slots <color>)

  (defclass <percent> () 
    value)

  (make-equals?-compare-class+slots <percent>)

  (defclass <polygon> (<shape>)
    vertices)

  (make-equals?-compare-class+slots <polygon>)

  (define (polygon &rest args)
    (make-polygon args))

  (defclass <realtime-state-db-listener> ()
    getter-name bindings code)
  
  )
