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

           <shape> shape?)
  
  (defclass <point> ()
    x y)

  (defclass <shape> ())

  (defclass <rect> (<shape>)
    left top right bottom)

  (defclass <color> ()
    red green blue alpha)

  (define (make-color-opt-alpha r g b &opt (a 0))
    (make-color r g b a))

  (defclass <percent> () 
    value)

  (defclass <polygon> (<shape>)
    vertices)

  (define (polygon &rest args)
    (make-polygon args))

  )
