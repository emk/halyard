(module interpolate (lib "language.ss" "halyard")
  (require (lib "util.ss" "mizzen"))
  (require (lib "types.ss" "halyard"))

  (provide number->integer interpolate-value)

  ;;; Convert any number to an integer.  Typically needed for use with
  ;;; INTERPOLATE-VALUE and ANIMATE.
  (define (number->integer n)
    (inexact->exact (round n)))

  (define (interpolate-float fraction from to)
    (+ from (* fraction (- to from))))

  ;;; Return a value FRACTION percent of the distance between FROM and
  ;;; TO.
  (defgeneric (interpolate-value (fraction <real>)
                                 (from <object>) (to <object>)))

  (defmethod (interpolate-value (fraction <real>)
                                (from <integer>) (to <integer>))
    (number->integer (interpolate-float fraction from to)))

  (defmethod (interpolate-value (fraction <real>) 
                                (from <real>) (to <real>))
    (interpolate-float fraction from to))

  (defmethod (interpolate-value (fraction <real>)
                                (from <point>) (to <point>))
    (elem-map-2 (curry interpolate-value fraction) from to))

  (defmethod (interpolate-value (fraction <real>)
                                (from <rect>) (to <rect>))
    (elem-map-2 (curry interpolate-value fraction) from to))

  (defmethod (interpolate-value (fraction <real>)
                                (from <color>) (to <color>))
    (elem-map-2 (curry interpolate-value fraction) from to))

  )
