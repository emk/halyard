;;=========================================================================
;;  Built-in Types
;;=========================================================================
;;  These methods implement various "built-in" types that are known to the
;;  5L engine.  They should *never* raise errors, because they're called
;;  directly from C++ code that isn't prepared to cope with Scheme errors.

(define-struct point (x y))

(define (point x y)
  (make-point x y)) 

(define-struct rect (left top right bottom))

(define (rect left top right bottom)
  (make-rect left top right bottom))

(define (rect-width r)
  (- (rect-right r) (rect-left r)))

(define (rect-height r)
  (- (rect-bottom r) (rect-top r)))

(define (rect-left-top r)
  (point (rect-left r) (rect-top r)))

(define (rect-left-bottom r)
  (point (rect-left r) (rect-bottom r)))

(define (rect-right-top r)
  (point (rect-right r) (rect-top r)))

(define (rect-right-bottom r)
  (point (rect-right r) (rect-bottom r)))

(define-struct color (red green blue alpha))

(define (color r g b a)
  ;; TODO - Alpha should be optional.
  (make-color r g b a))

;;=========================================================================
;;  Entry Points
;;=========================================================================
;;  These methods are called directly by the 5L engine.  They should
;;  *never* raise errors, because they're called directly from C++ code
;;  that isn't prepared to cope with Scheme errors.

(define (%kernel-idle)
  #f)

(define (%kernel-pause)
  #f)

(define (%kernel-wake-up)
  #f)

(define (%kernel-paused?)
  #f)

(define (%kernel-timeout card-name seconds)
  #f)

(define (%kernel-nap tenths-of-seconds)
  #f)

(define (%kernel-napping?)
  #f)

(define (%kernel-kill-nap)
  #f)
	
(define (%kernel-kill-current-card)
  #f)

(define (%kernel-do-redo-script card-name)
  #f)

(define (%kernel-jump-to-card-by-name card-name)
  #f)

(define (%kernel-current-card-name)
  "")

(define (%kernel-previous-card-name)
  "")
