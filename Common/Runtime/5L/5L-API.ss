(module 5L-API (lib "lispish.ss" "5L")

  (require 5L-Kernel)
  (provide (all-from 5L-Kernel))

  ;;=======================================================================
  ;;  Geometric Primitives
  ;;=======================================================================

  (provide rect-width rect-height rect-left-top rect-left-bottom
	   rect-right-top rect-right-bottom)

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
  
  
  ;;=======================================================================
  ;;  Useful Syntax
  ;;=======================================================================

  (provide fn callback)

  (define-syntax fn
    (syntax-rules ()
      [(fn arglist code ...)
       (lambda arglist code ...)]))

  (define-syntax callback
    (syntax-rules ()
      [(callback code ...)
       (lambda () code ...)]))

  ) ; end module
