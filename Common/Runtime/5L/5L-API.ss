(module 5L-API (lib "lispish.ss" "5L")

  (require 5L-Kernel)
  (provide (all-from 5L-Kernel))

  ;;=======================================================================
  ;;  Useful Syntax
  ;;=======================================================================

  (provide fn callback while for-each-item define-engine-variable)

  (define-syntax fn
    (syntax-rules ()
      [(fn arglist code ...)
       (lambda arglist code ...)]))

  (define-syntax callback
    (syntax-rules ()
      [(callback code ...)
       (lambda () code ...)]))

  (define-syntax while
    (syntax-rules ()
      [(while cond body ...)
       (when cond
	 (let loop []
	   body ...
	   (when cond
	     (loop))))]))

  (define-syntax for-each-item
    (syntax-rules ()
      [(for-each-item [name lst] body ...)
       (for-each (lambda (name) body ...) lst)]))

  (define-syntax define-engine-variable
    (syntax-rules ()
      [(define-engine-variable name 5l-name type)
       (define-symbol-macro name (engine-var '5l-name 'type))]))


  ;;=======================================================================
  ;;  Standard Engine Variables
  ;;=======================================================================

  (provide *text-x* *text-y* *graphic-x* *graphic-y*)

  (define-engine-variable *text-x*    _INCR_X    INTEGER)
  (define-engine-variable *text-y*    _INCR_Y    INTEGER)
  (define-engine-variable *graphic-x* _Graphic_X INTEGER)
  (define-engine-variable *graphic-y* _Graphic_Y INTEGER)

  
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
  
  ) ; end module
