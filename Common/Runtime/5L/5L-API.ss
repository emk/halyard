(module 5L-API (lib "lispish.ss" "5L")

  (require 5L-Kernel)
  (provide (all-from 5L-Kernel))


  ;;=======================================================================
  ;;  Support Modules
  ;;=======================================================================
  
  (provide with-tracing)
  (require (lib "trace.ss" "5L"))
  (set-trace-output-printer! debug-log)

  
  ;;;======================================================================
  ;;;  Useful Syntax
  ;;;======================================================================

  (provide fn callback deferred-callback while for foreach
	   define-engine-variable define-persistent-variable)

  ;;; Create an anonymous function object (which can be passed as a
  ;;; callback to many routines).  This is just an alias for Scheme's
  ;;; standard 'lambda' form.
  ;;;
  ;;; @syntax (fn arglist body ...)
  ;;; @param ARGLIST arglist A list of Scheme function parameters.
  ;;; @param BODY body The body of the function.
  (define-syntax fn
    (syntax-rules ()
      [(fn arglist code ...)
       (lambda arglist (begin/var code ...))]))

  ;;; Create an anonymous function object with no parameters.
  ;;;
  ;;; @syntax (callback body ...)
  ;;; @param BODY body The body of the function.
  (define-syntax callback
    (syntax-rules ()
      [(callback code ...)
       (lambda () (begin/var code ...))]))

  ;;; Create an anonymous function object with no parameters.  When
  ;;; invoked, this function object may run immediately, or it may run at
  ;;; a later time.  You should use deferred callbacks for code
  ;;; which needs to be run in response to a mouse event or key press, and
  ;;; which needs to play video, request user input or sleep.
  ;;;
  ;;; The callback will always return false.
  ;;;
  ;;; @syntax (callback body ...)
  ;;; @param BODY body The body of the function.
  (define-syntax deferred-callback
    (syntax-rules ()
      [(deferred-callback code ...)
       (callback (call-at-safe-time (callback code ...)))]))

  ;;; Run a body of code until a condition is met.
  ;;;
  ;;; @syntax (while condition body ...)
  ;;; @param EXPRESSION condition The condition to evaluate each time through
  ;;;   the loop.  This is tested before entering the loop for the first
  ;;;   time, so it's possible to skip the while loop entirely.
  ;;; @param BODY body The code to run.
  (define-syntax while
    (syntax-rules ()
      [(while cond body ...)
       (when cond
	 (let loop []
	   (begin/var body ...)
	   (when cond
	     (loop))))]))

  ;;; Run a body of code until a condition is met, updating a loop variable
  ;;; as specified.  This works in a fashion similar to C's 'for' loop.
  ;;;
  ;;; @syntax (for [name init-value cond next-value] body ...)
  ;;; @param NAME name The name of the loop variable.
  ;;; @param EXPRESSION init-value The initial value of 'name' before starting
  ;;;   the loop.
  ;;; @param EXPRESSION cond This expression is tested before each pass
  ;;;   through the loop.  If it returns false, the loop will end.
  ;;; @param EXPRESSION next-value An expression to calculate the value of
  ;;;   'name' for the next trip through the loop.
  (define-syntax for
    (syntax-rules ()
      [(for [name init-value cond next-value] body ...)
       (let loop [[name init-value]]
	 (begin/var body ...)
	 (if cond
	     (loop next-value)))]))

  ;;; Run a body once for each item in a list.
  ;;;
  ;;; @syntax (foreach [name list] body ...)
  ;;; @param NAME name The variable to use as the item name.
  ;;; @param LIST list The list from which to get the items.
  ;;; @param BODY body The code to run for each list item.
  (define-syntax foreach
    (syntax-rules ()
      [(foreach [name lst] body ...)
       (for-each (lambda (name) (begin/var body ...)) lst)]))

  ;;; Bind a Scheme variable name to a 5L engine variable.
  ;;;
  ;;; @syntax (define-engine-variable name 5L-name type &opt init-val)
  ;;; @param NAME name The Scheme name to use.
  ;;; @param NAME 5L-name The corresponding name in the 5L engine.
  ;;; @param NAME type The type of the variable.
  ;;; @opt EXPRESSION init-val The initial value of the variable.
  ;;; @xref engine-var set-engine-var!
  (define-syntax define-engine-variable
    (syntax-rules ()
      [(define-engine-variable name 5l-name type init-val)
       (begin
	 (define-symbol-macro name (engine-var '5l-name 'type))
	 (maybe-initialize-engine-variable '5l-name 'type init-val))]
      [(define-engine-variable name 5l-name type)
       (define-symbol-macro name (engine-var '5l-name 'type))]))

  (define (maybe-initialize-engine-variable 5l-name type init-val)
    ;; A private helper for define-engine-variable.  We only initialize
    ;; a variable if it doesn't already exist, so it can keep its value
    ;; across script reloads.
    (unless (call-5l-prim 'BOOL 'VariableExists 5l-name)
      (set! (engine-var 5l-name type) init-val)))

  ;;; Define a global variable which keeps its value across script
  ;;; reloads.  Note that two persistent variables with the same name,
  ;;; but in different modules, are essentially the same variable.
  ;;; Do not rely on this fact--it may change.
  ;;;
  ;;; @syntax (define-persistent-variable name type init-val)
  ;;; @param NAME name The name of the variable.
  ;;; @param NAME type The type of the variable.
  ;;; @param EXPRESSION init-val The initial value of the variable.
  (define-syntax define-persistent-variable
    (syntax-rules ()
      [(define-persistent-variable name type init-val)
       (define-engine-variable name name type init-val)]))

  ;;; @define SYNTAX with-tracing
  ;;;
  ;;; Trace execution of a code body by dumping information to the
  ;;; debug log.  This is very handy.  For now, this can't be used to
  ;;; wrap an global function definition--it must be used within the
  ;;; body of the global function.
  ;;;
  ;;; @syntax (with-tracing body ...)
  ;;; @param BODY body The code to trace.


  ;;;======================================================================
  ;;;  Standard Engine Variables
  ;;;======================================================================
  ;;;  The 5L engine exports a variety of special-purpose variables.
  ;;;
  ;;;  @xref define-engine-variable

  (provide *text-x* *text-y* *graphic-x* *graphic-y*
	   text-position set-text-position! with-saved-text-position
	   graphic-position set-graphic-position! with-saved-graphic-position
	   $screen-rect)

  ;;; The maximum horizontal position of the last text drawn.
  (define-engine-variable *text-x*    _INCR_X    INTEGER)

  ;;; The maximum vertical position of the last text drawn.
  (define-engine-variable *text-y*    _INCR_Y    INTEGER)

  ;;; The maximum horizontal position of the last graphic shown.
  (define-engine-variable *graphic-x* _Graphic_X INTEGER)

  ;;; The maximum vertical position of the last graphic shown.
  (define-engine-variable *graphic-y* _Graphic_Y INTEGER)

  ;;; @return POINT The point (point *text-x* *text-y*).
  ;;; @xref *text-x* *text-y* 
  (define (text-position)
    (point *text-x* *text-y*))

  ;;; Set *text-x* and *text-y* to the specified point.
  ;;; @param POINT p The new value to use.
  ;;; @xref *text-x* *text-y* 
  (define (set-text-position! p)
    (set! *text-x* (point-x p))
    (set! *text-y* (point-y p)))

  ;;; @return POINT The point (point *graphic-x* *graphic-y*).
  ;;; @xref *graphic-x* *graphic-y* 
  (define (graphic-position)
    (point *graphic-x* *graphic-y*))

  ;;; Set *graphic-x* and *graphic-y* to the specified point.
  ;;; @param POINT p The new value to use.
  ;;; @xref *graphic-x* *graphic-y* 
  (define (set-graphic-position! p)
    (set! *graphic-x* (point-x p))
    (set! *graphic-y* (point-y p)))

  ;;; Save (text-position) while executing a body, and restore it
  ;;; afterwards.
  ;;;
  ;;; @syntax (with-saved-text-position body ...)
  ;;; @xref text-position
  (define-syntax with-saved-text-position
    (syntax-rules ()
      [(with-saved-text-position body ...)
       (let [[saved #f]]
	 (dynamic-wind
	     (lambda () (set! saved (text-position)))
	     (lambda () (begin/var body ...))
	     (lambda () (set! (text-position) saved))))]))

  ;;; Save (graphic-position) while executing a body, and restore it
  ;;; afterwards.
  ;;;
  ;;; @syntax (with-saved-graphic-position body ...)
  ;;; @xref graphic-position
  (define-syntax with-saved-graphic-position
    (syntax-rules ()
      [(with-saved-graphic-position body ...)
       (let [[saved #f]]
	 (dynamic-wind
	     (lambda () (set! saved (graphic-position)))
	     (lambda () (begin/var body ...))
	     (lambda () (set! (graphic-position) saved))))]))  

  ;;; @type RECT The screen rectangle, in global co-ordinates.
  (define $screen-rect (rect 0 0 640 480))

  
  ;;;======================================================================
  ;;;  Geometric Primitives
  ;;;======================================================================

  (provide point-offset point-difference rect-offset
	   rect-width rect-height rect-left-top rect-left-bottom
	   rect-right-top rect-right-bottom)

  ;;; Move a point by the specified amount.
  ;;;
  ;;; @param POINT p The point to move.  Not modified.
  ;;; @param POINT by The offset by which to move the point.
  ;;; @return POINT The moved point.
  (define (point-offset p by)
    (point (+ (point-x p) (point-x by))
	   (+ (point-y p) (point-y by))))

  ;;; Subtract p2 from p1.
  ;;;
  ;;; @param POINT p1 A point.
  ;;; @param POINT p2 The point to subtract.
  ;;; @return POINT The result of the subtraction.
  (define (point-difference p1 p2)
    (point (- (point-x p1) (point-x p2))
	   (- (point-y p1) (point-y p2))))

  ;;; Move a rectangle by the specified amount.
  ;;;
  ;;; @param POINT r The rectangle to move.  Not modified.
  ;;; @param POINT by The offset by which to move the rectangle.
  ;;; @return POINT The moved rectangle.
  (define (rect-offset r by)
    (rect (+ (rect-left r) (point-x by))
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
    (point (rect-left r) (rect-top r)))
  
  ;;; @return POINT The left bottom corner of the rectangle.
  (define (rect-left-bottom r)
    (point (rect-left r) (rect-bottom r)))
  
  ;;; @return POINT The right top corner of the rectangle.
  (define (rect-right-top r)
    (point (rect-right r) (rect-top r)))
  
  ;;; @return POINT The right bottom corner of the rectangle.
  (define (rect-right-bottom r)
    (point (rect-right r) (rect-bottom r)))


  ;;;======================================================================
  ;;;  Origin
  ;;;======================================================================
  ;;;  5L supports a moveable graphics origin.  All drawing commands
  ;;;  take place relative to the current origin.

  (provide origin set-origin! offset-origin with-offset-origin)

  ;;; Get the current origin.
  ;;;
  ;;; @return POINT The current origin, in absolute global co-ordinates.
  ;;; @legacy _originx _originy
  (define (origin)
    (point (engine-var '_originx 'INTEGER)
	   (engine-var '_originy 'INTEGER)))

  ;;; Set the current origin to the specified absolute global co-ordinates,
  ;;; and update all the engine's position-related variables appropriately.
  ;;; (You probably want to call offset-origin instead of this function.)
  ;;;
  ;;; @param POINT p The new origin, in absolute global co-ordinates.
  ;;; @xref offset-origin
  ;;; @legacy ResetOrigin
  (define (set-origin! p)
    (let* [[old (origin)]
	   [delta (point-difference p old)]]
      (call-5l-prim 'VOID 'resetorigin p)
      (set! (text-position) (point-offset (text-position) delta))
      (set! (graphic-position) (point-offset (graphic-position) delta))))
  
  ;;; Move the origin relative to its current position.
  ;;;
  ;;; @param POINT by The relative position to move the origin to.
  ;;; @xref set-origin!
  ;;; @legacy Origin
  (define (offset-origin by)
    ;; Just call set-origin! to save ourselves the trouble of messing
    ;; around with all the variables again.
    (set! (origin) (point-offset (origin) by)))

  ;;; Run a body of code with an origin offset relative to its current
  ;;; position.
  ;;;
  ;;; @syntax (WITH-OFFSET-ORIGIN by body ...)
  ;;; @param POINT by The relative offset for the origin.
  ;;; @param BODY body The code to run.
  ;;; @xref offset-origin
  ;;; @legacy Origin ResetOrigin
  (define-syntax with-offset-origin
    (syntax-rules ()
      [(with-offset-origin by body ...)
       (let [[old (origin)]
	     [new (point-offset old by)]]
	 (dynamic-wind
	     (lambda () (set! (origin) new))
	     (lambda () (begin/var body ...))
	     (lambda () (set! (origin) old))))]))


  ;;;======================================================================
  ;;;  Text Drawing
  ;;;======================================================================
  ;;;  5L text drawing uses stylesheets.
  
  (provide stylesheet? stylesheet-name stylesheet-family
	   stylesheet-size stylesheet-flags
	   stylesheet-justification stylesheet-color
	   stylesheet-highlight-color stylesheet-height-adjustment
	   stylesheet-shadow-offset stylesheet-shadow-color
	   stylesheet-highlight-shadow-color
	   stylesheet-is-input-style? ; Deprecated.
	   define-stylesheet measure-text draw-text)

  (define-struct stylesheet
    (name family size flags justification
     color highlight-color height-adjustment
     shadow-offset shadow-color
     highlight-shadow-color
     windows-adjustment
     is-input-style?
     input-background-color)
    (make-inspector))
  
  ;; Helper: Given a stylesheet, register a corresponding header for
  ;; legacy support.
  (define (register-header sheet)
    ;; XXX - Colors are hard-coded until the engine is modified to
    ;; stop using palette values everywhere.
    (if (have-5l-prim? 'header)
	(call-5l-prim 'VOID 'header
		      (stylesheet-name sheet)
		      ;; Generate a fake header fontname.
		      (cat (if (member? 'bold (stylesheet-flags sheet)) "b" "")
			   "ser"
			   (number->string (stylesheet-size sheet)))
		      (stylesheet-justification sheet)
		      (stylesheet-color sheet)
		      (if (stylesheet-is-input-style? sheet)
			  (stylesheet-input-background-color sheet)
			  (stylesheet-highlight-color sheet))
		      (stylesheet-shadow-offset sheet)
		      (stylesheet-shadow-color sheet)
		      (stylesheet-windows-adjustment sheet)
		      (stylesheet-highlight-shadow-color sheet)
		      )))

  ;; Helper: Convert a list of flags to a defstyle flag value.
  (define (flags->defstyle-flags flags)
    (cond
     [(null? flags) 'r]
     [(equal? flags '(bold)) 'b]
     [(equal? flags '(italic)) 'i]
     [(equal? flags '(bold italic)) 'bi]
     [(equal? flags '(italic bold)) 'bi]
     [else
      (throw (cat "Unknown stylesheet flags: " flags))]))

  ;; Helper: Given a stylesheet, register a corresponding defstyle.
  (define (register-defstyle sheet)
    (call-5l-prim 'VOID 'defstyle
		  (stylesheet-name sheet)
		  (stylesheet-family sheet)
		  (stylesheet-size sheet)
		  (flags->defstyle-flags (stylesheet-flags sheet))
		  (stylesheet-justification sheet)
		  (stylesheet-color sheet)
		  (stylesheet-highlight-color sheet)
		  (stylesheet-height-adjustment sheet)
		  (stylesheet-shadow-offset sheet)
		  (stylesheet-shadow-color sheet)
		  (stylesheet-highlight-shadow-color sheet)))

  ;; An internal helper function which does all the heavy lifting.
  (define (register-style name
			  &key
			  [base #f]
			  [family (if base (stylesheet-family base) "Times")]
			  [size (if base (stylesheet-size base) 12)]
			  [flags (if base (stylesheet-flags base) '())]
			  [justification
			   (if base (stylesheet-justification base) 'left)]
			  [text-color :color
			   (if base
			       (stylesheet-color base)
			       (color #xFF #xFF #xFF))]
			  [highlight-color
			   (if base
			       (stylesheet-highlight-color base)
			       (color #xFF #xFF #xFF))]
			  [height-adjustment
			   (if base (stylesheet-height-adjustment base) 0)]
			  [shadow-offset
			   (if base (stylesheet-shadow-offset base) 0)]
			  [shadow-color
			   (if base
			       (stylesheet-shadow-color base)
			       (color #x00 #x00 #x00))]
			  [highlight-shadow-color
			   (if base
			       (stylesheet-highlight-shadow-color base)
			       shadow-color)]
			  ;; Deprecated parameter for header support.
			  [windows-adjustment
			   (if base (stylesheet-windows-adjustment base) 0)]
			  ;; Deprecated parameters for input support.
                          [is-input-style?
			   (if base (stylesheet-is-input-style? base) #f)]
			  [input-background-color
			   (if base
			       (stylesheet-input-background-color base)
			       (color #x00 #x00 #x00))])
    (let [[sheet (make-stylesheet name family size flags justification
				  text-color highlight-color height-adjustment
				  shadow-offset shadow-color
				  highlight-shadow-color
				  windows-adjustment
				  is-input-style?
				  input-background-color)]]
      (register-defstyle sheet)
      (register-header sheet)
      sheet))

  ;;; Define a new stylesheet for drawing text.
  ;;;
  ;;; @syntax (DEFINE-STYLESHEET name &key base family size flags justification
  ;;;                            color highlight-color height-adjustment
  ;;;                            shadow-offset shadow-color 
  ;;;                            highlight-shadow-color windows-adjustment)
  ;;; @param SYMBOL name The name of this stylesheet.
  ;;; @key STYLESHEET base An optional stylesheet from which to default
  ;;;   other keyword arguments.
  ;;; @key STRING family The font family to use.
  ;;; @key STRING size The font size.
  ;;; @key LIST:SYMBOL flags One of '(), '(bold), '(italic) or '(bold italic).
  ;;; @key SYMBOL justification One of 'left, 'center or 'right.
  ;;; @key COLOR color The text color.
  ;;; @key COLOR highlight-color The text highlight color.
  ;;; @key PERCENT leading Change in the default leading.
  ;;; @key INTEGER shadow-offset The offset for the drop, or 0, for no drop
  ;;;   shadow.  Positive offsets are down and to the right.
  ;;; @key COLOR shadow-color The color to use for the shadow.
  ;;; @key COLOR highlight-shadow-color The color to use the shadow of
  ;;;   highlighted text.
  ;;; @key INTEGER windows-adjustment The amount to adjust the first
  ;;;   baseline by when using the legacy Windows text subsystem.
  ;;; @xref draw-text input
  ;;; @legacy defstyle header
  (define-syntax define-stylesheet
    (syntax-rules ()
      [(define-stylesheet name args ...)
       (define name (register-style 'name args ...))]))

  ;;; Draw a string of text.
  ;;;
  ;;; @param STYLESHEET style The stylesheet to use.
  ;;; @param RECT r The recentangle in which to draw the text.
  ;;;   The text may extend below the bottom of this box if necessary.
  ;;; @param STRING text The text to draw.
  ;;; @xref measure-text text-position *text-x* *text-y*
  ;;; @legacy text textaa
  (define (draw-text style r text)
    ;; XXX - textaa uses an idiosyncratic formating language.
    (call-5l-prim 'VOID 'textaa (stylesheet-name style) r text))
  
  ;;; Measure a string of text.
  ;;;
  ;;; @param STYLESHEET style The stylesheet to use.
  ;;; @param STRING text The text to measure.
  ;;; @key STRING max-width The maximum horizontal space which may be used
  ;;;   for laying out the text.  Defaults to the screen width.
  ;;; @return RECT A rectangle large enough to hold the text, with an
  ;;;   an origin at 0,0.
  ;;; @xref draw-text
  (define (measure-text style msg
			&key (max-width (rect-width $screen-rect)))
    ;;; XXX - We can't measure anything but left-aligned text accurately.
    (with-saved-text-position
      (call-5l-prim 'VOID 'measuretextaa (stylesheet-name style) msg max-width)
      (rect 0 0
	    (engine-var '_text_width 'INTEGER)
	    (engine-var '_text_height 'INTEGER))))

  ) ; end module
