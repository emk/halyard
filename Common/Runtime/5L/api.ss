(module API (lib "lispish.ss" "5L")

  (require (lib "kernel.ss" "5L"))
  (provide (all-from (lib "kernel.ss" "5L")))

  (require (lib "shapes.ss" "5L"))
  (provide (all-from (lib "shapes.ss" "5L")))
  
  (require (lib "layout.ss" "5L"))
  (provide (all-from (lib "layout.ss" "5L")))


  ;;=======================================================================
  ;;  Support Modules
  ;;=======================================================================
  
  (provide with-tracing)
  (require (lib "trace.ss" "5L"))
  (set-trace-output-printer! debug-log)

  (require (rename (lib "match.ss") match-let match-let))
  (provide match-let)

  ;; Make sure this gets loaded.  It will register itself with the kernel.
  (require (lib "tags.ss" "5L"))
  
  
  ;;;======================================================================
  ;;;  Useful Syntax
  ;;;======================================================================

  (provide fn callback deferred-callback while for
           define-engine-variable define/p)

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
  (define-syntax-indent fn 1)

  ;;; Create an anonymous function object with no parameters.
  ;;;
  ;;; @syntax (callback body ...)
  ;;; @param BODY body The body of the function.
  (define-syntax callback
    (syntax-rules ()
      [(callback code ...)
       (lambda () (begin/var code ...))]))
  (define-syntax-indent callback 0)

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
  (define-syntax-indent deferred-callback 0)

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
  (define-syntax-indent while 0)

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
         (when cond
           (begin/var body ...)
           (loop next-value)))]))
  (define-syntax-indent for 1)

  ;;; Bind a Scheme variable name to a 5L engine variable.
  ;;;
  ;;; @syntax (define-engine-variable name 5L-name &opt init-val)
  ;;; @param NAME name The Scheme name to use.
  ;;; @param NAME 5L-name The corresponding name in the 5L engine.
  ;;; @opt EXPRESSION init-val The initial value of the variable.
  ;;; @xref engine-var set-engine-var!
  (define-syntax define-engine-variable
    (syntax-rules ()
      [(define-engine-variable name 5l-name init-val)
       (begin
         (define-symbol-macro name (engine-var '5l-name ))
         (maybe-initialize-engine-variable '5l-name init-val))]
      [(define-engine-variable name 5l-name)
       (define-symbol-macro name (engine-var '5l-name))]))
  (define-syntax-indent define-engine-variable 2)

  (define (maybe-initialize-engine-variable 5l-name init-val)
    ;; A private helper for define-engine-variable.  We only initialize
    ;; a variable if it doesn't already exist, so it can keep its value
    ;; across script reloads.
    (unless (engine-var-exists? 5l-name)
      (set! (engine-var 5l-name) init-val)))

  ;;; Define a persistent global variable which keeps its value across
  ;;; script reloads.  Note that two persistent variables with the same
  ;;; name, but in different modules, are essentially the same variable.
  ;;; Do not rely on this fact--it may change.
  ;;;
  ;;; @syntax (define/p name init-val)
  ;;; @param NAME name The name of the variable.
  ;;; @param EXPRESSION init-val The initial value of the variable.
  (define-syntax define/p
    (syntax-rules ()
      [(define/p name init-val)
       (define-engine-variable name name init-val)]))
  (define-syntax-indent define/p 1)

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
           $screen-rect *stylesheet-list*)

  ;;; The maximum horizontal position of the last text drawn.
  (define-engine-variable *text-x*    _INCR_X     0)

  ;;; The maximum vertical position of the last text drawn.
  (define-engine-variable *text-y*    _INCR_Y     0)

  ;;; The maximum horizontal position of the last graphic shown.
  (define-engine-variable *graphic-x* _Graphic_X  0)

  ;;; The maximum vertical position of the last graphic shown.
  (define-engine-variable *graphic-y* _Graphic_Y  0)

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
  (define-syntax-indent with-saved-text-position 0)

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
  (define-syntax-indent with-saved-graphic-position 0)

  ;;; @type RECT The screen rectangle, in global co-ordinates.
  (define $screen-rect (rect 0 0 800 600))

  ;;; Holds a list of all registered stylesheets
  (define *stylesheet-list* (list))


  ;;;======================================================================
  ;;;  Mathematical Primitives
  ;;;======================================================================

  (provide real->string)

  ;;; Format a real number as "x.xxx".
  ;;;
  ;;; @param REAL n The number to format.
  ;;; @param INTEGER places The number of places past the decimal point.
  (define (real->string n places)
    (define (pad-with-zeros str wanted-length)
      (if (>= (string-length str) wanted-length)
          str
          (pad-with-zeros (string-append "0" str) wanted-length)))
    (define sign (if (< n 0) "-" ""))
    ;; PORTING - This next line isn't completely portable Scheme--too many
    ;; behaviors of the number hierarchy are undefined.
    (define scaled (inexact->exact (round (* (abs n) (expt 10 places)))))
    (define str (pad-with-zeros (number->string scaled) (+ places 1)))
    (define split-pos (- (string-length str) places))
    (cat sign
         (substring str 0 split-pos)
         "."
         (substring str split-pos (string-length str))))
    

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
    (point (engine-var '_originx)
           (engine-var '_originy)))

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
      (call-5l-prim 'resetorigin p)
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
       (let* [[old (origin)]
              [new (point-offset old by)]]
         (dynamic-wind
             (lambda () (set! (origin) new))
             (lambda () (begin/var body ...))
             (lambda () (set! (origin) old))))]))
  (define-syntax-indent with-offset-origin 1)


  ;;;======================================================================
  ;;;  Text Drawing
  ;;;======================================================================
  ;;;  5L text drawing uses stylesheets.
  
  (provide (rename register-style stylesheet)
           stylesheet? stylesheet-name stylesheet-long-name stylesheet-family
           stylesheet-size stylesheet-flags
           stylesheet-justification stylesheet-color
           stylesheet-highlight-color stylesheet-height-adjustment
           stylesheet-shadow-offset stylesheet-shadow-color
           stylesheet-highlight-shadow-color
           define-stylesheet measure-text draw-text)

  (define-struct stylesheet
    (name long-name family size flags justification
     color highlight-color height-adjustment
     shadow-offset shadow-color
     highlight-shadow-color)
    (make-inspector))
  
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
    (call-5l-prim 'defstyle
                  (stylesheet-long-name sheet)
                  (stylesheet-family sheet)
                  (stylesheet-size sheet)
                  (flags->defstyle-flags (stylesheet-flags sheet))
                  (stylesheet-justification sheet)
                  (stylesheet-color sheet)
                  (stylesheet-highlight-color sheet)
                  (stylesheet-height-adjustment sheet)
                  (stylesheet-shadow-offset sheet)
                  (stylesheet-shadow-color sheet)
                  (stylesheet-highlight-shadow-color sheet))
    ;; XXX - Ugly hack for stylesheet display utilities in VTRA program.
    ;; (We don't want to display anonymous stylesheets, though.)
    (when (stylesheet-name sheet)
      (set! *stylesheet-list* (cons sheet *stylesheet-list*))))

  ;; Internal hash table containing the STYLESHEET-LONG-NAME of all
  ;; registered stylesheets.
  (define *registered-styles* (make-hash-table))

  ;; Helper: If we haven't already registered an equivalent style sheet,
  ;; register this one.
  (define (maybe-register-defstyle sheet)
    (define name (stylesheet-long-name sheet))
    (unless (hash-table-get *registered-styles* name (lambda () #f))
      (hash-table-put! *registered-styles* name #t)
      (register-defstyle sheet)))

  ;; Helper: Creates nice, long, X11-inspired style names so we never
  ;; register any duplicates.
  (define (style->long-name name family size flags justification
                            text-color highlight-color height-adjustment
                            shadow-offset shadow-color
                            highlight-shadow-color)
    (define c color->hex-string)
    (define (v-or-p value)
      (if (percent? value)
          (cat (percent-value value) "%")
          value))
    (symcat family "-" size "-" (flags->defstyle-flags flags) "-"
            justification "-" (c text-color) "-"
            (c highlight-color) "-" (v-or-p height-adjustment) "-"
            shadow-offset "-" (c shadow-color) "-"
            (c highlight-shadow-color)))

  ;; An internal helper function which does all the heavy lifting.
  (define (register-style &key
                          [name #f]
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
                               shadow-color)])
    (let* [[long-name (style->long-name name family size flags justification
                                        text-color highlight-color
                                        height-adjustment shadow-offset
                                        shadow-color highlight-shadow-color)]
           [sheet (make-stylesheet name long-name family size flags
                                   justification text-color highlight-color
                                   height-adjustment shadow-offset shadow-color
                                   highlight-shadow-color)]]
      (maybe-register-defstyle sheet)
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
       (define name (register-style :name 'name args ...))]))
  (define-syntax-indent define-stylesheet 1)

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
    (call-5l-prim 'textaa (stylesheet-long-name style) r text))
  
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
      (call-5l-prim 'measuretextaa (stylesheet-long-name style) msg max-width)
      (rect 0 0
            (engine-var '_text_width)
            (engine-var '_text_height))))


  ;;;======================================================================
  ;;;  State DB
  ;;;======================================================================
  ;;;  In most applications, it's necessary to update the GUI based on
  ;;;  some internal application state.  If both the GUI and the internal
  ;;;  state are complicated, the resulting application can be a mess.
  ;;;
  ;;;  One popular way to reduce this complexity is to use the
  ;;;  model-view-controller paradigm (see the web for details).  We choose
  ;;;  a similar approach: We create a global state database with
  ;;;  hierarchial keys of the form /foo/bar/baz.  To get data from the
  ;;;  state database, you need to register a listener, which will be run
  ;;;  immediately.  The engine will keep track of the data accessed, and
  ;;;  re-run the listener whenever any of that data changes.
  ;;;
  ;;;  A listener is a special kind of function.  You can create a
  ;;;  listener using DEFINE-STATE-DB-LISTENER, DEFINE-STATE-DB-LISTENER/RT,
  ;;;  STATE-DB-FN, or STATE-DB-FN/RT.  The RT versions are implemented in
  ;;;  a special, non-consing subset of Scheme which should not cause
  ;;;  the garbage collector to be invoked.
  ;;;
  ;;;  This is an advanced language feature, and simple Tamale programs
  ;;;  will almost never need to use it.

  (provide set-state-db! register-state-db-fn!
           state-db-fn state-db-fn/rt
           define-state-db-fn define-state-db-fn/rt
           define-state-db-listener define-state-db-listener/rt)
  
  ;;; Set the specified key in the state database.
  ;;;
  ;;; @param SYMBOL key The key to set.
  ;;; @param ANY val The new value.
  (define (set-state-db! key value)
    (call-5l-prim 'StateDbSet key value))

  ;;; Register a listener with the state database, and call the listener
  ;;; the first time.
  ;;;
  ;;; @param NODE node The node to which this listener should be attached.
  ;;; @param LISTENER listener 
  (define (register-state-db-fn! node fn)
    (call-5l-prim 'StateDbRegisterListener (node-full-name node) fn))

  (define (make-state-db-fn f)
    (fn (listener-name listener-serial-number)
      (define (state-db key)
        (call-5l-prim 'StateDbGet listener-name listener-serial-number key))
      (f state-db)))

  ;;; Create a function suitable for passing to REGISTER-STATE-DB-FN!.
  ;;;
  ;;; @syntax (STATE-DB-FN (state-db) body ...)
  ;;; @param FUNCTION state-db A function which will fetch data from the
  ;;;   state database.
  ;;; @param BODY body The code to run.
  (define-syntax state-db-fn
    (syntax-rules ()
      [(state-db-fn (state-db) . body)
       (make-state-db-fn (fn (state-db) . body))]))
  (define-syntax-indent state-db-fn 1)

  ;;; Create a function suitable for passing to REGISTER-STATE-DB-FN!.
  ;;; This function is written in a special, non-consing dialect of Scheme.
  ;;;
  ;;; @syntax (STATE-DB-FN/RT (state-db) [binding ...] body ...)
  ;;; @param FUNCTION state-db A function which will fetch data from the
  ;;;   state database.
  ;;; @param NAME binding A name from the enclosing lexical scope which
  ;;;   should be made available within the body.  Note that STATE-DB-FN/RT
  ;;;   will immediate create a read-only copy of the original binding; the
  ;;;   code body will not see any future updates from Scheme.
  ;;; @param BODY body The code to run.  For more documentation on the
  ;;;   subset of Scheme supported here, consult the engine source code
  ;;;   or experiment.
  (define-syntax (state-db-fn/rt stx)
    (define (expand-binding binding-stx)
      (unless (symbol? (syntax-object->datum binding-stx))
        (error "state-db-fn/rt binding must be symbol"))
      (quasisyntax/loc binding-stx (list '#,binding-stx #,binding-stx)))
    (define (expand-bindings bindings-stx)
      (map expand-binding (syntax->list bindings-stx)))
    (syntax-case stx ()
      [(state-db-fn/rt (state-db) bindings . body)
       (quasisyntax/loc
        stx
        (make <realtime-state-db-listener>
          :getter-name 'state-db
          :bindings (list #,@(expand-bindings #'bindings))
          :code 'body))]))
  (define-syntax-indent state-db-fn/rt 2)

  ;;; Equivalent to (define name (state-db-fn (state-db) ...)).
  ;;;
  ;;; @syntax (define-state-db-fn (name state-db) . body)
  (define-syntax define-state-db-fn
    (syntax-rules ()
      [(define-state-db-fn (name state-db) . body)
       (define name (state-db-fn (state-db) . body))]))
  (define-syntax-indent define-state-db-fn 1)
  
  ;;; Equivalent to (define name (state-db-fn/rt (state-db) ...)).
  ;;;
  ;;; @syntax (define-state-db-fn (name state-db) . body)
  (define-syntax define-state-db-fn/rt
    (syntax-rules ()
      [(define-state-db-fn/rt (name state-db) . body)
       (define name (state-db-fn/rt (state-db) . body))]))
  (define-syntax-indent define-state-db-fn/rt 1)

  ;;; Combines the features of REGISTER-STATE-DB-FN! and STATE-DB-FN.
  ;;;
  ;;; @syntax (DEFINE-STATE-DB-LISTENER (name state-db) body ...)
  ;;; @syntax (DEFINE-STATE-DB-LISTENER name value)
  (define-syntax (define-state-db-listener stx)
    (syntax-case stx ()
      [(define-state-db-listener (name state-db) . body)
       (quasisyntax/loc
        stx
        (define-state-db-listener name (state-db-fn (state-db) . body)))]
      [(define-state-db-listener name value)
       (quasisyntax/loc
        stx
        ;; We ignore #'NAME, but we use it to get the lexical context in
        ;; which a reasonable SELF variable is defined.
        (register-state-db-fn! #,(datum->syntax-object #'name 'self) value))]))
  (define-syntax-indent define-state-db-listener 1)
  
  ;;; Combines the features of REGISTER-STATE-DB-FN! and STATE-DB-FN/RT.
  ;;;
  ;;; @syntax (DEFINE-STATE-DB-LISTENER (name state-db) [binding ...] body ...)
  (define-syntax define-state-db-listener/rt
    (syntax-rules ()
      [(define-state-db-listener/rt (name state-db) bindings . body)
       (define-state-db-listener name
         (state-db-fn/rt (state-db) bindings . body))]))
  (define-syntax-indent define-state-db-listener/rt 2)

  ) ; end module
