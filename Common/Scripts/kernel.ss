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
;;  Assertions
;;=========================================================================

(define (%kernel-assert label value)
  (when (not value)
    ;; TODO - Make this error fatal.
    (fatal-error (cat "Assertion failure: " label))))

(define-syntax assert
  (syntax-rules ()
    [(assert cond)
     (%kernel-assert 'cond cond)]))


;;=========================================================================
;;  Utility Functions
;;=========================================================================

(define (value->string value)
  (if (string? value)
      value
      (let ((str-port (open-output-string)))
	(write value str-port)
	(get-output-string str-port))))

(define (cat . values)
  (if (not (null? values))
      (string-append (value->string (car values)) (apply cat (cdr values)))
      ""))

(define-syntax label
  (syntax-rules ()
    ((label name body ...)
     (call-with-current-continuation (lambda (name) body ...)))))

(define (call-with-errors-blocked report-func thunk)
  (let* ((result (with-handlers ([void (lambda (exn) (cons #f exn))])
		   (cons #t (thunk))))
	 (good? (car result))
	 (exn-or-value (cdr result)))
    (if good?
	exn-or-value
	(begin
	  (report-func (exn-message exn-or-value))
	  #f))))

(define-syntax with-errors-blocked
  (syntax-rules ()
    [(with-errors-blocked (report-func) body ...)
     (call-with-errors-blocked report-func (lambda () body ...))]))


;;=========================================================================
;;  5L API
;;=========================================================================

(define (call-5l-prim . args)
  (let ((result (apply %call-5l-prim args)))
    (%kernel-check-flags)
    result))

(define (idle)
  (call-5l-prim 'schemeidle))

(define (log msg)
  (call-5l-prim 'log '5L msg 'log))

(define (debug-log msg)
  (call-5l-prim 'log 'Debug msg 'log))

(define (caution msg)
  (call-5l-prim 'log '5L msg 'caution))

(define (debug-caution msg)
  (call-5l-prim 'log 'Debug msg 'caution))

(define (non-fatal-error msg)
  (call-5l-prim 'log '5L msg 'error))

(define (fatal-error msg)
  (call-5l-prim 'log '5L msg 'fatalerror))

(define (engine-var name)
  (call-5l-prim 'get name))

(define (set-engine-var! name value)
  ;; Useless performance hack.
  (if (string? value)
      (call-5l-prim 'set name value)
      (call-5l-prim 'set name (value->string value))))

(define (err msg)
  ;; TODO - More elaborate error support.
  (non-fatal-error msg)
  (error msg))

(define (exit-script)
  (call-5l-prim 'schemeexit))

(define (jump card)
  (assert *%kernel-exit-to-top-level-func*)
  (set! *%kernel-jump-card* (%kernel-find-card card))
  (debug-log (cat "Jumping to: " *%kernel-jump-card*))
  (*%kernel-exit-to-top-level-func*))


;;=========================================================================
;;  Cards
;;=========================================================================

(define *%kernel-exit-interpreter-func* #f)
(define *%kernel-exit-to-top-level-func* #f)
(define *%kernel-exit-interpreter?* #f)
(define *%kernel-jump-card* #f)

(define (%kernel-check-flags)
  (when (and *%kernel-exit-interpreter?*
	     *%kernel-exit-interpreter-func*)
    (*%kernel-exit-interpreter-func*)))

(define-struct %kernel-card (name thunk))

(define *%kernel-card-table* (make-hash-table))

(define (%kernel-register-card card)
  (let ((name (%kernel-card-name card)))
    (when (hash-table-get *%kernel-card-table* name (lambda () #f))
      (err (cat "Duplicate card: " name)))
    (hash-table-put! *%kernel-card-table* name card)))

(define (%kernel-run-card card)
  (debug-log (cat "Begin card: <" (%kernel-card-name card) ">"))
  (with-errors-blocked (non-fatal-error)
    ((%kernel-card-thunk card))))

(define (%kernel-find-card card-or-name)
  (cond
    [(%kernel-card? card-or-name)
     card-or-name]
    [(symbol? card-or-name)
     (let ((card (hash-table-get *%kernel-card-table*
				 card-or-name
				 (lambda () #f))))
       (or card (err (cat "Unknown card: " card-or-name))))]
    [(string? card-or-name)
     (%kernel-find-card (string->symbol card-or-name))]
    [#t
     (err (cat "Bogus argument: " card-or-name))]))

(define-syntax card
  (syntax-rules ()
    [(card name body ...)
     (begin
       (define name (make-%kernel-card 'name (lambda () body ...)))
       (%kernel-register-card name))]))


;;=========================================================================
;;  Kernel Entry Points
;;=========================================================================
;;  The '%kernel-' methods are called directly by the 5L engine.  They
;;  shouldn't raise errors, because they're called directly from C++
;;  code that doesn't want to catch them (and will, in fact, quit
;;  the program).

(define (%kernel-run)
  (with-errors-blocked (fatal-error)
    (label exit-interpreter
      (set! *%kernel-exit-interpreter-func* exit-interpreter)
      (let loop ()
	(idle)
	(label exit-to-top-level
          (set! *%kernel-exit-to-top-level-func* exit-to-top-level)
	  (cond
	   [*%kernel-jump-card*
	    (let ((jump-card *%kernel-jump-card*))
	      (set! *%kernel-jump-card* #f)
	      (%kernel-run-card (%kernel-find-card jump-card)))]
	   [#t
	    (debug-log "Doing nothing in idle loop")]))
	(set! *%kernel-exit-to-top-level-func* #f)
	(loop)))
    (set! *%kernel-exit-interpreter-func* #f)))

(define (%kernel-kill-interpreter)
  (set! *%kernel-exit-interpreter?* #t))

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

(define (%kernel-jump-to-card-by-name card-name)
  (with-errors-blocked (fatal-error)
    (debug-log (cat "jump-to-card-by-name: " card-name))
    (set! *%kernel-jump-card* card-name)))

(define (%kernel-current-card-name)
  "")

(define (%kernel-previous-card-name)
  "")
