(module deprecated (lib "5l.ss" "5L")
  ;; Things which have been removed entirely:
  ;;
  ;;   origin set-origin! offset-origin with-offset-origin
  ;;   opacity timeout
  ;;
  ;; Things which have been removed, but which can be handled by a simple
  ;; search & replace:
  ;;
  ;;   rect-offset -> offset-rect
  ;;   point-offset -> offset-point
  ;;   point-in-rect? -> point-in-shape?
  ;;   point-in-poly? -> point-in-shape?
  ;;   offset-shape -> offset-by-point
  ;;   %simple-zone% -> %clickable-zone%
  ;;   zone -> clickable-zone  [* see below]
  ;;   clear-screen -> clear-dc
  ;;   current-card-name -> (node-full-name (current-card))
  ;;   %edit-box-element% -> %edit-box%
  ;;   %sine-wave-element% -> %sine-wave%
  ;;   %movie-element% -> %movie%
  ;;   make-path-from-abstract -> abstract-path->native-path
  ;;   ensure-dir-exists -> ensure-directory-exists
  ;;   measure-picture -> measure-graphic
  ;;   draw-box -> draw-rectangle
  ;;   draw-box-outline -> draw-rectangle-outline
  ;;
  ;; The following functions used to take an element name as their first
  ;; argument.  This has been replaced by an optional keyword parameter
  ;; :NAME.
  ;;
  ;;   clickable-zone browser edit-box geiger-audio
  ;;   sine-wave vorbis-audio movie
  ;;
  ;; The following functions have had parameter changes:
  ;;
  ;;   geiger-synth                  (&rest parameter now regular parameter)
  ;;   draw-picture -> draw-graphic  (point is now first)
  ;;   draw-text                     (rect is now first)
  ;;
  ;; The following templates used to take a :LOCATION parameter.  This has
  ;; been replaced in each case with a :PATH parameter:
  ;;
  ;;   %flash-card% %browser% %geiger-audio% %movie%
  ;;
  ;; The %ZONE% template has been replaced by a %CUSTOM-ELEMENT% template
  ;; and a %BOX% template.  Note that :OVERLAY? defaults to #t for
  ;; %CUSTOM-ELEMENT%, not the old value of #f.
  ;;
  ;; The following functions have been replaced with ON handlers, and may
  ;; be accessed using SEND.  In most cases, this means that element
  ;; references of the form 'FOO are no longer supported; use @FOO instead.
  ;;
  ;;   activex-prop -> activex-prop
  ;;   set-activex-prop! -> set-activex-prop!
  ;;   movie-pause -> pause
  ;;   movie-resume -> resume
  ;;   set-media-volume! -> set-volume!
  ;;   set-geiger-audio-counts-per-second! -> set-counts-per-second!
  ;;
  ;; The following functions still exist, but no longer allow elements to
  ;; be specified by symbols (unless ENABLE-DEPRECATED-FEATURES! has been
  ;; called):
  ;;
  ;;   delete-element delete-elements wait
  ;;
  ;; This also means that you can no longer wait on non-existant elements.
  ;;
  ;; Files which are no longer included in the standard API, but which
  ;; are still available:
  ;;
  ;;   layout.ss
  ;;
  ;; Calling MEASURE-TEXT invalidates your *text-x* and *text-y* values.
  ;; Wrap it in (WITH-SAVED-TEXT-POSITION ...) to preserve the old behavior.
  ;;
  ;; CALL-AT-SAFE-TIME has been replaced by RUN-DEFERRED, which has subtly
  ;; different semantics (see the docs).  This change also affects
  ;; DEFERRED-CALLBACK, of course.
  ;;
  ;; All templates with their own DC must be converted to use ON DRAW
  ;; instead of a bare WITH-DC call.  This can be easy or hard, depending
  ;; on circumstances.  This also means that WITH-DC can be removed from
  ;; DRAW-BUTTON handlers.


  ;;;======================================================================
  ;;;  Transitions and Screenshots
  ;;;======================================================================

  (provide fade unfade screenshot center-text)

  ;;; Fade the screen to black.  Calls REFRESH.
  (define (fade &key (ms 500))
    (clear-dc (color 0 0 0))
    (refresh :transition 'toblack :ms ms))

  ;;; Assuming the screen was black when last refreshed, fade it up from
  ;;; black.
  (define (unfade &key (ms 500))
    (refresh :transition 'fromblack :ms ms))

  ;;; Save a screenshot of the screen to the current working directory.
  (define (screenshot)
    (define (three-char-print n)
      (cond 
       ((> n 999) "000")
       ((> n 99) (format "~a" n))
       ((> n 9) (format "0~a" n))
       ((> n -1) (format "00~a" n))
       (else "000")))

    (define dir (ensure-directory-exists "Screenshots"))
    (call-5l-prim 
     'screenshot 
     (let loop ((count 0))
       (define path (build-path dir (cat (three-char-print count) ".png")))
       (cond
        ((= count 1000) path)
        ((or (file-exists? path) (directory-exists? path))
         (loop (+ count 1)))
        (else path)))))

  ;; Like DRAW-TEXT, but center the text within BOX.
  ;; This function is deprecated for the moment.
  (define (center-text stylesheet box msg &key (axis 'both))
    (define bounds (measure-text stylesheet msg :max-width (rect-width box)))
    (define r
      (case axis
        [[both]
         (move-rect-center-to bounds (rect-center box))]
        [[y]
         (move-rect-left-to
          (move-rect-vertical-center-to bounds (rect-vertical-center box))
          (rect-left box))]
        [[x]
         (move-rect-top-to
          (move-rect-horizontal-center-to bounds (rect-horizontal-center box))
          (rect-top box))]
        [else
         (throw (cat "center-text: Unknown centering axis: " axis))]))
    (draw-text r stylesheet msg))


  ;;;======================================================================
  ;;;  Standard Engine Variables
  ;;;======================================================================
  ;;;  The 5L engine exports a variety of special-purpose variables.  These
  ;;;  have all been deprecated for a long time.
  ;;;
  ;;;  @xref define-engine-variable

  (provide *text-x* *text-y* *graphic-x* *graphic-y*
           text-position set-text-position! with-saved-text-position
           graphic-position set-graphic-position! with-saved-graphic-position)

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
   
  )
