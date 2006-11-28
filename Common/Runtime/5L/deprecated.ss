(module deprecated (lib "5l.ss" "5L")
  ;; Things which have been removed entirely:
  ;;
  ;;   origin set-origin! offset-origin with-offset-origin
  ;;   opacity
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
  ;;   zone -> clickable-zone
  ;;   clear-screen -> clear-dc
  ;;   current-card-name -> (node-full-name (current-card))
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
  ;; Files which are no longer included in the standard API, but which
  ;; are still available:
  ;;
  ;;   layout.ss
  ;;
  ;; Calling MEASURE-TEXT invalidates your *text-x* and *text-y* values.
  ;; Wrap it in (WITH-SAVED-TEXT-POSITION ...) to preserve the old behavior.


  ;;;======================================================================
  ;;;  Transitions and Screenshots
  ;;;======================================================================

  (provide fade unfade screenshot)

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

    (define dir (ensure-dir-exists "Screenshots"))
    (call-5l-prim 
     'screenshot 
     (let loop ((count 0))
       (define path (build-path dir (cat (three-char-print count) ".png")))
       (cond
        ((= count 1000) path)
        ((or (file-exists? path) (directory-exists? path))
         (loop (+ count 1)))
        (else path)))))



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
