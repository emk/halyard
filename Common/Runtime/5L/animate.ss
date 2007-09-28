(module animate (lib "5l.ss" "5L")

  ;;=======================================================================
  ;;  Utility routines
  ;;=======================================================================
  ;;  The names and details of these functions are extremely likely to
  ;;  change.

  ;; Helpful functions for listing the frames of an animation.
  (provide range prefix-and-suffix zero-pad)

  ;; Return a list of numbers from START to END, inclusive, increasing by
  ;; STEP.
  (define (range start end &opt (step 1))
    (define (range-internal start)
      (cond 
        ((> start end) '())
        ((= start end) (list end))
        (else (cons start (range-internal (+ start step))))))
    (range-internal start))

  (define (prefix-and-suffix prefix lst suffix)
    (map (fn (x) (cat prefix x suffix))
         lst))

  (define (zero-pad n x)
    (define str (cat x))
    (cat (make-string (- n (string-length str)) #\0) str))
  
  
  ;;;======================================================================
  ;;;  General Animation Support
  ;;;======================================================================
  ;;;  TODO - Some API details, especially names, may change slightly.

  (provide ease-in ease-out ease-in/out
           interpolate play-sprite slide reshape slide-and-reshape
           simultaneously do-nothing after immediately finally quantize 
           animate)

  ;; Given x(0), x(1), dx/dt(0), and dx/dt(1), fit a cubic polynomial to
  ;; those four constraints and evaluate it.
  (define (eval-cubic-1d-spline x0 x1 dx/dt0 dx/dt1 t)
    ;; The equation for this spline was determined using the following
    ;; commands in Maxima, a GPL'd version of MacSyma.  (Below, xp(t) is
    ;; dx/dt at t.)
    ;;
    ;;   x(t) := a*t^3 + b*t^2 + c*t + d;
    ;;   define(xp(t), diff(x(t),t));
    ;;   solve([x(0)=x0,x(1)=x1,xp(0)=xp0,xp(1)=xp1],[a,b,c,d]);
    ;;
    ;; Maxima reported the following result:
    ;;
    ;;   a = xp1 + xp0 - 2 x1 + 2 x0
    ;;   b = - xp1 - 2 xp0 + 3 x1 - 3 x0
    ;;   c = xp0
    ;;   d = x0
    (let [[a (+ (* 2 x0) (* -2 x1) dx/dt0 dx/dt1)]
          [b (+ (* -3 x0) (* 3 x1) (* -2 dx/dt0) (* -1 dx/dt1))]
          [c dx/dt0]
          [d x0]]
      (+ (* a (expt t 3)) (* b (expt t 2)) (* c t) d)))

  (define (ease-anim anims x0 x1 dx/dt0 dx/dt1)
    (fn ()
      (define draw-func ((apply simultaneously anims)))
      (fn (fraction)
        (draw-func (eval-cubic-1d-spline x0 x1 dx/dt0 dx/dt1 fraction)))))

  ;;; Begin ANIMS gradually.
  (define (ease-in &rest anims) (ease-anim anims 0 1 0 1))
  (define-syntax-indent ease-in 0)

  ;;; End ANIMS gradually.
  (define (ease-out &rest anims) (ease-anim anims 0 1 1 0))
  (define-syntax-indent ease-out 0)

  ;;; Begin and end ANIMS gradually.
  (define (ease-in/out &rest anims) (ease-anim anims 0 1 0 0))
  (define-syntax-indent ease-in/out 0)

  ;;; Creates an animator that interpolates a given expression (which can 
  ;;; be read and SET!) from its initial value.
  ;;;
  ;;; XXX - Handle nested "places" of the form (interpolate (f1 (f2 x))
  ;;; final) in a more graceful fashion.
  (define-syntax interpolate
    (syntax-rules ()
      [(_ var final) 
       ;; Wrapped in a let so the semantics match that of all other
       ;; animators, which is that the FINAL value is evaluated at the
       ;; time the animator is constructed, since they are just
       ;; functions.
       (let [[end final]]
         (fn ()
           (define start var)
           (fn (fraction)
             (define old var)
             (define new (interpolate-value fraction start end))
             (unless (equals? new old)
               (set! var new)))))]))

  ;;; Similar to (interpolate (prop obj prop-name final)), but considerably
  ;;; faster, because it resolves object paths once up front.
  (define-syntax interpolate-prop
    (syntax-rules ()
      [(_ obj prop-name final)
       (let [[obj (resolve obj)]]
         (interpolate (prop obj prop-name) final))]))
  
  ;;; Returns an animator that plays through the frames of a %SPRITE%.
  (define (play-sprite sprite &key (reverse? #f))
    (interpolate-prop sprite frame
                      (if reverse? 0 (- (length (prop sprite frames)) 1))))

  ;;; This function creates a simple DRAW-FUNC for use with ANIMATE.  The
  ;;; resulting function moves OBJs from the point FROM to the point TO
  ;;; over the duration of the animation.
  (define (slide elem to)
    (interpolate-prop elem at to))

  ;;; Return an animator that resizes an element to a given rect.  Mostly 
  ;;; useful for changing the size of a rectangle or rectangle-outline element.
  (define (reshape elem final-rect)
    (interpolate-prop elem shape final-rect))

  ;;; Return an animator that changes the rect for an element to a given rect 
  ;;; (both shape and position).
  (define (slide-and-reshape elem new-rect)
    (simultaneously 
      (slide elem (shape-origin new-rect))
      (reshape elem (move-shape-left-top-to new-rect (point 0 0)))))

  ;;; Create a function that will invoke multiple animations simultaneously
  ;;; (when it is called).
  (define (simultaneously &rest anim-thunks)
    (fn ()
      (define draw-funcs (map (fn (x) (x)) anim-thunks))
      (fn (fraction)
        (foreach [f draw-funcs]
          (f fraction)))))
  (define-syntax-indent simultaneously 0)

  ;;; An animation which does nothing at all.  Most useful with AFTER.
  (define (do-nothing)
    (fn ()
      (fn (fraction)
        #f)))

  ;; Internal support for AFTER macro.
  (define (after-helper &rest args)
    ;; A whole bunch of helper functions for implementing AFTER. All
    ;; of he helper functions that don't close over the state of the
    ;; animation are defined here; the ones that do close over the
    ;; state are defined in the animation thunk.
    (define (add-zero lst)
      (if (= (caar lst) 0.0)
        lst
        (append (list (cons 0.0 (do-nothing))) lst)))
    (define (split lst) ; [(a,b)] -> ([a],[b])
      (let loop [[times '()] [anims '()] [lst lst]]
        (cond
         [(null? lst)
          (cons (reverse times) (reverse anims))]
         [else (loop (cons (caar lst) times)
                     (cons (cdar lst) anims)
                     (cdr lst))])))
    ;; TODO - this is confusing, comment it.
    (define (index-for-time start-times time)
      (let loop [[c 0] [times start-times]]
        (cond
         [(or (null? times) (null? (cdr times))) 
          (error "Bad arguments passed to index-for-time" start-times time)]
         [(and (null? (cddr times)) (<= (car times) time (cadr times)))
          c]
         [(and (<= (car times) time) (< time (cadr times))) c]
         [else (loop (+ c 1) (cdr times))])))
    (define (scaled-time start end time)
      (assert (<= start time end))
      (if (= start end)
          1.0
          (/ (- time start) (- end start))))
    (fn ()
      (define last-index 0)
      (define anim-fns '())
      (define split-list (split (add-zero args)))
      ;; We add an extra 1.0 to the times list to make all of the time
      ;; range calculations easier (so they can just treat this 1.0 as
      ;; the end of the range, instead of having a special case).
      (define times (append (car split-list) (list 1.0)))
      (define thunks (cdr split-list))
      ;; This function returns the animation function for a given
      ;; index, evaluating its thunk if it hasn't already been
      ;; evaluated. 
      ;; NOTE: due to our condition that in an AFTER clause, each
      ;; animation thunk is only evaluated after all previous
      ;; animations have completed, it is incorrect to call this
      ;; function for a given index before all previous animation
      ;; functions have been completed (called with argument 1.0).
      (define (anim-fn-for-index index)
        (if (< index (length anim-fns))
          (list-ref anim-fns index)
          (begin 
            (assert (= index (length anim-fns)))
            (let [[anim-fn ((list-ref thunks index))]]
              (set! anim-fns (append anim-fns (list anim-fn)))
              anim-fn))))
      (fn (fraction) 
        (define index (index-for-time times fraction))
        (let loop [[n last-index]]
          (if (= n index)
            ((anim-fn-for-index n) 
             (scaled-time (list-ref times index) 
                          (list-ref times (+ index 1))
                          fraction))
            (begin ((anim-fn-for-index n) 1.0)
                   (loop (+ n 1)))))
        (set! last-index index))))

  ;;; Play animations in sequence, each one starting at a given fraction.
  (define-syntax after
    (syntax-rules ()
      [(_ [frac anim ...] ...)
       (after-helper (cons frac (simultaneously anim ...)) ...)]))
  (define-syntax-indent after 0)

  ;;; Create a new animation that runs ANIMS instantly, at the beginning
  ;;; of the animation, and does nothing for the remainder.
  (define (immediately &rest anims)
    (after 
      [0.0 (apply simultaneously anims)]
      [0.0]))
  (define-syntax-indent immediately 0)

  ;;; Create a new animation that does nothing for the entire period of 
  ;;; the animation, and then runs ANIMS instantly at the end
  (define (finally &rest anims)
    (after 
      [1.0 (apply simultaneously anims)]))
  (define-syntax-indent finally 0)

  ;;; For performance reasons, limit ANIMS to occur in approximately STEPS
  ;;; discrete steps.
  (define (quantize steps &rest anims)
    (fn ()
      (define draw-func ((apply simultaneously anims)))
      (fn (frac)
        (if (= frac 1.0)
          (draw-func 1.0) ; Pass 1.0 exactly, without rounding errors.
          (draw-func (/ (round (* steps frac)) steps))))))
  (define-syntax-indent quantize 1)

  ;;; Run ANIMS over the course of MILLISECONDS.
  (define (animate milliseconds &rest anims)
    (define start-time (current-milliseconds))
    (define end-time (+ start-time milliseconds))
    (define draw-func ((apply simultaneously anims)))
    (draw-func 0.0)
    (let loop []
      (let [[current-time (current-milliseconds)]]
        (when (< current-time end-time)
          (let* [[elapsed-time (- current-time start-time)]
                 [fraction (/ (* 1.0 elapsed-time) milliseconds)]]
            (draw-func fraction)
            ;; We call IDLE, then REFRESH.  The IDLE makes sure that any
            ;; movies and the OS have a chance to do whatever they want,
            ;; and the REFRESH makes sure that we update the screen for
            ;; each frame, even if the OS is slow about sending repaint
            ;; events.
            (idle)
            (refresh)
            (loop)))))
    (draw-func 1.0))
  (define-syntax-indent animate 1)
  
  )
