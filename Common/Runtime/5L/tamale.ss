;;=========================================================================
;; Random Tamale Primitives
;;=========================================================================
;; This is a collection of loosely documented and poorly-organized Tamale
;; primitives, all subject to change at a moment's notice.

(module tamale (lib "5l.ss" "5L")

  (provide load-picture modal-input zone delete-element delete-elements
           clear-screen rect-horizontal-center rect-vertical-center
           rect-center move-rect-left-to move-rect-top-to
           move-rect-horizontal-center-to move-rect-vertical-center-to
           move-rect-center-to center-text html edit-box movie
           wait tc draw-box inset-rect timeout)

  (define (load-picture name p &key (subrect :rect #f))
    (let [[path (build-path (current-directory) "Graphics" name)]]
      (unless (file-exists? path)
        (throw (cat "No such graphic: " path)))
      (if subrect
          (call-5l-prim 'loadsubpic path p subrect)
          (call-5l-prim 'loadpic path p))))

  (define (modal-input r size forecolor backcolor)
    (call-5l-prim 'input r size forecolor backcolor)
    (engine-var '_modal_input_text))
  
  (define (zone name r action)
    (call-5l-prim 'zone name r action))
  
  (define (delete-element name)
    (delete-elements (list name)))
  
  (define (delete-elements &opt (names '()))
    (apply call-5l-prim 'deleteelements names))
  
  (define (clear-screen c)
    (call-5l-prim 'screen c))
  
  (define (rect-horizontal-center r)
    (+ (rect-left r) (round (/ (- (rect-right r) (rect-left r)) 2))))
  
  (define (rect-vertical-center r)
    (+ (rect-top r) (round (/ (- (rect-bottom r) (rect-top r)) 2))))
  
  (define (rect-center r)
    (point (rect-horizontal-center r) (rect-vertical-center r)))
  
  (define (move-rect-left-to r h)
    (rect h (rect-top r) (+ h (rect-width r)) (rect-bottom r)))

  (define (move-rect-top-to r v)
    (rect (rect-left r) v (rect-right r) (+ v (rect-height r))))

  (define (move-rect-horizontal-center-to r x)
    (move-rect-left-to r (- x (round (/ (rect-width r) 2)))))

  (define (move-rect-vertical-center-to r y)
    (move-rect-top-to r (- y (round (/ (rect-height r) 2)))))

  (define (move-rect-center-to r p)
    (move-rect-horizontal-center-to (move-rect-vertical-center-to r
                                                                  (point-y p))
                                    (point-x p)))

  (define (center-text stylesheet box msg)
    (define bounds (measure-text stylesheet msg :max-width (rect-width box)))
    (draw-text stylesheet (move-rect-center-to bounds (rect-center box)) msg))

  (define (html name r location)
    (call-5l-prim 'html name r (build-path (current-directory) location)))

  (define (edit-box name r text)
    (call-5l-prim 'editbox name r text))
  
  (define (movie name r location
                 &key controller? audio-only? loop? interaction?)
    (call-5l-prim 'movie name r
                  (build-path (current-directory) "Media" location)
                  controller? audio-only? loop? interaction?))
  
  (define (wait name &key frame)
    (if frame
        (call-5l-prim 'wait name frame)
        (call-5l-prim 'wait name)))
  
  (define (tc arg1 &opt arg2 arg3)
    (cond
     [arg3 (+ (* (+ (* arg1 60) arg2) 30) arg3)]
     [arg2 (+ (* arg1 30) arg2)]
     [else arg1]))
  
  (define (draw-box r c)
    (call-5l-prim 'box r c))
  
  (define (inset-rect r pixels)
    ;; TODO - Rename foo-offset to offset-foo.
    (rect (+ (rect-left r) pixels)
          (+ (rect-top r) pixels)
          (- (rect-right r) pixels)
          (- (rect-bottom r) pixels)))
  
  (define (timeout seconds card)
    (call-5l-prim 'timeout seconds (card-name card)))
  
  )