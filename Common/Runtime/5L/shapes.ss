;;=========================================================================
;;  Miscellaneous Shape-Manipulation Routines
;;=========================================================================
;;  We should probably look in tamale.ss and see if we can drag more stuff
;;  into here.

(module shapes (lib "lispish.ss" "5L")
  (require (lib "types.ss" "5L"))
  
  (provide bounds)
  
  (defgeneric (bounds (shape <shape>)))

  (defmethod (bounds (shape <rect>))
    shape)
  
  (defmethod (bounds (shape <polygon>))
    (define pts (polygon-vertices shape))
    (if (null? pts)
        (rect 0 0 0 0)
        (let loop [[left (point-x (car pts))]
                   [top (point-y (car pts))]
                   [right (point-x (car pts))]
                   [bottom (point-y (car pts))]
                   [pts (cdr pts)]]
          (if (null? pts)
              (rect left top right bottom)
              (loop (min left (point-x (car pts)))
                    (min top (point-y (car pts)))
                    (max right (point-x (car pts)))
                    (max bottom (point-y (car pts)))
                    (cdr pts))))))
  
  (defmethod (as (type (singleton <polygon>)) (r <rect>))
    (polygon (point (rect-left r) (rect-top r)) 
             (point (rect-right r) (rect-top r))
             (point (rect-right r) (rect-bottom r))
             (point (rect-left r) (rect-bottom r)))))


