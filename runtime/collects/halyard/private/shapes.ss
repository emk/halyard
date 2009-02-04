;; @BEGIN_LICENSE
;;
;; Halyard - Multimedia authoring and playback system
;; Copyright 1993-2009 Trustees of Dartmouth College
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as
;; published by the Free Software Foundation; either version 2.1 of the
;; License, or (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;; 
;; You should have received a copy of the GNU Lesser General Public
;; License along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
;; USA.
;;
;; @END_LICENSE

;;=========================================================================
;;  Miscellaneous Point & Shape Manipulation Routines
;;=========================================================================

(module shapes (lib "mizzen.ss" "mizzen")
  (require (lib "util.ss" "mizzen"))
  (require (lib "types.ss" "halyard/private"))
  (require (lib "kernel.ss" "halyard/private"))
  
  ;;;======================================================================
  ;;;  Geometric Primitives
  ;;;======================================================================

  (provide offset-point point-difference offset-rect
           rect-width rect-height rect-shape rect-left-top rect-left-bottom
           rect-right-top rect-right-bottom

           copy-rect rect-horizontal-center rect-vertical-center
           rect-center shape-center
           move-rect-left-to move-rect-top-to
           move-rect-right-to move-rect-bottom-to
           move-rect-left-top-to
           move-rect-horizontal-center-to move-rect-vertical-center-to
           move-rect-center-to move-shape-left-top-to center-shape-on
           shape-at?

           point-in-shape? offset-by-point inset-rect shape-origin
           bounding-box)

  ;;; Move a point by the specified amount.
  ;;;
  ;;; @param POINT p The point to move.  Not modified.
  ;;; @param POINT by The offset by which to move the point.
  ;;; @return POINT The moved point.
  (define (offset-point p by)
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
  (define (offset-rect r by)
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
  
  ;;; @return RECT A rect with the same width and height as R, moved to 0,0.
  (define (rect-shape r)
    (shape (rect-width r) (rect-height r)))

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

  ;;; Copy a rectangle.
  (define (copy-rect r)
    (rect (rect-left r) (rect-top r)
          (rect-right r) (rect-bottom r)))  

  ;;; Return the horizontal center of a rectangle.
  (define (rect-horizontal-center r)
    (+ (rect-left r) (round (/ (- (rect-right r) (rect-left r)) 2))))
  
  ;;; Return the vertical center of a rectangle.
  (define (rect-vertical-center r)
    (+ (rect-top r) (round (/ (- (rect-bottom r) (rect-top r)) 2))))
  
  ;;; Return the center of a rectangle.
  (define (rect-center r)
    (point (rect-horizontal-center r) (rect-vertical-center r)))
  
  ;;; Return the center point of the bounding box of SHAPE.
  (define (shape-center obj)
    (rect-center (bounding-box obj)))

  ;;; Create a new rect with the same size and vertical position as R, with
  ;;; the left edge at H.
  (define (move-rect-left-to r h)
    (rect h (rect-top r) (+ h (rect-width r)) (rect-bottom r)))

  ;;; Create a new rect with the same size and horizontal position as R,
  ;;; with the top edge at V.
  (define (move-rect-top-to r v)
    (rect (rect-left r) v (rect-right r) (+ v (rect-height r))))

  ;;; Create a new rect with the same size and vertical position as R,
  ;;; with the right edge at H.
  (define (move-rect-right-to r h)
    (rect (- h (rect-width r)) (rect-top r) h (rect-bottom r)))

  ;;; Create a new rect with the same size and horizontal position as R,
  ;;; with the bottom edge at V.
  (define (move-rect-bottom-to r v)
    (rect (rect-left r) (- (rect-height r) v) (rect-right r) v))

  ;;; Create a new rect with the same size as R, with its left-top corner
  ;;; at P.
  (define (move-rect-left-top-to r p)
    (move-rect-left-to (move-rect-top-to r (point-y p)) (point-x p)))

  ;;; Create a new rect with the same size and vertical position as R, with
  ;;; the horizontal center at X.
  (define (move-rect-horizontal-center-to r x)
    (move-rect-left-to r (- x (round (/ (rect-width r) 2)))))

  ;;; Create a new rect with the same size and horizontal position as R,
  ;;; with the vertical center at Y.
  (define (move-rect-vertical-center-to r y)
    (move-rect-top-to r (- y (round (/ (rect-height r) 2)))))

  ;;; Create a new rect with the same size as R, centered on P.
  (define (move-rect-center-to r p)
    (move-rect-horizontal-center-to (move-rect-vertical-center-to r
                                                                  (point-y p))
                                    (point-x p)))
  
  ;;; Move the left-top corner of the bounding box of SHAPE to the
  ;;; specified point, returning the moved shape.
  (define (move-shape-left-top-to obj p)
    (offset-by-point obj (point-difference p (shape-origin obj))))

  ;;; Center SHAPE relative to ON-SHAPE, returning the centered shape.
  (define (center-shape-on shape on-shape)
    (define new-bounds (move-rect-center-to (bounding-box shape) 
                                            (shape-center on-shape)))
    (define offset (point-difference (rect-left-top new-bounds)
                                     (rect-left-top (bounding-box shape))))
    (offset-by-point shape offset))

  ;;; Return #t if SHAPE is located at the specified point.
  (define (shape-at? shape p)
    (assert (equals? p (rect-left-top (bounding-box shape)))))

  ;;; Return true if P falls inside SHAPE.
  (defgeneric (point-in-shape? (p <point>) (shape <shape>)))
  
  (defmethod (point-in-shape? (p <point>) (r <rect>))
    (and (<= (rect-left r) (point-x p))
         (<  (point-x p)   (rect-right r))
         (<= (rect-top r)  (point-y p))
         (<  (point-y p)   (rect-bottom r))))

  (defmethod (point-in-shape? (p <point>) (poly <polygon>))
    (call-prim 'PolygonContains poly p))
  
  ;;; Offset a shape or a point by the number of pixels specified by the
  ;;; point P.  This function is used internally in a number of places,
  ;;; and was exported so that supporting libraries could have access to
  ;;; the same logic.
  (define (offset-by-point s p)
    (cond
     [(point? s) (offset-point s p)]
     [(rect? s) (offset-rect s p)]
     [(polygon? s) (apply polygon (map (lambda (v) (offset-point v p))
                                       (polygon-vertices s)))]
     [else
      (error (cat "Don't know how to offset " s))]))

  ;;; Move the edges of R inwards by the specified number of pixels.
  (define (inset-rect r pixels)
    (rect (+ (rect-left r) pixels)
          (+ (rect-top r) pixels)
          (- (rect-right r) pixels)
          (- (rect-bottom r) pixels)))
  
  ;;; Return the origin of a shape.
  (defgeneric (shape-origin (shape <shape>)))

  (defmethod (shape-origin (shape <rect>))
    (rect-left-top shape))

  (defmethod (shape-origin (shape <polygon>))
    (rect-left-top (polygon-bounding-box shape)))

  ;;; Calculate the bounding rectangle of SHAPE.
  (defgeneric (bounding-box (shape <shape>)))

  (defmethod (bounding-box (shape <rect>))
    shape)
  
  (defmethod (bounding-box (shape <polygon>))
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

  ;;; Convert a rectangle to a polygon.
  ;;;
  ;;;   (as <polygon> (rect 10 10 100 100))
  (defmethod (as (type (singleton <polygon>)) (r <rect>))
    (polygon (point (rect-left r) (rect-top r)) 
             (point (rect-right r) (rect-top r))
             (point (rect-right r) (rect-bottom r))
             (point (rect-left r) (rect-bottom r)))))