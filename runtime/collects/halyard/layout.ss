;; @BEGIN_LICENSE
;;
;; Halyard - Multimedia authoring and playback system
;; Copyright 1993-2008 Trustees of Dartmouth College
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

(module layout (lib "mizzen.ss" "mizzen")
  (require (lib "types.ss" "halyard"))
  (require (lib "shapes.ss" "halyard"))

  (provide <layout> layout? layout-hspace layout-vspace layout-box-shape
           layout-next-box-at
           layout-width-used layout-height-used layout-shape-used
           add-box! layout-next-box-at! layout-current-box
           layout-current-box-shape next-column!
           layout-nth-box layout-nth-box-at layout-nth-box-shape
           layout-box-count)
  
  ;;; A class that can preform simple layout of rectangles, with optional
  ;;; spacing between.
  (defclass <layout> ()
    [hspace :initvalue 0]
    [vspace :initvalue 0]
    ;; TODO Test LAYOUT-BOX-WIDTH when this is #f.
    [box-shape :initvalue #f]
    [next-box-at :initializer (lambda () (point 0 0))]
    [width-used :initvalue 0]
    [height-used :initvalue 0]
    [boxes :initvalue '()]
    [current-box :initvalue #f])

  ;;; The shape currently used by all boxes in this layout.
  (define (layout-shape-used layout)
    (rect 0 0 (layout-width-used layout) (layout-height-used layout)))

  (define (layout-box-width layout)
    (if (layout-box-shape layout)
      (rect-width (layout-box-shape layout))
      #f))

  (define (layout-box-height layout)
    (if (layout-box-shape layout)
      (rect-height (layout-box-shape layout))
      #f))

  (define (add-vspace! layout &opt [vspace (layout-vspace layout)])
    (inc! (point-y (layout-next-box-at layout)) vspace))

  (define (add-hspace! layout &opt [hspace (layout-hspace layout)])
    (inc! (point-x (layout-next-box-at layout)) hspace))

  (define (box-shape r)
    (rect 0 0 (rect-width r) (rect-height r)))

  (define (mark-point-as-used! layout p)
    (set! (layout-width-used layout)
          (max (layout-width-used layout) (point-x p)))
    (set! (layout-height-used layout)
          (max (layout-height-used layout) (point-y p))))

  ;;; Add a new box to the layout.
  (define (add-box! layout
                    &key
                    [width (layout-box-width layout)]
                    [height (layout-box-height layout)]
                    [shape (rect 0 0 width height)])

    (define box (offset-rect shape (layout-next-box-at layout)))
    (mark-point-as-used! layout (rect-right-bottom box))

    ;; Update the location of our next box.
    (add-vspace! layout (rect-height box))
    (add-vspace! layout)

    ;; Return our box.
    (set! (layout-boxes layout)
          (append! (layout-boxes layout) (list box)))
    (set! (layout-current-box layout) box)
    box)

  ;;; Add a new box to the layout, and return the upper-left corner.
  ;;; You'll typically follow this up with a call to CURRENT-BOX-SHAPE.
  ;;;
  ;;; XXX Very bad name--fix this.
  ;;;
  ;;; @see layout-current-box-shape
  (define (layout-next-box-at! layout &key width height shape &rest keys)
    (rect-left-top (apply add-box! layout keys)))
  
  ;;; Get the shape of the box most recently added to the layout.
  ;;;
  ;;; @see layout-next-box-at!
  (define (layout-current-box-shape layout)
    (box-shape (layout-current-box layout)))

  ;;; Start adding boxes to the next column.
  (define (next-column! layout)
    (set! (layout-next-box-at layout)
          (point (layout-width-used layout) 0))
    (add-hspace! layout))

  ;;; Get the nth box added to this layout, counting from 0.
  (define (layout-nth-box layout index)
    (nth (layout-boxes layout) index))

  ;;; Get the left-top corner of the nth box added to this layout, counting
  ;;; from 0.
  (define (layout-nth-box-at layout index)
    (rect-left-top (layout-nth-box layout index)))

  ;;; Get the shape of the nth box added to this layout, counting from 0.
  (define (layout-nth-box-shape layout index)
    (box-shape (layout-nth-box layout index)))

  ;;; Get the number of boxes added to this layout so far.
  (define (layout-box-count layout)
    (length (layout-boxes layout)))

  )
