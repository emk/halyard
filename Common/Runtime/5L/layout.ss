(module layout (lib "lispish.ss" "5L")
  (require (lib "types.ss" "5L"))

  (provide <layout> layout? layout-hspace layout-vspace layout-box-shape
           layout-width-used layout-height-used layout-shape-used
           add-box! next-box-at! current-box-shape next-column!)
  
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
    [current-box :initvalue #f])

  ;;; The shape currently used by all boxes in this layout.
  (define (layout-shape-used layout)
    (rect 0 0 (layout-width-used layout) (layout-height-used layout)))

  (define (layout-box-width layout)
    (rect-width (layout-box-shape layout)))

  (define (layout-box-height layout)
    (rect-height (layout-box-shape layout)))

  (define (add-vspace! layout &opt [vspace (layout-vspace layout)])
    (inc! (point-y (layout-next-box-at layout)) vspace))

  (define (add-hspace! layout &opt [hspace (layout-hspace layout)])
    (inc! (point-x (layout-next-box-at layout)) hspace))

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

    (define box (rect-offset shape (layout-next-box-at layout)))
    (mark-point-as-used! layout (rect-right-bottom box))

    ;; Update the location of our next box.
    (add-vspace! layout (rect-height box))
    (add-vspace! layout)

    ;; Return our box.
    (set! (layout-current-box layout) box)
    box)

  ;;; Add a new box to the layout, and return the upper-left corner.
  ;;; You'll typically follow this up with a call to CURRENT-BOX-SHAPE.
  ;;;
  ;;; @see current-box-shape
  (define (next-box-at! layout &key width height shape &rest keys)
    (rect-left-top (apply add-box! layout keys)))
  
  ;;; Get the shape of the box most recently added to the layout.
  ;;;
  ;;; @see next-box-at!
  (define (current-box-shape layout)
    (define r (layout-current-box layout))
    (rect 0 0 (rect-width r) (rect-height r)))

  ;;; Start adding boxes to the next column.
  (define (next-column! layout)
    (set! (layout-next-box-at layout)
          (point (layout-width-used layout) 0))
    (add-hspace! layout))

  )
