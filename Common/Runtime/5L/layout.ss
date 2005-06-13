(module layout (lib "lispish.ss" "5L")
  (require (lib "types.ss" "5L"))

  (provide <layout> layout? layout-hspace layout-vspace layout-box-shape
           layout-width-used layout-height-used layout-shape-used
           add-box! next-column!)
  
  ;;; A class that can preform simple layout of rectangles, with optional
  ;;; spacing between.
  (defclass <layout> ()
    [hspace :initvalue 0]
    [vspace :initvalue 0]
    ;; TODO Test LAYOUT-BOX-WIDTH when this is #f.
    [box-shape :initvalue #f]
    [next-box-at :initializer (lambda () (point 0 0))]
    [width-used :initvalue 0]
    [height-used :initvalue 0])

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

  (define (require-width! layout width)
    (set! (layout-width-used layout)
          (max (layout-width-used layout) width)))

  (define (require-height! layout height)
    (set! (layout-height-used layout)
          (max (layout-height-used layout) height)))    

  ;;; Add a new box to the layout.
  (define (add-box! layout
                    &key
                    [width (layout-box-width layout)]
                    [height (layout-box-height layout)]
                    [shape (rect 0 0 width height)])

    (define box (rect-offset shape (layout-next-box-at layout)))

    ;; Update our width & height.
    (require-width! layout (rect-right box))
    (require-height! layout (rect-bottom box))

    ;; Update the location of our next box.
    (add-vspace! layout (rect-height box))
    (add-vspace! layout)

    ;; Return our box.
    box)
    
  (define (next-column! layout)
    (set! (layout-next-box-at layout)
          (point (layout-width-used layout) 0))
    (add-hspace! layout))

  )
