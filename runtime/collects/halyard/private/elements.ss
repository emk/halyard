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
;;  Random Tamale Primitives
;;=========================================================================
;;  This is a collection of loosely documented and poorly-organized Tamale
;;  primitives, all subject to change at a moment's notice.

(module elements (lib "mizzen.ss" "mizzen")
  (require (lib "kernel.ss" "halyard/private"))
  (require (lib "api.ss" "halyard/private"))
  (require (lib "events.ss" "halyard/private"))

  (require (lib "after-updating.ss" "halyard/private"))
  (provide (all-from (lib "after-updating.ss" "halyard/private")))

  (require (lib "define-node-helper.ss" "halyard/private"))
  (provide (all-from (lib "define-node-helper.ss" "halyard/private")))
  

  ;;;======================================================================
  ;;;  File and Path Functions
  ;;;======================================================================

  (provide ensure-directory-exists)

  ;; XXX - This is not a well-designed function; it can only create
  ;; directories in a folder that's typically write-only (d'oh).  This
  ;; function is only useful for the updater (which only runs if it
  ;; has write privileges) and for developer tools.
  (define (ensure-directory-exists name)
    (define dir (build-path (current-directory) name))
    (when (not (directory-exists? dir))
      (make-directory dir))
    dir)


  ;;;======================================================================
  ;;;  Core Element Support
  ;;;======================================================================

  (provide local->card parent->card below to-the-right-of %element%
           %invisible-element% %custom-element% %box% box new-box
           %clickable-zone% clickable-zone new-clickable-zone delete-element
           delete-elements element-exists?  delete-element-if-exists)

  (define $black (color 0 0 0))
  (define $transparent (color 0 0 0 0))
  
  (define (transparent? c)
    (equals? c $transparent))
  
  ;;; Convert the co-ordinates of a point or shape X into card
  ;;; co-ordinates.  We do this do that elements attached to elements
  ;;; can interpret AT, RECT and similar parameters relative to their
  ;;; parent elements.
  (define (local->card node x)
    ;; TODO - Should we rename this function?
    (if (element? node)
        (local->card (node-parent node) (offset-by-point x (node .at)))
        x))

  (define (parent->card node x)
    (local->card (node-parent node) x))

  ;;; Return the point the specified number of PIXELS below ELEM.
  (define (below elem pixels)
    (define bounds (elem .bounding-box))
    (point (rect-left bounds) (+ pixels (rect-bottom bounds))))

  ;;; Return the point the specified number of PIXELS to the right of ELEM.
  (define (to-the-right-of elem pixels)
    (define bounds (elem .bounding-box))
    (point (+ pixels (rect-right bounds)) (rect-top bounds)))

  (define (update-element-position elem)
    ;; Don't make me public.
    (call-prim 'MoveElementTo (elem .full-name)
                  (parent->card elem (elem .at)))
    (foreach [e (node-elements elem)]
      (update-element-position e)))

  ;;; The abstract superclass of all elements.  The other half of this
  ;;; definition appears in nodes.ss.
  (with-instance %element%
    ;; XXX - Code for updating (.at) conflicts with code in %widget% and
    ;; elsewhere, which computes (.at) from (.rect).  See case 2353.
    (attr at :type <point> :label "Position" :writable? #t)
    (after-updating at
      (update-element-position self))

    (attr shown? #t :type <boolean> :label "Shown?" :writable? #t)
    (after-updating shown?
      (set! (element-shown? self) (.shown?)))

    ;;; Return the bounding box of this element, or #f if it has no
    ;;; bounding-box.
    (def (bounding-box)
      #f)

    (setup
      ;; TODO - This is technically too late to set this value, and we should
      ;; probably add a SHOWN? parameter to every object creation primitive.
      (set! (element-shown? self) (.shown?)))

    ;;; Raise this element above its siblings.
    (def (raise-to-top!)
      ;; TODO - Rearrange element order in Scheme, too?
      (call-prim 'RaiseToTop (.full-name))
      (define elems (node-elements self))
      (foreach [elem elems]
        (elem .raise-to-top!)))

    ;;; Center this element on its parent.
    (def (center-on-parent!)
      (define parent (node-parent self))
      (define parent-shape
        ;; TODO - There should be a SHAPE method on all (visible?) nodes.
        (if (element? parent)
            (parent .shape)
            $screen-rect))
      (define desired-shape (center-shape-on (.shape) parent-shape))
      (set! (.at) (rect-left-top (bounding-box desired-shape))))
    )

  ;;; The abstract superclass of all elements which have no on-screen
  ;;; representation.
  (define-class %invisible-element% (%element%)
    (value at (point 0 0)) 
    (value shown? #f)
    (default %has-engine-element? #f))

  ;;; The superclass of all native GUI elements which can be displayed
  ;;; on the stage.
  (define-class %widget% (%element%)
    ;; XXX - It's possible for users to call (set! (widget .at) ...), but
    ;; doing so will fail to upate the rect.  See case 2353.
    (attr rect :type <rect> :label "Rectangle")
    (value at (rect-left-top (.rect)))

    ;;; Return the bounding rectangle for this element.
    (def (bounding-box)
      (.rect))

    ;;; Set the keyboard focus to this element.
    (def (focus)
      (call-prim 'Focus (.full-name))))

  ;; Let the engine know whether we want a cursor.  Note that the engine
  ;; knows nothing about 'auto, so we need to map it to #f manually.
  (define (set-wants-cursor! elem value)
    (call-prim 'WantsCursorSet (elem .full-name)
                  (case value
                    [[#f auto] #f]
                    [[#t] #t])))

  ;; Let the engine know whether we're currently dragging this object.
  (define (set-in-drag-layer?! elem value)
    (call-prim 'ElementSetInDragLayer (elem .full-name) value))
  
  ;; Does S posses either negative width or negative height?
  (define (negative-shape? s)
    (define r (bounding-box s))
    (or (> 0 (rect-height r))
        (> 0 (rect-width r))))

  ;;; A %custom-element% is a lightweight element (i.e., implemented by the
  ;;; engine, not by the OS), optionally with an associated drawing
  ;;; overlay.
  ;;;
  ;;; Normally, %custom-element% is created by specifying :at and :shape.
  ;;; In this case (shape-origin shape) must return (point 0 0).  For
  ;;; example:
  ;;;
  ;;;   :at (point 10 10) :shape (rect 0 0 100 100)
  ;;;
  ;;; ...or, more colloquially:
  ;;;
  ;;;   :at (point 10 10) :shape (shape 100 100)
  ;;;
  ;;; If you want to use a shape with non-zero origin, you can instead
  ;;; write:
  ;;;
  ;;;   :bounds (rect 10 10 110 110)
  (define-class %custom-element% (%element%)

    (with-instance (.class)
      ;;; Ignore the presence of initializer keyword ":name".  This is so
      ;;; other attrs can be initialized from this keyword, without ever
      ;;; needing to have a corresponding slot[1].  Note that this could be
      ;;; refactored into ruby-objects.ss if anybody else needs to use it.
      ;;;
      ;;; [1] See %initializer-keywords% in ruby-objects.ss for details of
      ;;; the strange phantom objects we use during initialization.
      (def (ignore-initializer name)
        (define setter-name (symcat "set-" name "!"))
        (.define-method setter-name
          (method (value)
            (if (.initialized?)
                (error (cat self " ." setter-name " may only be called "
                            "during initialization"))
                ;; Just ignore any initializers for this attr.
                (void)))))
      )

    ;; Support code for at/shape/bounds initializers.  We use .bounds to
    ;; provide default values for .at and .shape, and then we just throw it
    ;; away without actually storing it anywhere.
    (default at (shape-origin (.bounds)))
    (attr shape
          (offset-by-point (.bounds) (elem-map - (shape-origin (.bounds))))
          :type <shape> :label "Shape" :writable? #t)
    (.ignore-initializer 'bounds)
    (def (bounds)
      (offset-by-point (.shape) (.at)))

    ;; Change our default :wants-cursor? value from #f to the magic 'auto,
    ;; which is used in conjunction with some .define-method magic in
    ;; events.ss to set .wants-cursor? to #t whenever a mouse handler is
    ;; defined by one of our subclasses.
    (default wants-cursor? 'auto)

    (attr cursor     'hand :type <symbol>  :label "Cursor" :writable? #t)
    (attr overlay?   #t    :type <boolean> :label "Has overlay?")
    (attr alpha?     #f    :type <boolean> :label "Overlay transparent?")
    (attr dragging?  #f    :type <boolean> :label "In drag layer?" 
                           :writable? #t)
    (attr clickable-where-transparent? #f
          :type <boolean> :label "Clickable where transparent?")

    (after-updating cursor
      (set-element-cursor! self (.cursor)))
    (after-updating wants-cursor?
      (set-wants-cursor! self (.wants-cursor?)))
    (after-updating dragging?
      (set-in-drag-layer?! self (.dragging?)))

    (advise before (set-shape! value)
      (define (err reason)
        (error (cat self " .set-shape! " value ": " reason)))
      (cond
       [(not (shape? value))
        (err "not actually a shape")]
       [(negative-shape? value)
        (err "has negative size")]
       [(not (equals? (point 0 0) (shape-origin value)))
        (err "has non-zero origin")]))
    (after-updating shape
      (if (.overlay?)
        (call-prim 'OverlaySetShape (.full-name) (.shape))
        (call-prim 'ZoneSetShape (.full-name)
                      (offset-by-point (as <polygon> (.shape)) (.at))))
      (.invalidate))

    (def (initialize &rest keys)
      (super)
      
      ;; All overlays are erased by the engine at creation time, so we can
      ;; skip the first call to ERASE-BACKGROUND.  Yes, this is a
      ;; performance hack.
      (set! (slot 'erased-by-engine?) #t)

      ;;; Make sure that this element has a non-negative initial shape.
      ;;; TODO - Move into set-shape!.
      (when (negative-shape? (.shape))
        (let [[original-shape (.shape)]]
          (set! (.shape) (rect 0 0 0 0))
          (error (cat "%custom-element%: " (.full-name)
                      " may not have a negative-sized shape: " 
                      original-shape "."))))
      )
    
    (def (create-engine-node)
      (if (.overlay?)
        (call-prim 'Overlay (.full-name)
                      (parent->card self (.bounds))
                      (make-node-event-dispatcher self) (.cursor) (.alpha?)
                      (.clickable-where-transparent?))
        (call-prim 'Zone (.full-name)
                      (parent->card self (as <polygon> (.bounds)))
                      (make-node-event-dispatcher self) (.cursor))))

    (setup
      ;; We need to postpone this until the underlying engine object
      ;; is created.
      (set-wants-cursor! self (.wants-cursor?))
      (set-in-drag-layer?! self (.dragging?))
      (.invalidate))

    (def (bounding-box)
      (bounding-box (.bounds)))

    ;;; Invalidate the contents of this %custom-element% (perhaps because
    ;;; of a property change).  Makes sure that DRAW gets called before the
    ;;; next screen update.
    (def (invalidate)
      ;; XXX - See bug #4779.  We have a problem with MOUSE-ENTER and
      ;; MOUSE-LEAVE being sent at non-idle times, which triggers
      ;; inappropriate screen redraws.  This is especially problematic when
      ;; an element is currently being deleted.  The NODE-STATE check below
      ;; is an ugly workaround--it prevents most well-written
      ;; mouse-enter/mouse-leave handlers from actually drawing anything.
      ;; This is (hopefully) a low-impact workaround on the stable branch.
      (when (and (.overlay?) (memq (.node-state) '(ENTERING ACTIVE)))
        (with-dc self
          (.erase-background)
          (.draw))))
    
    ;;; Erases the background of an overlay in preperation for a redraw.
    ;;; If you know that your DRAW handler overwrites the entire overlay,
    ;;; you can override this function and have it do nothing.
    (def (erase-background)
      (unless (slot 'erased-by-engine?)
        (clear-dc (if (.alpha?) $transparent $black)))
      (set! (slot 'erased-by-engine?) #f))

    ;;; Draw this element, based on current property values.  Does not
    ;;; actually update the user-visible screen until the next REFRESH or
    ;;; blocking function.
    (def (draw)
      (void))
        
    ;;; Resize this element to fit exactly all of its childrent, plus
    ;;; PADDING pixels on the edge.  If PADDING is non-zero, then
    ;;; offset all children by PADDING on both axes.
    (def (fit-to-children! &opt (padding 0))
      (define max-x 0)
      (define max-y 0)
      (foreach [child (node-elements self)]
        (define r (child .bounding-box))
        (set! max-x (max max-x (rect-right r)))
        (set! max-y (max max-y (rect-bottom r)))
        (unless (= padding 0)
          (set! (child .at) (elem-map (fn (x) (+ x padding)) 
                                          (child .at)))))
      (set! (.shape) (rect 0 0 
                           (+ (* 2 padding) max-x) 
                           (+ (* 2 padding) max-y))))
    )
  
  ;;; A box is a generic, invisible container element.  It exists only
  ;;; to be the parent of other elements.
  (define-class %box% (%custom-element%)
    (value overlay? #f))

  ;;; Declare a %box% element.
  (define-node-helper box (bounds) %box%)

  ;;; Create a %box% element.
  (define (new-box bounds &key name 
                   (parent (default-element-parent))
                   (shown? #t))
    (%box% .new :name name :parent parent :shown? shown? :bounds bounds))

  ;; Mix in the necessary code to support a .click method with :command and
  ;; :action parameters.  This is used by things which work more or less
  ;; like buttons.  If you need this function to be public, please ask.
  (define (mix-in-standard-click-method! klass)
    (with-instance klass
      (attr command  #f :label "Command symbol" :writable? #t)
      (attr action   #f :label "On click" :writable? #t)
      ;;; Override this method to specify what happens when you click.
      (def (click)
        (cond
         [(and (.command) (.action))
          (error (cat self " has both a :command and an :action!"))]
         [(.command)
          (.propagate (.command))]
         [(.action)
          ((.action))]
         [else
          (error (cat "No :command, :action or DEF (CLICK) on " self))]))))

  ;;; A %clickable-zone% will run the specified ACTION when the user clicks on
  ;;; it.
  (define-class %clickable-zone% (%custom-element%)
    (mix-in-standard-click-method! self)
    (default overlay? #f)

    (def (mouse-down event)
      (.click)))

  ;;; Declare a %clickable-zone% element.
  (define-node-helper clickable-zone (bounds action) %clickable-zone%)

  ;;; Create a %clickable-zone%.
  (define (new-clickable-zone bounds action
                              &key name (cursor 'hand)
                              (overlay? #f) (alpha? #f)
                              (parent (default-element-parent))
                              (shown? #t))
    (%clickable-zone% .new
      :name name
      :parent parent
      :shown? shown?
      :bounds bounds
      :cursor cursor
      :action action
      :overlay? overlay?
      :alpha? alpha?))
  
  (define (element-exists-in-engine? elem)
    (call-prim 'ElementExists (elem .full-name)))

  (define (set-element-shown?! elem show?)
    ;; Not all subclasses of %element% actually have a corresponding
    ;; engine object.
    (when (element-exists-in-engine? elem)
      (call-prim 'ElementSetShown (elem .full-name) show?)))

  ;;; Delete the specified elements.
  (define (delete-elements
           &opt (elems-or-names (node-elements (current-card))))
    (foreach [elem elems-or-names]
      (delete-element elem)))

  ;;; Return true if and only if the specified element exists.  The keyword
  ;;; argument :PARENT can be used to search for elements somewhere other
  ;;; than the current DEFAULT-ELEMENT-PARENT.
  (define (element-exists? name &key (parent (default-element-parent)))
    (and (memq name (map node-name (node-elements parent)))
         #t))

  ;;; If an element with the specified name and parent exists, delete it.
  ;;; Otherwise, do nothing.
  (define (delete-element-if-exists name
                                    &key (parent (default-element-parent)))
    (when (element-exists? name :parent parent)
      (delete-element (find-node (string->symbol (cat (parent .full-name)
                                                      "/" name)) #t))))


  ;;;======================================================================
  ;;;  Drawing Functions
  ;;;======================================================================
  ;;;  Most of these functions have a corresponding element, below.

  (provide draw-graphic measure-graphic measure-graphics mask
           set-image-cache-size! with-dc
           dc-rect color-at clear-dc
           draw-line draw-rectangle draw-rectangle-outline
           draw-oval draw-oval-outline)

  ;;; Draw a graphic loaded from PATH at point P in the current DC.  You
  ;;; may optionally specify a sub-rectangle of the graphic to draw.
  (define (draw-graphic p path
                        &key [subrect :rect #f] [scale-x 1.0] [scale-y 1.0])
    (let [[native (resolve-content-path "graphics" path)]]
      (if subrect
          (call-prim 'LoadSubPic native p scale-x scale-y subrect)
          (call-prim 'LoadPic native p scale-x scale-y))))
  
  ;;; Return a rectangle located at 0,0 large enough to hold the graphic
  ;;; specified by NAME.
  (define (measure-graphic path &key [scale-x 1.0] [scale-y 1.0])
    (let [[native (resolve-content-path "graphics" path)]]
      (call-prim 'MeasurePic native scale-x scale-y)))

  ;;; Measure a list of graphics, returning a single bounding box large
  ;;; enough to contain any of the graphics.
  (define (measure-graphics graphics)
    (define max-width 0)
    (define max-height 0)
    (foreach [graphic graphics]
      (define bounds (measure-graphic graphic))
      (when (> (rect-width bounds) max-width)
        (set! max-width (rect-width bounds)))
      (when (> (rect-height bounds) max-height)
        (set! max-height (rect-height bounds))))
    (rect 0 0 max-width max-height))
  
  ;;; Destructively apply a mask to a DC with an alpha channel.  Areas of
  ;;; the DC corresponding to completely opaque areas of the mask will be
  ;;; left alone (as will areas outside the bounds of the mask).  Areas of
  ;;; the DC corresponding to completely transparent areas of the mask will
  ;;; be erased.  The color of the mask is ignored, but for forwards
  ;;; compatibility, please set it to white.
  ;;;
  ;;; (Note that a mask is basically an eraser with the alpha
  ;;; channel inverted.  But doing this way around allows more efficient
  ;;; implementations under a wide variety of graphics APIs, including
  ;;; Windows and Cairo.)
  (define (mask p path)
    (let [[native (resolve-content-path "graphics" path)]]
      (call-prim 'Mask native p))) 

  ;;; Specify how many bytes may be used by the engine to cache
  ;;; recently-used images.
  (define (set-image-cache-size! bytes)
    (call-prim 'SetImageCacheSize bytes))

  ;;; Perform a set of drawing calls in the specified overlay.
  ;;;
  ;;; @syntax (with-dc dc body ...)
  ;;; @param NODE dc The overlay to draw into.
  ;;; @param BODY body The drawing code.
  (define-syntax with-dc
    (syntax-rules ()
      [(with-dc dc body ...)
       (dynamic-wind
           (lambda () (call-prim 'DcPush (dc .full-name)))
           (lambda () (begin/var body ...))
           (lambda () (call-prim 'DcPop (dc .full-name))))]))
  (define-syntax-indent with-dc 1)

  ;;; Get the bounding rectangle of the current DC.
  (define (dc-rect)
    (call-prim 'DcRect))

  ;;; Get the color at the specified point.
  (define (color-at p)
    (call-prim 'ColorAt p))

  ;;; Clear the current DC to color C.
  (define (clear-dc c)
    (call-prim 'Screen c))

  ;;; Draw a line between two points using the specified color and width.
  (define (draw-line from to c width)
    (call-prim 'DrawLine from to c width))

  ;;; Draw a filled rectangle in the specified color.
  (define (draw-rectangle r c)
    (call-prim 'DrawBoxFill r c))

  ;;; Draw the outline of a rectangle, with the specified color and line width.
  (define (draw-rectangle-outline r c width)
    (call-prim 'DrawBoxOutline r c width))

  ;;; Draw a filled oval in the specified color.
  (define (draw-oval r c)
    (call-prim 'DrawOvalFill r c))

  ;;; Draw the outline of an oval, with the specified color and line width.
  (define (draw-oval-outline r c width)
    (call-prim 'DrawOvalOutline r c width))

  
  ;;;======================================================================
  ;;;  Graphic Elements
  ;;;======================================================================
  ;;;  These templates each correspond to a drawing function, and should
  ;;;  generally produce visually identical output.
  ;;;
  ;;;  These templates are expected to get more complex.  In particular,
  ;;;  most of them will get keyword arguments to support accessibility,
  ;;;  and most of their properties will become settable.

  (provide %text-box% text-box new-text-box %text% text new-text
           %graphic% graphic new-graphic
           %rectangle% rectangle new-rectangle
           %rectangle-outline% rectangle-outline new-rectangle-outline)
  
  ;;; A static text element with a specified bounding rectangle.
  (define-class %text-box% (%custom-element%)
    (attr style :label "Style" :writable? #t)
    (attr text :type <string> :label "Text" :writable? #t)
    (default alpha? #t)

    ;; On the off chance that you _can_ use the mouse to interact with text
    ;; (you generally can't), nobody wants to try to hit the actual pixels
    ;; of the letters, so default to making transparent areas clickable.
    (default clickable-where-transparent? #t)
 
    (after-updating [style text]
      (.invalidate))

    (def (draw)
      (draw-text (dc-rect) (.style) (.text))))
  
  ;;; Declare a %text-box% element.
  (define-node-helper text-box (bounds style text) %text-box%)

  ;;; Create a new %text-box% element.
  (define (new-text-box bounds style text
                        &key name (parent (default-element-parent))
                        (shown? #t))
    (%text-box% .new :name name :parent parent :shown? shown?
                     :bounds bounds :style style :text text))
  
  ;;; A text element just large enough to fit the specified text.
  (define-class %text% (%text-box%)
    ;; TODO - The default max-width is rather silly.
    (attr max-width (rect-width $screen-rect) :label "Max width"
          :writable? #t)

    ;; TODO - Wouldn't it be nice to handle property dependencies
    ;; automatically?  See case 2353.
    (value shape (measure-text (.style) (.text) :max-width (.max-width)))
    (after-updating [style text max-width]
      (set! (.shape) (measure-text (.style) (.text) :max-width (.max-width))))

    ;; XXX - Workaround for case 2977.  When updating (.text) from "" to a
    ;; non-empty string, we frequently get an error: "Trying to break line
    ;; in the middle of a character".  This is because we've updated our
    ;; .text *before* our .shape, and there's not enough space to draw even
    ;; a single letter.
    ;;
    ;; The correct fix: Split .invalidate and .draw into two separate
    ;; phases, and update the drawing architecture to call draw only at
    ;; well-defined times, not during property updates.
    ;;
    ;; This is non-trivial, so we work around it by a quick-and-dirty hack:
    ;; We check to see whether the width of our .shape has been fully
    ;; updated yet.
    (def (draw)
      (when (= (rect-width (.shape))
               (rect-width (measure-text (.style) (.text)
                                         :max-width (.max-width))))
        (super)))
    )
  
  ;;; Declare a %text% element.
  (define-node-helper text (at style text) %text%)

  ;;; Create a new %text% element.
  (define (new-text p style text
                    &key name (parent (default-element-parent))
                    (shown? #t) (max-width (rect-width $screen-rect)))
    (%text% .new :name name :parent parent :shown? shown?
                 :at p :max-width max-width :style style :text text))
  
  
  ;;; A simple graphic.  For now, you must specify the :ALPHA? value you
  ;;; want; the engine can't compute a reasonable value automatically.
  (define-class %graphic% (%custom-element%)
    (attr path :type <string> :label "Path" :writable? #t)
    (attr scale 1.0 :type <number> :label "Scale" :writable? #t)

    (attr scale-x (.scale) :type <number> :label "Scale X" :writable? #t)
    (attr scale-y (.scale) :type <number> :label "Scale Y" :writable? #t)
    (after-updating scale
      (set! (.scale-x) (.scale))
      (set! (.scale-y) (.scale)))
    
    (value shape (measure-graphic (.path)
                                  :scale-x (.scale-x) :scale-y (.scale-y)))

    (after-updating [path scale-x scale-y]
      (set! (.shape) (measure-graphic (.path)
                                      :scale-x (.scale-x) :scale-y (.scale-y)))
      (.invalidate))

    ;; TODO - Optimize erase-background now that we can update the graphic.
    (def (draw)
      (draw-graphic (point 0 0) (.path)
                    :scale-x (.scale-x) :scale-y (.scale-y))))
  
  ;;; Declare a %graphic% element.
  (define-node-helper graphic (at path) %graphic%)

  ;;; Create a new %graphic%.
  (define (new-graphic p path
                       &key name (alpha? #f)
                       (parent (default-element-parent)) (shown? #t))
    (%graphic% .new :name name :parent parent :shown? shown?
                    :at p :alpha? alpha? :path path))
  
  ;;; A rectangular element, filled with a single color.
  (define-class %rectangle% (%custom-element%)
    (attr color         $transparent :type <color>   :label "Color"
                                     :writable? #t)
    (attr outline-width 1            :type <integer> :label "Outline width"
                                     :writable? #t)
    (attr outline-color $transparent :type <color>   :label "Outline color"
                                     :writable? #t)
    (default alpha? #t)

    (after-updating [color outline-width outline-color] 
      (.invalidate))

    ;; TODO - Optimize erase-background now that we can update our properties.
    (def (draw)
      (unless (transparent? (.color))
        (draw-rectangle (dc-rect) (.color)))
      (unless (transparent? (.outline-color))
        (draw-rectangle-outline (dc-rect) (.outline-color) (.outline-width)))))
  
  ;;; Declare a %rectangle% element.
  (define-node-helper rectangle (bounds color) %rectangle%)

  ;;; Create a new %rectangle%.
  (define (new-rectangle r c
                         &key name (parent (default-element-parent))
                         (shown? #t) (outline-width 1)
                         (outline-color $transparent))
    (%rectangle% .new :name name :parent parent :shown? shown? :bounds r 
                      :color c :outline-width outline-width 
                      :outline-color outline-color))
  
  (define-class %rectangle-outline% (%rectangle%)
    (value color $transparent))

  ;;; Declare a %rectangle-outline% element.
  (define-node-helper rectangle-outline (bounds outline-color outline-width) 
    %rectangle-outline%)

  ;;; Create a new %rectangle% with an outline and a transparent center.
  (define (new-rectangle-outline r c width
                                 &key name (shown? #t)
                                 (parent (default-element-parent)))
    (%rectangle-outline% .new :name name :parent parent :shown? shown? 
                              :bounds r :outline-width width 
                              :outline-color c))
  

  ;;;======================================================================
  ;;;  Using elements as cursors
  ;;;======================================================================

  (provide %cursor-element%)
  
  ;;; An overlay element which can be used as a cursor.  The NODE-NAME of
  ;;; this object will be used as the cursor's name.
  (define-class %cursor-element% (%custom-element%)
    (attr hotspot (point 0 0) :type <point> :label "Cursor hotspot")
    (default alpha? #t)

    ;; Most of these really do need to be hard-coded, and some of them
    ;; shouldn't even be settable.
    ;; TODO - Can we enforce a better policy here?
    (value overlay? #t)
    (value at (point 0 0))
    (value shown? #f)
    (value wants-cursor? #f)

    (def (create-engine-node)
      (call-prim 'CursorElement (.full-name)
                    (parent->card self
                                  (offset-rect (.shape) (.at)))
                    (make-node-event-dispatcher self)
                    (.alpha?) (node-name self)))
    
    ;;; Called when the cursor is moved.  This will be called once before
    ;;; CURSOR-SHOWN is called, and then repeatedly between the call the
    ;;; CURSOR-SHOWN and the matching call to CURSOR-HIDDEN.
    ;;;
    ;;; There is no need for this to call REFRESH, since it's called
    ;;; automatically by the engine whenever it is safe to do so.
    (def (cursor-moved event)
      (set! (.at)
            (point-difference (event .position) (.hotspot))))

    ;;; Called when the cursor is shown on the screen.
    (def (cursor-shown event)
      (set! (.shown?) #t)
      ;; TODO - Putting this cursor in the drag layer isn't quite
      ;; adequate.  Ideally, we'd add a whole new layer for cursors, above
      ;; the drag layer, but that will require adding more complexity in
      ;; the compositing routines.
      (set! (.dragging?) #t))

    ;;; Called when the cursor is hidden (but not when an active cursor is
    ;;; destroyed).
    (def (cursor-hidden event)
      (set! (.shown?) #f)
      (set! (.dragging?) #f))
    )


  ;;;======================================================================
  ;;;  Sprites
  ;;;======================================================================
  ;;;  This is a replacement for %animated-graphic%.

  (provide %sprite% sprite new-sprite)

  (define-class %sprite% (%custom-element%)
    (attr frames   :type <list>    :label "List of image files")
    (attr frame  0 :type <integer> :label "Index of current frame"
                   :writable? #t)

    (value shape (measure-graphics (.frames)))

    (after-updating frame
      (.invalidate))

    (def (draw)
      (draw-graphic (point 0 0) (list-ref (.frames) (.frame)))))

  (define-node-helper sprite (at frames) %sprite%)

  (define (new-sprite at frames 
                      &key name (alpha? #f)
                      (parent (default-element-parent)) (shown? #t))
    (%sprite% .new :at at :frames frames :name name :alpha? alpha? 
                   :parent parent :shown? shown?))


  ;;;======================================================================
  ;;;  Mouse and Cursor Support
  ;;;======================================================================

  (provide register-cursor hide-cursor-until-mouse-moved! mouse-position
           grab-mouse ungrab-mouse mouse-grabbed? mouse-grabbed-by?)

  (define (set-element-cursor! elem cursor)
    ;; Don't make me public.
    (call-prim 'SetZoneCursor (elem .full-name) cursor))

  ;;; Register the graphic in FILENAME with the engine as a cursor named
  ;;; SYM.  If the hotspot is not in the default path, it should be
  ;;; specified explicitly.
  (define (register-cursor sym filename &key (hotspot (point -1 -1)))
    (let [[native (resolve-content-path "cursors" filename)]]
      (call-prim 'RegisterCursor sym native hotspot)))

  ;;; Hide the cursor until the next time the mouse moves.  Only works in
  ;;; full-screen mode, and only if the underlying system allows it.
  (define (hide-cursor-until-mouse-moved!)
    (call-prim 'HideCursorUntilMouseMoved))

  ;;; Get the current position of the mouse.  This function is almost
  ;;; certainly the wrong function to use; see the mouse-moved events
  ;;; instead.
  (define (mouse-position)
    ;; XXX - This keeps returning exciting results even if we're in the
    ;; background.  Yuck.
    (call-prim 'MousePosition))

  ;;; Redirect all mouse events to ELEM until further notice.  For more
  ;;; information on how mouse grabbing works, consult the documentation
  ;;; for the wxWidgets GUI library (or any other GUI library--it tends to
  ;;; be very similar).
  ;;;
  ;;; @see ungrab-mouse mouse-grabbed? mouse-grabbed-by?
  (define (grab-mouse elem)
    (assert (element? elem))
    (call-prim 'MouseGrab (elem .full-name)))

  ;;; Ungrab the mouse, which must be currently grabbed by ELEM.
  ;;; @see grab-mouse
  (define (ungrab-mouse elem)
    (assert (element? elem))
    (call-prim 'MouseUngrab (elem .full-name)))

  ;;; Is the mouse currently grabbed?
  ;;; @see grab-mouse
  (define (mouse-grabbed?)
    (call-prim 'MouseIsGrabbed))
  
  ;;; Is the mouse currently grabbed by ELEM?
  ;;; @see grab-mouse
  (define (mouse-grabbed-by? elem)
    (call-prim 'MouseIsGrabbedBy (elem .full-name)))


  ;;;======================================================================
  ;;;  ActiveX and Flash
  ;;;======================================================================

  
  (provide %activex% %flash-card%)

  ;;; A native ActiveX element.  Consult the C++ source for documentation.
  (define-class %activex% (%widget%)
    (attr activex-id :type <string>)

    ;; TODO - We really ought to have some way to map ActiveX properties to
    ;; normal-looking getters and setters.

    ;;; Get a property of an ActiveX element.
    (def (activex-prop name)
      (call-prim 'ActiveXPropGet (.full-name) name))

    ;;; Set a property of an ActiveX element.
    (def (set-activex-prop! name value)
      (call-prim 'ActiveXPropSet (.full-name) name value))

    (def (create-engine-node)
      (call-prim 'ActiveX (.full-name) 
                    (make-node-event-dispatcher self)
                    (parent->card self (.rect))
                    (.activex-id))))

  ;;; Show a Macromedia Flash movie scaled to fit the current card.
  ;;; Requires that the user have an appropriate version of Flash
  ;;; installed.  Macromedia reserves the right to break this interface in
  ;;; future versions of Flash, so this is more for demo purposes than
  ;;; anything else.
  (define-class %flash-card% (%card%)
    (attr path :type <string> :label "Path")
    
    ;; TODO - Once we have local element declarations, we need to heavily
    ;; redesign this card.
    (run
      (define flash
        (%activex% .new :name 'flash :rect $screen-rect
                        :activex-id "ShockwaveFlash.ShockwaveFlash"))
      (flash .set-activex-prop! "movie"
             (build-path (current-directory) "Flash" (.path)))))
  

  ;;;======================================================================
  ;;;  Web Browser Support
  ;;;======================================================================

  (provide %browser% browser new-browser)

  ;;; A web browser element.
  (define-class %browser% (%widget%)
    (attr path :label "Path" :writable? #t) ;; string or PLT path
    ;;; WARNING: The fallback browser is not very good, and only
    ;;; tends to work under carefully controlled circumstances.  In
    ;;; particular, it tends to deal with errors by popping up ugly
    ;;; WX error dialogs at the user.  It's probably suitable for
    ;;; online help, but not much else.  Try to avoid using it if
    ;;; you have any choice.
    (attr fallback? #f :type <boolean> 
                       :label "Use primitive fallback web browser?")
    
    (after-updating path
      (define native (resolve-content-path "html" (.path)))
      (call-prim 'BrowserLoadPage (.full-name) native))

    ;;; Load the specified page in the web browser.  Can be pointed
    ;;; to either the local HTML folder or to a URL.
    (def (load-page page)
      (set! (.path) page))
    
    ;;; Return true if and only if COMMAND should be enabled.  Supported
    ;;; values are: BACK, FORWARD, RELOAD and STOP.
    (def (command-enabled? command)
      (case command
        [[back]
         (call-prim 'BrowserCanBack (.full-name))]
        [[forward]
         (call-prim 'BrowserCanForward (.full-name))]
        [[reload]
         (call-prim 'BrowserCanReload (.full-name))]
        [[stop]
         (call-prim 'BrowserCanStop (.full-name))]
        [else
         (super)]))
    (.always-propagate 'command-enabled?)

    ;;; Go back to the previous page.
    (def (back)
      (call-prim 'BrowserBack (.full-name)))
    ;;; Go forward.
    (def (forward)
      (call-prim 'BrowserForward (.full-name)))
    ;;; Reload the currently displayed page.
    (def (reload)
      (call-prim 'BrowserReload (.full-name)))
    ;;; Stop loading the currently displayed page.
    (def (stop)
      (call-prim 'BrowserStop (.full-name)))

    (def (create-engine-node)
      (call-prim 'Browser (.full-name) 
                    (make-node-event-dispatcher self)
                    (parent->card self (.rect))
                    (.fallback?))
      (.load-page (.path)))

    (def (setup-finished)
      (super)
      ;; We don't update the UI until very late in the processing, giving
      ;; everybody's SETUP a chance to complete.
      (define (update-command command)
        (.update-ui (%update-ui-event% .new :command command)))
      (update-command 'back)
      (update-command 'forward)
      (update-command 'reload)
      (update-command 'stop))

    )

  ;;; Declare a %browser% object.
  (define-node-helper browser (rect path) %browser%)

  ;;; Create a new %browser% object.
  (define (new-browser r path
                       &key name (parent (default-element-parent))
                       (shown? #t))
    (%browser% .new :name name :parent parent :shown? shown?
                    :rect r :path path))


  ;;;======================================================================
  ;;;  Text Editing
  ;;;======================================================================

  (provide %edit-box% edit-box new-edit-box)

  ;;; A native GUI edit box.
  (define-class %edit-box% (%widget%)
    (attr font-size 9 :type <integer> :label "Font size")
    (attr multiline? #f :type <boolean> :label "Allow multiple lines?")
    (attr send-enter-event? #t :type <boolean>)
      
    ;; TODO - We need some way to declare the :label field that corresponds
    ;; to this "virtual attr".
    (attr text "" :type <string> :getter? #f :setter? #f)

    ;;; Return the text from this edit box.
    (def (text)
      (if (.initialized?)
        (call-prim 'EditBoxGetValue (.full-name))
        (slot 'text)))

    ;;; Set the text in this edit box.
    (def (set-text! value)
      (check-setter-type self 'text <string> value)
      (if (.initialized?)
        (call-prim 'EditBoxSetValue (.full-name) value)
        (set! (slot 'text) value)))
    
    (def (char event)
      (define pressed (event .modifiers-and-character))
      (cond
        [(and (equal? '(#\tab) pressed)
              (.maybe-navigate-controls #t))
         (void)]
        [(and (equal? '(shift #\tab) pressed)
              (.maybe-navigate-controls #f))
         (void)]
        [(.should-propagate-char-event? event)
         ;; We've been told to propagate this character event, so do the usual
         ;; thing here.
         (super)]
        [else
         ;; Let wxWidgets decide what to do with this character event.        
         (event .mark-as-not-handled!)]))
    
    ;;; Do we want to propagate this character event to the card, sequence,
    ;;; etc., for further processing (say, a Control-I command that jumps to
    ;;; an index card), or do we want to let it be typed into the text field
    ;;; _even if_ it would otherwise be interpreted as an accelerator?
    ;;;
    ;;; TODO - This is probably not really the right API, and it exists mostly
    ;;; for making our current thought process clear.  Don't override this
    ;;; without a compelling reason.
    (def (should-propagate-char-event? event)
      #f)
    (.seal-method! 'should-propagate-char-event?)
    
    ;;; This function is called whenever the user presses Tab or Shift-Tab.
    ;;; If the user pressed Tab, forward? will be #t, otherwise it will
    ;;; be #f.  Return true if this Tab character was handled; return #f
    ;;; to treat this like a normal tab character.
    (def (maybe-navigate-controls forward?)
      (define (do-nav maybe-control)
        (when maybe-control
          (maybe-control .focus))
        #t)
      (cond
        [(.multiline?) #f]
        [forward?      (do-nav (.next-control))]
        [else          (do-nav (.prev-control))]))
    
    ;;; Return the next control in tab order, if any.  It's probably desirable
    ;;; to override this method with something that can automatically compute
    ;;; a tab order if you have more than two or three controls.
    (def (next-control)
      #f)
    
    ;;; Return the previous control in tab order, if any.
    (def (prev-control)
      #f)
    
    ;;; Move the insertion point to the specificed location.  Indices start
    ;;; at 0, and an index of -1 specifies "after the last character".
    (def (set-insertion-point! index)
      (.focus)
      (call-prim 'EditBoxSetInsertionPoint (.full-name) index))
    ;;; Set the selection.  Indices are the same as SET-INSERTION-POINT!.
    (def (set-selection! start end)
      (.focus)
      (call-prim 'EditBoxSetSelection (.full-name) start end))

    (def (create-engine-node)
      (call-prim 'EditBox (.full-name)
                    (make-node-event-dispatcher self)
                    (parent->card self (.rect)) (slot 'text)
                    (.font-size) (.multiline?) (.send-enter-event?)))
    )

  ;;; Declare a %edit-box% object.
  (define-node-helper edit-box (rect text) %edit-box%)

  ;;; Create an %edit-box%.
  (define (new-edit-box r text
                        &key name (font-size 9)
                        (multiline? #f) (send-enter-event? #t)
                        (parent (default-element-parent)) (shown? #t))
    (%edit-box% .new :name name :parent parent :shown? shown? :rect r
                     :text text :font-size font-size :multiline? multiline?
                     :send-enter-event? send-enter-event?))

  
  ;;;======================================================================
  ;;;  Generic Media Support
  ;;;======================================================================

  (provide wait tc)

  ;; (Internal use only.)  Pause a media element.
  (define (media-pause elem)
    ;; Note: these functions may not be happy if the underlying movie
    ;; code doesn't like to be paused.
    (call-prim 'MoviePause (elem .full-name)))

  ;; (Internal use only.)  Resume a media element.
  (define (media-resume elem)
    (call-prim 'MovieResume (elem .full-name)))
  
  ;; (Internal use only.)  End playback of a media element. From the
  ;; perspective of the WAIT function, the media element will skip
  ;; immediately to the end of playback.
  (define (media-end-playback elem)
    (call-prim 'MovieEndPlayback (elem .full-name)))

  ;; (Internal use only.)  Set the volume of a media element.  Channels may
  ;; be LEFT, RIGHT, ALL, or something else depending on the exact type of
  ;; media being played.  Volume ranges from 0.0 to 1.0.
  (define (set-media-volume! elem channel volume)
    (call-prim 'MediaSetVolume (elem .full-name) channel volume))  
           
  ;; (Internal use only.)  If we can find an appropriate caption file, then
  ;; attach it to a media element.  We assume that captions live in
  ;; local/media, because we can't load them from over the network (which
  ;; we might need to do with things normally stored in streaming/media).
  ;; If this is a problem for you, you may want to encode your captions as
  ;; a track in the media itself.
  (define (media-maybe-attach-caption-file! elem path)
    (with-handlers [[exn:fail:content-not-found? (lambda (exn) (void))]]
      (let [[native (resolve-content-path "media" (cat path ".capt"))]]
        (unless (url? native)
          (call-prim 'MediaAttachCaptionFile (elem .full-name) native)))))

  (define (add-common-media-methods! klass)
    (with-instance klass
      ;;; Wait until the end of the media stream, or until FRAME (if
      ;;; specified).  Note that unlike the old, global WAIT function, this
      ;;; doesn't take a keyword argument--everybody was always leaving
      ;;; that keyword off.
      (def (wait &opt (frame #f))
        (define name (.full-name))
        (if frame
          (call-prim 'Wait name frame)
          (call-prim 'Wait name)))

      ;;; Pause playback.
      (def (pause)
        (media-pause self))
      
      ;;; Resume playback.
      (def (resume)
        (media-resume self))
      
      ;;; End playback. From the perspective of the WAIT function, this
      ;;; media element will skip immediately to the end of playback.
      (def (end-playback)
        (media-end-playback self))

      ;;; Set the volume of a media element.  Channels may be LEFT, RIGHT,
      ;;; ALL, or something else depending on the exact type of media being
      ;;; played.  Volume ranges from 0.0 to 1.0.
      (def (set-channel-volume! channel volume)
        ;; TODO - We should make (set! (.volume) n) map to a call to (set!
        ;; (.channel-volume 'all) n).
        (set-media-volume! self channel volume))

      ;;; When FRAME is reached, send an PLAYBACK-TIMER event.
      (def (set-playback-timer! frame)
        (call-prim 'MovieSetPlaybackTimer (.full-name) frame))
      
      ;;; Clear an exiting playback timer.
      (def (clear-playback-timer!)
        (call-prim 'MovieClearPlaybackTimer (.full-name)))
      ))

  ;;; The superclass of all audio-only elements.
  ;;; @see %movie%
  (define-class %audio-element% (%invisible-element%)
    (value %has-engine-element? #t)
    (attr volume 1.0 :type <number> :label "Volume (0.0 to 1.0)")
    (add-common-media-methods! self))

  ;;; Pause script execution until the end of the specified media element,
  ;;; or until a specific frame is reached.  This function mostly exists
  ;;; for backwards compatibility; new code is welcome to use .WAIT
  ;;; instead.
  (define (wait elem &key frame)
    (elem .wait frame))
  
  ;;; Convert an industry-standard timecode to frames.  The engine has a
  ;;; single, nominal frame-rate of 30 frames per second, regardless of
  ;;; the underlying media's frame rate.
  (define (tc arg1 &opt arg2 arg3)
    (cond
     [arg3 (+ (* (+ (* arg1 60) arg2) 30) arg3)]
     [arg2 (+ (* arg1 30) arg2)]
     [else arg1]))
  
  
  ;;;======================================================================
  ;;;  Audio Synthesis Elements
  ;;;======================================================================
  ;;;  These are very specialized, and aren't expected to be used much.
  
  (provide %geiger-audio% geiger-audio new-geiger-audio
           %geiger-synth% new-geiger-synth
           %sine-wave% sine-wave new-sine-wave)

  (define-class %geiger-audio% (%audio-element%)
    (attr path :type <string> :label "Path")
      
    (def (set-counts-per-second! counts)
      (call-prim 'AudioStreamGeigerSetCps (.full-name) counts))
    
    (def (create-engine-node)
      (call-prim 'AudioStreamGeiger (.full-name)
                    (make-node-event-dispatcher self)
                    (build-path (current-directory) "local" "media" (.path))
                    (.volume))))

  (define-node-helper geiger-audio (path) %geiger-audio%)

  (define (new-geiger-audio path &key name (volume 1.0)
                            (parent (default-element-parent)))
    (%geiger-audio% .new :name name :parent parent :path path :volume volume))

  (define-class %geiger-synth% (%audio-element%)
    ;;; Note that you'll need to (require (lib "state-db.ss" "halyard")) to
    ;;; be able to set the value for this state-path.
    (attr state-path) 
    (attr chirp) 
    (attr loops)
    
    (def (create-engine-node)
      (apply call-prim 'GeigerSynth (.full-name) (.state-path)
             (build-path (current-directory) "local" "media" (.chirp))
             (.volume)
             (* 512 1024)
             (map (fn (item)
                    (if (string? item)
                        (build-path (current-directory) "local" "media" item)
                        item))
                  (.loops)))))

  (define (new-geiger-synth state-path chirp loops
                            &key name
                            (parent (default-element-parent)))
    (%geiger-synth% .new :name name :parent parent :state-path state-path
                         :chirp chirp :loops loops))

  ;;; Plays a pure sine-wave tone.
  (define-class %sine-wave% (%audio-element%)
    (attr frequency :type <integer> :label "Frequency (Hz)")
      
    (def (create-engine-node)
      (call-prim 'AudioStreamSine (.full-name)
                    (make-node-event-dispatcher self)
                    (.volume) (.frequency))))

  (define-node-helper sine-wave (frequency) %sine-wave%)

  ;;; Create a %sine-wave%.
  (define (new-sine-wave frequency
                         &key name (parent (default-element-parent))
                         (volume 1.0))
    (%sine-wave% .new :name name :parent parent :frequency frequency 
                      :volume volume))


  ;;;======================================================================
  ;;;  Vorbis Audio
  ;;;======================================================================
  ;;;  Vorbis audio streams.  These are most useful for foley and for
  ;;;  background audio which should continue playing even if the engine is
  ;;;  otherwise occupied.
  
  (provide %vorbis-audio% vorbis-audio new-vorbis-audio)

  ;;; Plays an Ogg Vorbis audio stream.
  (define-class %vorbis-audio% (%audio-element%)
    (attr path       :type <string>  :label "Path")
    (attr buffer 512 :type <integer> :label "Buffer Size (K)")
    (attr loop?  #f  :type <boolean> :label "Loop this clip?")

    (def (create-engine-node)
      (let [[path (resolve-content-path "media" (.path))]]
        (call-prim 'AudioStreamVorbis (.full-name)
                      (make-node-event-dispatcher self) path
                      (.volume) (* 1024 (.buffer)) (.loop?))))

    (setup
      (media-maybe-attach-caption-file! self (.path))))
  
  (define-node-helper vorbis-audio (path) %vorbis-audio%)

  ;;; Create a %vorbis-audio% element.
  (define (new-vorbis-audio path
                            &key name
                            (parent (default-element-parent))
                            (loop? #f) (volume 1.0))
    (%vorbis-audio% .new :name name :parent parent :path path 
                         :loop? loop? :volume volume))
  

  ;;;======================================================================
  ;;;  Streaming Media Support
  ;;;======================================================================
  ;;;  For now, these functions generally only work with %MOVIE%.

  (provide media-is-installed? media-cd-is-available? search-for-media-cd
           set-media-base-url!)

  (define *cd-media-directory* #f)

  ;;; Return true if and only if we have a local media directory.
  (define (media-is-installed?)
    (directory-exists? (build-path (current-directory) "streaming" "media")))

  ;;; Return true if and only if we have media available on CD.
  (define (media-cd-is-available?)
    (and *cd-media-directory* #t))

  ;;; Try to determine whether or not we have a CD with our media files on
  ;;; it.  We look at each drive on the system, and see whether it contains
  ;;; a streaming/media directory with a file named 'pathname'.  You can
  ;;; use '/' as a path separator in pathname, as usual.
  (define (search-for-media-cd pathname)
    (label return
      (foreach [drive (filesystem-root-list)]
        (define candidate (build-path drive "streaming" "media"))
        (define file (build-path candidate pathname))
        (when (file-exists? file)
          (set! *cd-media-directory* candidate)
          (return)))))

  (define *media-base-url* #f)

  ;;; Set the base URL at which the program's media can be found for
  ;;; streaming.
  (define (set-media-base-url! url)
    (set! *media-base-url* url))

  ;;; Given an abstract media path, return a native path or a URL pointing
  ;;; to that particular file.
  (define (media-path path)
    ;; We run this function if the usual, non-media techniques for finding
    ;; content files all fail.
    ;; TODO - Refactor some of this code into resolve-content-path?
    (define (try-fallbacks exn)
      (define cd-path
        (if (media-cd-is-available?)
          (build-path *cd-media-directory* path)
          #f))
      (cond
       ;; Then check the CD, if we have one.
       [(and cd-path (file-exists? cd-path))
        cd-path]
       ;; If all else fails, and we've been told about a server, assume our
       ;; media is there.
       [*media-base-url*
        (cat *media-base-url* "/" path)]
       ;; OK, we give up.
       [#t
        (content-not-found-error "media" path)]))

    ;; We try, whenever possible to rely on resolve-content-path when
    ;; looking up media paths.
    (with-handlers [[exn:fail:content-not-found? try-fallbacks]]
      (resolve-content-path "media" path)))


  ;;;======================================================================
  ;;;  Movie Elements
  ;;;======================================================================

  (provide %movie% movie new-movie)

  ;;; A movie.
  (define-class %movie% (%widget%)
    (attr path                :type <string>  :label "Path")
    (attr volume 1.0          :type <number>  :label "Volume (0.0 to 1.0)")
    (attr controller? #f      :type <boolean> :label "Has controller")
    (attr audio-only? #f      :type <boolean> :label "Audio only")
    (attr loop?       #f      :type <boolean> :label "Loop movie")
    (attr interaction? #f     :type <boolean> :label "Allow interaction")
    (attr report-captions? #t :type <boolean> :label "Report captions?")

    (default shown? (or (not (.audio-only?)) (.controller?)))

    ;; Mix in pause, resume, and other common media-related methods.
    (add-common-media-methods! self)

    ;;; Report a problem with the movie, and ask the user what to do.
    (def (movie-problem title msg)
      (define result
        (native-dialog title msg
                       "&Skip Movie" "E&xit Program"))
      (if (= result 1)
          (.end-playback)
          (.user-exit-request)))

    ;;; Report a problem with the network, and ask the user what to do.
    (def (network-problem type)
      (.movie-problem (cat "Network Movie " type)
                      (cat type " loading movie.  You may want to order "
                           "a CD version\nof this program; see the "
                           "README.")))

    ;;; Called when the user asks to exit the script because of movie
    ;;; errors.  If you want fancier behavior, you'll need to override this
    ;;; handler.
    (def (user-exit-request)
      (exit-script))

    ;;; Called when a network timeout occurs for a streaming movie.  Override
    ;;; if you want different behavior.
    (def (media-network-timeout event)
      (.network-problem "Timeout"))

    ;;; Called when a network error occurs for a streaming movie.  Override
    ;;; if you want different behavior.
    (def (media-network-error event)
      (.network-problem "Error"))

    ;;; Called when a local error occurs for a movie.  Override if you want
    ;;; different behavior.
    (def (media-local-error event)
      (error (cat "Error playing movie (" (.path) ")")))

    ;;; Set the timeout for this movie, in seconds.  A timeout of 0
    ;;; turns off timeout handling.  (Timeouts are actually pretty
    ;;; sophisticated internally; this is the nominal length of time
    ;;; the user will be asked to wait without *something* happening.
    ;;; The controller bar turns off timeouts.)
    (def (set-timeout! seconds)
      (call-prim 'MovieSetTimeout (.full-name) seconds))

    (def (create-engine-node)
      (define path (media-path (.path)))
      (call-prim 'Movie (.full-name)
                    (make-node-event-dispatcher self)
                    (parent->card self (.rect))
                    path (.volume)
                    (.controller?) (.audio-only?) (.loop?) (.interaction?)
                    (.report-captions?)))

    (setup
      (media-maybe-attach-caption-file! self (.path)))
    )

  (define-node-helper movie (rect path) %movie%)

  ;;; Create a %movie%.
  (define (new-movie r path
                     &key name (volume 1.0)
                     controller? audio-only? loop? interaction?
                     (report-captions? #t) (parent (default-element-parent)))
    (%movie% .new :name name :parent parent :rect r :path path
                  :volume volume
                  :controller? controller? 
                  :audio-only? audio-only?
                  :loop? loop?
                  :interaction? interaction?
                  :report-captions? report-captions?))
  

  ;;;======================================================================
  ;;;  Miscellaneous
  ;;;======================================================================
  
  (provide string->xml native-dialog sleep-milliseconds nap
           copy-string-to-clipboard open-in-browser
           script-user-data-directory
           script-user-local-data-directory
           %basic-button%
           quicktime-component-version
           mark-unprocessed-events-as-stale!
           register-debug-report-file!)

  (define $amp-regexp (regexp "&"))
  (define $lt-regexp (regexp "<"))

  ;;; Escape any XML meta-characters in a string.  Use this if you want
  ;;; to have text be formatted unchanged.
  (define (string->xml str)
    (regexp-replace* $lt-regexp
                     (regexp-replace* $amp-regexp str "\\&amp;")
                     "\\&lt;"))

  ;;; Displays a native OS dialog, and returns the number of the button
  ;;; clicked. DO NOT USE FOR OK/CANCEL DIALOGS: The cancel button won't
  ;;; get the proper keybindings. Button 1 is the default button.
  (define (native-dialog title text
                         &opt [button1 ""] [button2 ""] [button3 ""])
    (call-prim 'Dialog title text button1 button2 button3))

  ;;; Delay for the specified number of milliseconds.  Calls IDLE, so
  ;;; events will be processed and the screen will be repainted.
  (define (sleep-milliseconds milliseconds)
    (define end-time (+ (adjust-delay milliseconds) (current-milliseconds)))
    (let repeat-delay []
      (idle)
      (when (> end-time (current-milliseconds))
        (repeat-delay))))    

  ;;; Sleep for the specified number of tenths of seconds.
  (define (nap tenths)
    (sleep-milliseconds (* 100 tenths)))

  ;;; Copy a string to the OS clipboard.  Used mostly by developer tools.
  (define (copy-string-to-clipboard string)
    (call-prim 'CopyStringToClipboard string))

  ;;; Open a URL in the user's default browser.  Returns true if the
  ;;; browser is launched successfully, or false if something obvious goes
  ;;; wrong.  Note that this will minimize the currently running program
  ;;; (if we're in full-screen mode), and that many users may have trouble
  ;;; figuring out to get back once they're done browsing.
  (define (open-in-browser url)
    (call-prim 'OpenInBrowser url))

  ;;; Returns a path to the directory which should be used to store any
  ;;; user-specific script data files.  Under Windows, this directory may
  ;;; actually get copied between login sessions on different machines, so
  ;;; it would be rude to store huge files here.  See
  ;;; SCRIPT-USER-LOCAL-DATA-DIRECTORY instead for big files.
  (define (script-user-data-directory)
    (call-prim 'DataPath))

  ;;; Returns a path to the directory which should be used to store any
  ;;; user-and-machine-specific script data files.  
  (define (script-user-local-data-directory)
    (define dir (call-prim 'DataPathLocal))
    ;; We may actually have to create this one; the engine doesn't
    ;; necessarily do it for us.
    (unless (directory-exists? dir)
      (make-directory dir))
    dir)

  ;;; An abstract superclass which implements typical GUI button behavior.
  (define-class %basic-button% (%custom-element%)
    (mix-in-standard-click-method! self)
    (attr enabled? #t :type <boolean> :label "Enabled?" :writable? #t)
  
    (value wants-cursor? (.enabled?))
    (after-updating enabled?
      (set! (.wants-cursor?) (.enabled?))
      (.invalidate))

    (def (initialize &rest args)
      (super)
      (set! (slot 'mouse-in-button?) #f))
      
    ;;; Valid states are DISABLED, NORMAL, PRESSED and ACTIVE.
    (def (button-state)
      (cond [(not (.enabled?))              'disabled]
            [(not (slot 'mouse-in-button?)) 'normal]
            [(mouse-grabbed-by? self)       'pressed]
            [#t                             'active]))

    ;;; Draw the button in the state specified by .BUTTON-STATE.
    (def (draw)
      (error (cat ".draw must be overridden on " self)))

    (def (mouse-enter event)
      (set! (slot 'mouse-in-button?) #t)
      (.invalidate))
    (def (mouse-leave event)
      (set! (slot 'mouse-in-button?) #f)
      (.invalidate))
    (def (mouse-down event)
      (grab-mouse self)
      (.invalidate)
      (refresh))
    (def (mouse-up event)
      (define was-grabbed? #f)
      (when (mouse-grabbed-by? self)
        (set! was-grabbed? #t)
        (ungrab-mouse self))
      (.invalidate)
      (refresh)
      (when (and (slot 'mouse-in-button?) was-grabbed?)
        (.click)))
    )

  ;;; Get the version of a QuickTime component, given the four-letter,
  ;;; case-sensitive type and subtype strings. Returns 0 if the component
  ;;; is not installed.
  (define (quicktime-component-version type subtype)
    (call-prim 'QTComponentVersion type subtype))

  ;;; Mark all events which were posted at or before this time--but which
  ;;; have yet to be processed--as stale.  These events can be later
  ;;; identified using EVENT-STALE?.  They will otherwise be processed in
  ;;; the normal fashion.
  ;;;
  ;;; @see event-stale?
  (define (mark-unprocessed-events-as-stale!)
    (call-prim 'MarkUnprocessedEventsAsStale))

  ;;; Register a file to be included in any debug (i.e., crash) reports
  ;;; generated for either the script or the engine itself.
  ;;;
  ;;; The description should be a short lowercase string.  Don't bother
  ;;; to capitalize the first word, or your description will look funny
  ;;; in the list of descriptions.
  (define (register-debug-report-file! file description)
    ;; TODO - This is an ugly hack to support absolute path names.
    (let [[path (if (absolute-path? file)
                    file
                    (build-path (current-directory) file))]]
      (call-prim 'DebugReportAddFile path description)))

  )
