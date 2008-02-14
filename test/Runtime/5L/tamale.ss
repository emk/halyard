;;=========================================================================
;;  Random Tamale Primitives
;;=========================================================================
;;  This is a collection of loosely documented and poorly-organized Tamale
;;  primitives, all subject to change at a moment's notice.

(module tamale (lib "language.ss" "5L")
  (require (lib "api.ss" "5L"))

  (require (lib "after-updating.ss" "5L"))
  (provide (all-from (lib "after-updating.ss" "5L")))
  

  ;;;======================================================================
  ;;;  Enabling Deprecated Features at Runtime
  ;;;======================================================================
  ;;;  Most of our deprecated features are provided by deprecated.ss, but
  ;;;  a few can only be enabled at runtime.

  (provide enable-deprecated-features!)

  (define *deprecated-features-enabled?* #f)
  
  ;;; Turn on various deprecated features.
  (define (enable-deprecated-features!)
    (set! *deprecated-features-enabled?* #t))

  ;; Backwards compatibility glue for code which refers to elements by
  ;; name.  Used by functions such as WAIT.
  (define (elem-or-name-hack elem-or-name)
    (if (and (symbol? elem-or-name)
             *deprecated-features-enabled?*)
        (begin
          (debug-caution (cat "Change '" elem-or-name
                              " to (@ " elem-or-name ")"))
          (@* elem-or-name))
        (begin
          (assert (element? elem-or-name))
          elem-or-name)))


  ;;;======================================================================
  ;;;  File and Path Functions
  ;;;======================================================================
  ;;;  Most of these are only used internally, in this file.

  (provide abstract-path->native-path make-native-path ensure-directory-exists)

  (define (url? path)
    (regexp-match "^(http|ftp|rtsp|file|gopher|about):" path))

  ;;; Given a path of the form "foo/bar.baz", convert it to a native OS
  ;;; path string.
  (define (abstract-path->native-path . args)
    (define reversed (reverse! args))
    (define abstract-component (car reversed))
    (define regular-components (reverse! (cdr reversed)))
    (path->string (apply build-path 
                         (append! regular-components
                                  (regexp-split "/" abstract-component)))))

  ;;; Build a path for accessing a script resource.  If PATH is a URL, it
  ;;; is returned unchanged.  Otherwise, assume that PATH is an abstract
  ;;; path relative to SUBDIR, and pass it to ABSTRACT-PATH->NATIVE-PATH.
  (define (make-native-path subdir path)
    (if (url? path)
        path
        (abstract-path->native-path (current-directory) subdir path)))

  (define (check-file path)
    (unless (or (url? path) (file-exists? path))
      (error (cat "No such file: " path))))
  
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

  (provide local->card %element% %invisible-element% %custom-element%
           %box% box %clickable-zone% clickable-zone
           delete-element delete-elements
           element-exists? delete-element-if-exists)

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

  (define (update-element-position elem)
    ;; Don't make me public.
    (call-5l-prim 'MoveElementTo (node-full-name elem)
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
      (call-5l-prim 'RaiseToTop (node-full-name self))
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
    (value shown? #f))

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
      (call-5l-prim 'Focus (node-full-name self))))

  ;; Let the engine know whether we want a cursor.  Note that the engine
  ;; knows nothing about 'auto, so we need to map it to #f manually.
  (define (set-wants-cursor! elem value)
    (call-5l-prim 'WantsCursorSet (node-full-name elem)
                  (case value
                    [[#f auto] #f]
                    [[#t] #t])))

  ;; Let the engine know whether we're currently dragging this object.
  (define (set-in-drag-layer?! elem value)
    (call-5l-prim 'ElementSetInDragLayer (node-full-name elem) value))
  
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
    ;; nodes.ss to set .wants-cursor? to #t whenever a mouse handler is
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
        (call-5l-prim 'OverlaySetShape (node-full-name self) (.shape))
        (call-5l-prim 'ZoneSetShape (node-full-name self)
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
          (error (cat "%custom-element%: " (node-full-name self)
                      " may not have a negative-sized shape: " 
                      original-shape "."))))
      )
    
    (def (create-engine-node)
      (if (.overlay?)
        (call-5l-prim 'Overlay (node-full-name self)
                      (parent->card self (.bounds))
                      (make-node-event-dispatcher self) (.cursor) (.alpha?)
                      (.clickable-where-transparent?))
        (call-5l-prim 'Zone (node-full-name self)
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

  ;;; Create a %box% element.
  (define (box bounds &key (name (gensym)) (parent (default-element-parent))
               (shown? #t))
    (%box% .new :name name :parent parent :shown? shown? :bounds bounds))

  ;;; A %clickable-zone% will run the specified ACTION when the user clicks on
  ;;; it.
  (define-class %clickable-zone% (%custom-element%)
    (attr action :label "Action" :writable? #t)
    (default overlay? #f)

    (def (mouse-down event)
      ((.action))))

  ;;; Create a %clickable-zone%.
  (define (clickable-zone bounds action
                          &key (name (gensym)) (cursor 'hand)
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
    (call-5l-prim 'ElementExists (node-full-name elem)))

  (define (set-element-shown?! elem show?)
    ;; Not all subclasses of %element% actually have a corresponding
    ;; engine object.
    (when (element-exists-in-engine? elem)
      (call-5l-prim 'ElementSetShown (node-full-name elem) show?)))

  ;;; Delete the specified element.
  (define (delete-element elem-or-name)
    ;; TODO - Get rid of elem-or-name-hack, and rename
    ;; delete-element-internal to delete-element.
    (delete-element-internal (elem-or-name-hack elem-or-name)))
  
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
      (delete-element (find-node (string->symbol (cat (node-full-name parent)
                                                      "/" name)) #t))))


  ;;;======================================================================
  ;;;  Drawing Functions
  ;;;======================================================================
  ;;;  Most of these functions have a corresponding element, below.

  (provide draw-graphic measure-graphic mask
           set-image-cache-size! with-dc
           dc-rect color-at clear-dc
           draw-line draw-rectangle draw-rectangle-outline)

  ;;; Draw a graphic loaded from PATH at point P in the current DC.  You
  ;;; may optionally specify a sub-rectangle of the graphic to draw.
  (define (draw-graphic p path &key (subrect :rect #f))
    (let [[native (make-native-path "Graphics" path)]]
      (check-file native)
      (if subrect
          (call-5l-prim 'LoadSubPic native p subrect)
          (call-5l-prim 'LoadPic native p))))
  
  ;;; Return a rectangle located at 0,0 large enough to hold the graphic
  ;;; specified by NAME.
  (define (measure-graphic path)
    (let [[native (make-native-path "Graphics" path)]]
      (check-file native)
      (call-5l-prim 'MeasurePic native)))

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
    (let [[native (make-native-path "Graphics" path)]]
      (check-file native)
      (call-5l-prim 'Mask native p))) 

  ;;; Specify how many bytes may be used by the engine to cache
  ;;; recently-used images.
  (define (set-image-cache-size! bytes)
    (call-5l-prim 'SetImageCacheSize bytes))

  ;;; Perform a set of drawing calls in the specified overlay.
  ;;;
  ;;; @syntax (with-dc dc body ...)
  ;;; @param NODE dc The overlay to draw into.
  ;;; @param BODY body The drawing code.
  (define-syntax with-dc
    (syntax-rules ()
      [(with-dc dc body ...)
       (dynamic-wind
           (lambda () (call-5l-prim 'DcPush (node-full-name dc)))
           (lambda () (begin/var body ...))
           (lambda () (call-5l-prim 'DcPop (node-full-name dc))))]))
  (define-syntax-indent with-dc 1)

  ;;; Get the bounding rectangle of the current DC.
  (define (dc-rect)
    (call-5l-prim 'DcRect))

  ;;; Get the color at the specified point.
  (define (color-at p)
    (call-5l-prim 'ColorAt p))

  ;;; Clear the current DC to color C.
  (define (clear-dc c)
    (call-5l-prim 'Screen c))

  ;;; Draw a line between two points using the specified color and width.
  (define (draw-line from to c width)
    (call-5l-prim 'DrawLine from to c width))

  ;;; Draw a filled rectangle in the specified color.
  (define (draw-rectangle r c)
    (call-5l-prim 'DrawBoxFill r c))

  ;;; Draw the outline of a rectangle, with the specified color and line width.
  (define (draw-rectangle-outline r c width)
    (call-5l-prim 'DrawBoxOutline r c width))

  
  ;;;======================================================================
  ;;;  Graphic Elements
  ;;;======================================================================
  ;;;  These templates each correspond to a drawing function, and should
  ;;;  generally produce visually identical output.
  ;;;
  ;;;  These templates are expected to get more complex.  In particular,
  ;;;  most of them will get keyword arguments to support accessibility,
  ;;;  and most of their properties will become settable.

  (provide %text-box% text-box %text% text
           %graphic% graphic
           %rectangle% rectangle rectangle-outline)
  
  ;;; A static text element with a specified bounding rectangle.
  (define-class %text-box% (%custom-element%)
    (attr style :label "Style" :writable? #t)
    (attr text :type <string> :label "Text" :writable? #t)
    (default alpha? #t)
 
    (after-updating [style text]
      (.invalidate))

    (def (draw)
      (draw-text (dc-rect) (.style) (.text))))
  
  ;;; Create a new %text-box% element.
  (define (text-box bounds style text
                    &key (name (gensym)) (parent (default-element-parent))
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
    )
  
  ;;; Create a new %fitted-text% element.
  (define (text p style text
                &key (name (gensym)) (parent (default-element-parent))
                (shown? #t) (max-width (rect-width $screen-rect)))
    (%text% .new :name name :parent parent :shown? shown?
                 :at p :max-width max-width :style style :text text))
  
  
  ;;; A simple graphic.  For now, you must specify the :ALPHA? value you
  ;;; want; the engine can't compute a reasonable value automatically.
  (define-class %graphic% (%custom-element%)
    (attr path :type <string> :label "Path" :writable? #t)

    (value shape (measure-graphic (.path)))
    (after-updating path
      (set! (.shape) (measure-graphic (.path)))
      (.invalidate))

    ;; TODO - Optimize erase-background now that we can update the graphic.
    (def (draw)
      (draw-graphic (point 0 0) (.path))))
  
  ;;; Create a new %graphic%.
  (define (graphic p path
                   &key (name (gensym)) (alpha? #f)
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
  
  ;;; Create a new %rectangle%.
  (define (rectangle r c
                     &key (name (gensym)) (parent (default-element-parent))
                     (shown? #t) (outline-width 1)
                     (outline-color $transparent))
    (%rectangle% .new :name name :parent parent :shown? shown? :bounds r 
                      :color c :outline-width outline-width 
                      :outline-color outline-color))
  
  ;;; Create a new %rectangle% with an outline and a transparent center.
  (define (rectangle-outline r c width
                             &key (name (gensym)) (shown? #t)
                             (parent (default-element-parent)))
    (%rectangle% .new :name name :parent parent :shown? shown? :bounds r
                      :color $transparent :outline-width width 
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

    (def (create-engine-element)
      (call-5l-prim 'CursorElement (node-full-name self)
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
            (point-difference (event-position event) (.hotspot))))

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
  ;;;  Animated Graphic Elements (deprecated)
  ;;;======================================================================

  (provide %animated-graphic%)

  (define (animated-graphic-shape graphics)
    (define max-width 0)
    (define max-height 0)
    (foreach [graphic graphics]
      (define bounds (measure-graphic graphic))
      (when (> (rect-width bounds) max-width)
        (set! max-width (rect-width bounds)))
      (when (> (rect-height bounds) max-height)
        (set! max-height (rect-height bounds))))
    (rect 0 0 max-width max-height))

  ;;; An animated graphic is a specialized overlay that can be
  ;;; animated under state-db control.  In order to use it, create an
  ;;; %animated-graphic% passing in the list of graphics you would
  ;;; like to change between to :GRAPHICS, and the state DB path you
  ;;; would like to use to control the graphic to :STATE-PATH.  Also,
  ;;; create the following state DB keys, and set them in order to
  ;;; control the animation:
  ;;;
  ;;; <state-path>/index   Set this to the index within the GRAPHICS list
  ;;;                      that you want to be displayed.
  ;;; <state-path>/x       These do some sort of movement, not documented
  ;;; <state-path>/y       at the moment. For now, set them both to 0
  ;;;
  ;;; DEPRECATED: For normal code, please use %sprite% instead.  This
  ;;; class is primarily intended for use with Quake 2 overlays and the
  ;;; state-db.
  (define-class %animated-graphic% (%custom-element%)
    (attr state-path :type <symbol> :label "State DB Key Path")
    (attr graphics   :type <list>   :label "Graphics to display")
    (value shape (animated-graphic-shape (.graphics)))

    (def (create-engine-element)
      (call-5l-prim 'OverlayAnimated (node-full-name self)
                    (parent->card self
                                  (offset-rect (.shape) (.at)))
                    (make-node-event-dispatcher self) (.cursor)
                    (.alpha?) (.state-path)
                    (map (fn (p) (make-native-path "Graphics" p))
                         (.graphics))))
    )


  ;;;======================================================================
  ;;;  Sprites
  ;;;======================================================================
  ;;;  This is a replacement for %animated-graphic%.

  (provide %sprite% sprite)
  
  (define-class %sprite% (%custom-element%)
    (attr frames   :type <list>    :label "List of image files")
    (attr frame  0 :type <integer> :label "Index of current frame"
                   :writable? #t)

    (value shape (animated-graphic-shape (.frames)))

    (after-updating frame
      (.invalidate))

    (def (draw)
      (draw-graphic (point 0 0) (list-ref (.frames) (.frame)))))

  (define (sprite at frames 
                  &key (name (gensym)) (alpha? #f)
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
    (call-5l-prim 'SetZoneCursor (node-full-name elem) cursor))

  ;;; Register the graphic in FILENAME with the engine as a cursor named
  ;;; SYM.  If the hotspot is not in the default path, it should be
  ;;; specified explicitly.
  (define (register-cursor sym filename &key (hotspot (point -1 -1)))
    (let [[native (make-native-path "Graphics" (cat "cursors/" filename))]]
      (check-file native)
      (call-5l-prim 'RegisterCursor sym native hotspot)))

  ;;; Hide the cursor until the next time the mouse moves.  Only works in
  ;;; full-screen mode, and only if the underlying system allows it.
  (define (hide-cursor-until-mouse-moved!)
    (call-5l-prim 'HideCursorUntilMouseMoved))

  ;;; Get the current position of the mouse.  This function is almost
  ;;; certainly the wrong function to use; see the mouse-moved events
  ;;; instead.
  (define (mouse-position)
    ;; XXX - This keeps returning exciting results even if we're in the
    ;; background.  Yuck.
    (call-5l-prim 'MousePosition))

  ;;; Redirect all mouse events to ELEM until further notice.  For more
  ;;; information on how mouse grabbing works, consult the documentation
  ;;; for the wxWidgets GUI library (or any other GUI library--it tends to
  ;;; be very similar).
  ;;;
  ;;; @see ungrab-mouse mouse-grabbed? mouse-grabbed-by?
  (define (grab-mouse elem)
    (assert (element? elem))
    (call-5l-prim 'MouseGrab (node-full-name elem)))

  ;;; Ungrab the mouse, which must be currently grabbed by ELEM.
  ;;; @see grab-mouse
  (define (ungrab-mouse elem)
    (assert (element? elem))
    (call-5l-prim 'MouseUngrab (node-full-name elem)))

  ;;; Is the mouse currently grabbed?
  ;;; @see grab-mouse
  (define (mouse-grabbed?)
    (call-5l-prim 'MouseIsGrabbed))
  
  ;;; Is the mouse currently grabbed by ELEM?
  ;;; @see grab-mouse
  (define (mouse-grabbed-by? elem)
    (call-5l-prim 'MouseIsGrabbedBy (node-full-name elem)))


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
      (call-5l-prim 'ActiveXPropGet (node-full-name self) name))

    ;;; Set a property of an ActiveX element.
    (def (set-activex-prop! name value)
      (call-5l-prim 'ActiveXPropSet (node-full-name self) name value))

    (def (create-engine-node)
      (call-5l-prim 'ActiveX (node-full-name self) 
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

  (provide %browser% browser)

  ;;; A web browser element.
  (define-class %browser% (%widget%)
    (attr path :type <string> :label "Path" :default "about:blank" 
               :writable? #t)
    ;;; WARNING: The fallback browser is not very good, and only
    ;;; tends to work under carefully controlled circumstances.  In
    ;;; particular, it tends to deal with errors by popping up ugly
    ;;; WX error dialogs at the user.  It's probably suitable for
    ;;; online help, but not much else.  Try to avoid using it if
    ;;; you have any choice.
    (attr fallback? #f :type <boolean> 
                       :label "Use primitive fallback web browser?")
    
    (after-updating path
      (define native (make-native-path "HTML" (.path)))
      (check-file native)
      (call-5l-prim 'BrowserLoadPage (node-full-name self) native))

    ;;; Load the specified page in the web browser.  Can be pointed
    ;;; to either the local HTML folder or to a URL.
    (def (load-page page)
      (set! (.path) page))
    
    ;;; Return true if and only if COMMAND should be enabled.  Supported
    ;;; values are: BACK, FORWARD, RELOAD and STOP.
    (def (command-enabled? command)
      (case command
        [[back]
         (call-5l-prim 'BrowserCanBack (node-full-name self))]
        [[forward]
         (call-5l-prim 'BrowserCanForward (node-full-name self))]
        [[reload]
         (call-5l-prim 'BrowserCanReload (node-full-name self))]
        [[stop]
         (call-5l-prim 'BrowserCanStop (node-full-name self))]
        [else
         (super)]))
    (.propagate 'command-enabled?)

    ;;; Go back to the previous page.
    (def (back)
      (call-5l-prim 'BrowserBack (node-full-name self)))
    ;;; Go forward.
    (def (forward)
      (call-5l-prim 'BrowserForward (node-full-name self)))
    ;;; Reload the currently displayed page.
    (def (reload)
      (call-5l-prim 'BrowserReload (node-full-name self)))
    ;;; Stop loading the currently displayed page.
    (def (stop)
      (call-5l-prim 'BrowserStop (node-full-name self)))

    (def (create-engine-node)
      (call-5l-prim 'Browser (node-full-name self) 
                    (make-node-event-dispatcher self)
                    (parent->card self (.rect))
                    (.fallback?))
      (.load-page (.path)))

    (def (setup-finished)
      (super)
      ;; We don't update the UI until very late in the processing, giving
      ;; everybody's SETUP a chance to complete.
      (define (update-command command)
        (.update-ui (make <update-ui-event> :command command)))
      (update-command 'back)
      (update-command 'forward)
      (update-command 'reload)
      (update-command 'stop))

    )

  ;;; Create a new %browser% object.
  (define (browser r path
                   &key (name (gensym)) (parent (default-element-parent))
                   (shown? #t))
    (%browser% .new :name name :parent parent :shown? shown?
                    :rect r :path path))


  ;;;======================================================================
  ;;;  Text Editing
  ;;;======================================================================

  (provide %edit-box% edit-box)

  ;;; A native GUI edit box.
  (define-class %edit-box% (%widget%)
    (attr font-size 9 :type <integer> :label "Font size")
    (attr multiline? #f :type <boolean> :label "Allow multiple lines?")
    (attr send-enter-event? #t :type <boolean>)
      
    ;; TODO - We need some way to declare the :label field that corresponds
    ;; to this "virtual attr".
    (default text "")

    ;;; Return the text from this edit box.
    (def (text)
      (call-5l-prim 'EditBoxGetValue (node-full-name self)))

    ;;; Set the text in this edit box.
    (def (set-text! value)
      (call-5l-prim 'EditBoxSetValue (node-full-name self) value))
    
    ;;; Move the insertion point to the specificed location.  Indices start
    ;;; at 0, and an index of -1 specifies "after the last character".
    (def (set-insertion-point! index)
      (.focus)
      (call-5l-prim 'EditBoxSetInsertionPoint (node-full-name self) index))
    ;;; Set the selection.  Indices are the same as SET-INSERTION-POINT!.
    (def (set-selection! start end)
      (.focus)
      (call-5l-prim 'EditBoxSetSelection (node-full-name self) start end))

    (def (create-engine-node)
      (call-5l-prim 'EditBox (node-full-name self)
                    (make-node-event-dispatcher self)
                    (parent->card self (.rect)) (.text)
                    (.font-size) (.multiline?) (.send-enter-event?)))
    )

  ;;; Create an %edit-box%.
  (define (edit-box r text
                    &key (name (gensym)) (font-size 9)
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
    (call-5l-prim 'MoviePause (node-full-name elem)))

  ;; (Internal use only.)  Resume a media element.
  (define (media-resume elem)
    (call-5l-prim 'MovieResume (node-full-name elem)))
  
  ;; (Internal use only.)  End playback of a media element. From the
  ;; perspective of the WAIT function, the media element will skip
  ;; immediately to the end of playback.
  (define (media-end-playback elem)
    (call-5l-prim 'MovieEndPlayback (node-full-name elem)))

  ;; (Internal use only.)  Set the volume of a media element.  Channels may
  ;; be LEFT, RIGHT, ALL, or something else depending on the exact type of
  ;; media being played.  Volume ranges from 0.0 to 1.0.
  (define (set-media-volume! elem channel volume)
    (call-5l-prim 'MediaSetVolume (node-full-name elem) channel volume))  
           
  ;; (Internal use only.)  If we can find an appropriate caption file, then
  ;; attach it to a media element.  We assume that captions live in
  ;; LocalMedia, because we can't load them from over the network (which we
  ;; might need to do with things normally stored in Media).  If this is a
  ;; problem for you, you may want to encode your captions as a track in
  ;; the media itself.
  (define (media-maybe-attach-caption-file! elem path)
    (let [[native (make-native-path "LocalMedia" (cat path ".capt"))]]
      (when (and (not (url? native)) (file-exists? native))
        (call-5l-prim 'MediaAttachCaptionFile (node-full-name elem) native))))

  (define (add-common-media-methods! klass)
    (with-instance klass
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
        (call-5l-prim 'MovieSetPlaybackTimer (node-full-name self) frame))
      
      ;;; Clear an exiting playback timer.
      (def (clear-playback-timer!)
        (call-5l-prim 'MovieClearPlaybackTimer (node-full-name self)))
      ))

  ;;; The superclass of all audio-only elements.
  ;;; @see %movie%
  (define-class %audio-element% (%invisible-element%)
    (attr volume 1.0 :type <number> :label "Volume (0.0 to 1.0)")      
    (add-common-media-methods! self))

  ;;; Pause script execution until the end of the specified media element,
  ;;; or until a specific frame is reached.
  (define (wait elem-or-name &key frame)
    (define name (node-full-name (elem-or-name-hack elem-or-name)))
    (if frame
        (call-5l-prim 'Wait name frame)
        (call-5l-prim 'Wait name)))
  
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
  
  (provide %geiger-audio% geiger-audio %geiger-synth% geiger-synth
           %sine-wave% sine-wave)

  (define-class %geiger-audio% (%audio-element%)
    (attr path :type <string> :label "Path")
      
    (def (set-counts-per-second! counts)
      (call-5l-prim 'AudioStreamGeigerSetCps (node-full-name self) counts))
    
    (def (create-engine-node)
      (call-5l-prim 'AudioStreamGeiger (node-full-name self)
                    (make-node-event-dispatcher self)
                    (build-path (current-directory) "LocalMedia" (.path))
                    (.volume))))

  (define (geiger-audio path &key (name (gensym)) (volume 1.0)
                        (parent (default-element-parent)))
    (%geiger-audio% .new :name name :parent parent :path path :volume volume))

  (define-class %geiger-synth% (%audio-element%)
    (attr state-path) 
    (attr chirp) 
    (attr loops)
    
    (def (create-engine-node)
      (apply call-5l-prim 'GeigerSynth (node-full-name self) (.state-path)
             (build-path (current-directory) "LocalMedia" (.chirp))
             (.volume)
             (* 512 1024)
             (map (fn (item)
                    (if (string? item)
                        (build-path (current-directory) "LocalMedia" item)
                        item))
                  (.loops)))))

  (define (geiger-synth state-path chirp loops
                        &key (name (gensym)) (parent (default-element-parent)))
    (%geiger-synth% .new :name name :parent parent :state-path state-path
                         :chirp chirp :loops loops))

  ;;; Plays a pure sine-wave tone.
  (define-class %sine-wave% (%audio-element%)
    (attr frequency :type <integer> :label "Frequency (Hz)")
      
    (def (create-engine-node)
      (call-5l-prim 'AudioStreamSine (node-full-name self)
                    (make-node-event-dispatcher self)
                    (.volume) (.frequency))))

  ;;; Create a %sine-wave%.
  (define (sine-wave frequency
                     &key (name (gensym)) (parent (default-element-parent))
                     (volume 1.0))
    (%sine-wave% .new :name name :parent parent :frequency frequency 
                      :volume volume))


  ;;;======================================================================
  ;;;  Vorbis Audio
  ;;;======================================================================
  ;;;  Vorbis audio streams.  These are most useful for foley and for
  ;;;  background audio which should continue playing even if the engine is
  ;;;  otherwise occupied.
  
  (provide %vorbis-audio% vorbis-audio)

  ;;; Plays an Ogg Vorbis audio stream.
  (define-class %vorbis-audio% (%audio-element%)
    (attr path       :type <string>  :label "Path")
    (attr buffer 512 :type <integer> :label "Buffer Size (K)")
    (attr loop?  #f  :type <boolean> :label "Loop this clip?")

    (def (create-engine-node)
      (let [[path (make-native-path "LocalMedia" (.path))]]
        (check-file path)
        (call-5l-prim 'AudioStreamVorbis (node-full-name self)
                      (make-node-event-dispatcher self) path
                      (.volume) (* 1024 (.buffer)) (.loop?))))

    (setup
      (media-maybe-attach-caption-file! self (.path))))
  
  ;;; Create a %vorbis-audio% element.
  (define (vorbis-audio path
                        &key (name (gensym)) (parent (default-element-parent))
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
    (directory-exists? (build-path (current-directory) "Media")))

  ;;; Return true if and only if we have media available on CD.
  (define (media-cd-is-available?)
    (and *cd-media-directory* #t))

  ;;; Try to determine whether or not we have a CD with our media
  ;;; files on it.  We look at each drive on the system, and see
  ;;; whether it contains a Media directory with a file named
  ;;; 'pathname'.  You can use '/' as a path separator in pathname,
  ;;; as usual.
  (define (search-for-media-cd pathname)
    (label return
      (foreach [drive (filesystem-root-list)]
        (define candidate (build-path drive "Media"))
        (define file (abstract-path->native-path candidate pathname))
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
    ;; Create some of the paths we'll check.
    (define hd-path-1 (make-native-path "LocalMedia" path))
    (define hd-path-2 (make-native-path "Media" path))
    (define cd-path
      (if (media-cd-is-available?)
          (abstract-path->native-path *cd-media-directory* path)
          #f))

    (cond
     ;; Pass explicit URLs straight through.
     [(url? path)
      path]
     ;; Otherwise, first check our media directory for the file.
     [(file-exists? hd-path-1)
      hd-path-1]
     [(file-exists? hd-path-2)
      hd-path-2]
     ;; Then check the CD, if we have one.
     [(and cd-path (file-exists? cd-path))
      cd-path]
     ;; If all else fails, and we've been told about a server, assume our
     ;; media is there.
     [*media-base-url*
      (cat *media-base-url* "/" path)]
     ;; OK, we give up.
     [#t
      (error (cat "Cannot find file " path))]))


  ;;;======================================================================
  ;;;  Movie Elements
  ;;;======================================================================

  (provide %movie% movie)

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
      (call-5l-prim 'MovieSetTimeout (node-full-name self) seconds))

    (def (create-engine-node)
      (define path (media-path (.path)))
      (check-file path)
      (call-5l-prim 'Movie (node-full-name self)
                    (make-node-event-dispatcher self)
                    (parent->card self (.rect))
                    path (.volume)
                    (.controller?) (.audio-only?) (.loop?) (.interaction?)
                    (.report-captions?)))

    (setup
      (media-maybe-attach-caption-file! self (.path)))
    )

  ;;; Create a %movie%.
  (define (movie r path
                 &key (name (gensym)) (volume 1.0)
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
  ;;;  State DB Debugging Support
  ;;;======================================================================

  (provide state-db-debug)

  (define-class %state-db-debugger%  (%invisible-element%)
    (attr path)
    (attr report-fn)

    (setup
      (define-state-db-listener (debug state-db)
        ((.report-fn) (state-db (.path))))))
  
  ;;; Here's a nasty little hack for reading the state database from
  ;;; outside an element.  Calling this from anywhere but the listener is
  ;;; definitely a bug.
  (define (state-db-debug path)
    (define result #f)
    (define (set-result! value)
      (set! result value))
    (define elem
      (%state-db-debugger% .new :parent (running-root-node)
                                :path path
                                :report-fn set-result!))
    (delete-element elem)
    result)

  
  ;;;======================================================================
  ;;;  State DB Time Support
  ;;;======================================================================
  ;;;  The state-db's /system/clock/seconds and /system/clock/milliseconds
  ;;;  values do not use the same units as CURRENT-MILLISECONDS.

  (provide state-db-seconds state-db-milliseconds)

  ;;  XXX - THESE SHOULD NOT USE STATE-DB-DEBUG!!! It's extremely slow.
  ;;  Ask one of the C++ programmers to add primitives which fetch
  ;;  these values.
  
  ;;; Get the current time in seconds, as recorded by the state-db.
  (define (state-db-seconds)
    (state-db-debug '/system/clock/seconds))

  ;;; Get the current time in miliseconds, as recorded by the state-db.
  (define (state-db-milliseconds)
    (state-db-debug '/system/clock/milliseconds))

  
  ;;;======================================================================
  ;;;  State DB Update Support
  ;;;======================================================================

  (provide update-state-db! inc-state-db! inc-state-db-if-true!)

  ;;  XXX - THIS SHOULD NOT USE STATE-DB-DEBUG!!! It's extremely slow.
  ;;  Ask one of the C++ programmers to add a primitive to do this.
  
  ;;; Apply FUNC to the current value stored at PATH, and replace it with
  ;;; the return value.
  (define (update-state-db! path func)
    (set! (state-db path) (func (state-db-debug path)))
    #f) ; Make absolutely certain we don't return the value we updated.
  
  ;;; Add AMOUNT to the value stored in the state-db at PATH.
  ;;;
  ;;; TODO - Decide whether this is redundant.
  (define (inc-state-db! path amount)
    (update-state-db! path (fn (value) (+ value amount))))

  ;;; Add AMOUNT to the value stored in the state-db at PATH, unless that
  ;;; value is #f.
  ;;;
  ;;; TODO - Decide whether this is redundant.
  (define (inc-state-db-if-true! path amount)
    (update-state-db! path
                      (fn (value)
                        (if value
                            (+ value amount)
                            value))))
  

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
    (call-5l-prim 'Dialog title text button1 button2 button3))

  ;;; Delay for the specified number of milliseconds.  Calls IDLE, so
  ;;; events will be processed and the screen will be repainted.
  (define (sleep-milliseconds milliseconds)
    (define end-time (+ milliseconds (current-milliseconds)))
    (let repeat-delay []
      (idle)
      (when (> end-time (current-milliseconds))
        (repeat-delay))))    

  ;;; Sleep for the specified number of tenths of seconds.
  (define (nap tenths)
    (sleep-milliseconds (* 100 tenths)))

  ;;; Copy a string to the OS clipboard.  Used mostly by developer tools.
  (define (copy-string-to-clipboard string)
    (call-5l-prim 'CopyStringToClipboard string))

  ;;; Open a URL in the user's default browser.  Returns true if the
  ;;; browser is launched successfully, or false if something obvious goes
  ;;; wrong.  Note that this will minimize the currently running program
  ;;; (if we're in full-screen mode), and that many users may have trouble
  ;;; figuring out to get back once they're done browsing.
  (define (open-in-browser url)
    (call-5l-prim 'OpenInBrowser url))

  ;;; Returns a path to the directory which should be used to store any
  ;;; user-specific script data files.  Under Windows, this directory may
  ;;; actually get copied between login sessions on different machines, so
  ;;; it would be rude to store huge files here.  See
  ;;; SCRIPT-USER-LOCAL-DATA-DIRECTORY instead for big files.
  (define (script-user-data-directory)
    (call-5l-prim 'DataPath))

  ;;; Returns a path to the directory which should be used to store any
  ;;; user-and-machine-specific script data files.  
  (define (script-user-local-data-directory)
    (define dir (call-5l-prim 'DataPathLocal))
    ;; We may actually have to create this one; the engine doesn't
    ;; necessarily do it for us.
    (unless (directory-exists? dir)
      (make-directory dir))
    dir)

  ;;; An abstract superclass which implements typical GUI button behavior.
  (define-class %basic-button% (%custom-element%)
    (attr action   (callback) :type <function> :label "On click" :writable? #t)
    (attr enabled? #t         :type <boolean> :label "Enabled?"  :writable? #t)
  
    (value wants-cursor? (.enabled?))
    (after-updating enabled?
      (set! (.wants-cursor?) (.enabled?))
      (.invalidate))

    (def (initialize &rest args)
      (super)
      (set! (slot 'mouse-in-button?) #f))
      
    (def (button-state)
      (cond [(not (.enabled?))              'disabled]
            [(not (slot 'mouse-in-button?)) 'normal]
            [(mouse-grabbed-by? self)       'pressed]
            [#t                             'active]))

    (def (draw)
      (.draw-button (.button-state)))

    ;;; Draw the button in the specified state.  Valid states are DISABLED,
    ;;; NORMAL, PRESSED and ACTIVE.  Must be overridden by subclasses.
    ;;; TODO - This method should go away (case 2421).
    (def (draw-button state)
      (error "draw-button must be overridden"))

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
        (.button-clicked event)))

    (def (button-clicked event)
      ((.action)))
    )

  ;;; Get the version of a QuickTime component, given the four-letter,
  ;;; case-sensitive type and subtype strings. Returns 0 if the component
  ;;; is not installed.
  (define (quicktime-component-version type subtype)
    (call-5l-prim 'QTComponentVersion type subtype))

  ;;; Mark all events which were posted at or before this time--but which
  ;;; have yet to be processed--as stale.  These events can be later
  ;;; identified using EVENT-STALE?.  They will otherwise be processed in
  ;;; the normal fashion.
  ;;;
  ;;; @see event-stale?
  (define (mark-unprocessed-events-as-stale!)
    (call-5l-prim 'MarkUnprocessedEventsAsStale))

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
                    (abstract-path->native-path (current-directory) file))]]
      (call-5l-prim 'DebugReportAddFile path description)))

  )
