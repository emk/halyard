;;=========================================================================
;;  Random Tamale Primitives
;;=========================================================================
;;  This is a collection of loosely documented and poorly-organized Tamale
;;  primitives, all subject to change at a moment's notice.

(module tamale (lib "lispish.ss" "5L")
  (require (lib "api.ss" "5L"))


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

  (provide abstract-path->native-path ensure-directory-exists)

  (define (url? path)
    (regexp-match "^(http|ftp|rtsp|file):" path))

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
      (throw (cat "No such file: " path))))
  
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
        (local->card (node-parent node) (offset-by-point x (prop node at)))
        x))

  (define (parent->card node x)
    (local->card (node-parent node) x))

  (define (update-element-position elem)
    ;; Don't make me public.
    (call-5l-prim 'MoveElementTo (node-full-name elem)
                  (parent->card elem (prop elem at)))
    (foreach [e (node-elements elem)]
      (update-element-position e)))

  ;;; The abstract superclass of all elements.
  (define-element-template %element%
      [[at :type <point> :label "Position"]
       [shown? :type <boolean> :label "Shown?" :default #t]]
      ()
    (on prop-change (name value prev veto)
      (case name
        [[at]
         (update-element-position self)]
        [[shown?]
         (set! (element-shown? self) value)]
        [else
         (call-next-handler)]))
    ;;; Return the bounding box of this element, or #f if it has no bounds.
    (on bounds ()
      #f)
    (on setup-finished ()
      ;; TODO - This is technically too late to set this value, and we should
      ;; probably add a SHOWN? parameter to every object creation primitive.
      (set! (element-shown? self) shown?)))

  ;;; The abstract superclass of all elements which have no on-screen
  ;;; representation.
  (define-element-template %invisible-element% []
      (%element% :at (point 0 0) :shown? #f))

  ;;; The superclass of all native GUI elements which can be displayed
  ;;; on the stage.
  (define-element-template %widget%
      [[rect :type <rect> :label "Rectangle"]]
      (%element% :at (rect-left-top rect))
    (on bounds ()
      rect))

  ;;; A %custom-element% is a lightweight element (i.e., implemented by the
  ;;; engine, not by the OS), optionally with an associated drawing
  ;;; overlay.
  (define-element-template %custom-element%
      [[at :new-default (point 0 0)]
       [shape :type <shape> :label "Shape"]
       [cursor :type <symbol> :default 'hand :label "Cursor"]
       [overlay? :type <boolean> :default #t :label "Has overlay?"]
       [alpha? :type <boolean> :default #f :label "Overlay transparent?"]
       [wants-cursor? :default 'auto :label "Wants cursor?"]
       [clickable-where-transparent? :type <boolean> :default #f
                                     :label "Clickable where transparent?"]
       [dragging? :type <boolean> :default #f :label "In drag layer?"]
       [%nocreate? :type <boolean> :default #f
                   :label "Set to true if subclass creates in engine"]]
      (%element%)
    
    ;; This variable will disable passing prop change events to %ELEMENT%
    ;; while we're initializing out AT and SHAPE paremeters. If we didn't
    ;; do that, element would try and pass our changes to AT to the underlying
    ;; object, which hasn't been created yet. See below for why we need to muck
    ;; with AT during element creation. 
    (define initializing-origin? #t)
    
    ;; Let the engine know whether we want a cursor.
    (define (set-wants-cursor! value)
      (call-5l-prim 'WantsCursorSet (node-full-name self)
                    (case value
                      [[#f auto] #f]
                      [[#t] #t])))

    ;; Let the engine know whether we're currently dragging this object.
    (define (set-in-drag-layer?! value)
      (call-5l-prim 'ElementSetInDragLayer (node-full-name self) value))

    (on bounds ()
      (bounds (offset-by-point (prop self shape) (prop self at))))

    (on setup-finished ()
      (call-next-handler)
      ;; We need to postpone this until the underlying engine object
      ;; is created.
      (set-wants-cursor! wants-cursor?)
      (set-in-drag-layer?! dragging?)
      (send self invalidate))

    ;;; Invalidate the contents of this %custom-element% (perhaps because
    ;;; of a property change).  Makes sure that DRAW gets called before the
    ;;; next screen update.
    (on invalidate ()
      (when overlay?
        (with-dc self
          (send self erase-background)
          (send self draw))))
    
    ;; All overlays are erased by the engine at creation time, so we can
    ;; skip the first call to ERASE-BACKGROUND.  Yes, this is a performance
    ;; hack.
    (define erased-by-engine? #t)
    
    ;;; Erases the background of an overlay in preperation for a redraw.
    ;;; If you know that your DRAW handler overwrites the entire overlay,
    ;;; you can override this function and have it do nothing.
    (on erase-background ()
      (unless erased-by-engine?
        (clear-dc (if alpha? $transparent $black)))
      (set! erased-by-engine? #f))

    ;;; Draw this element, based on current property values.  Does not
    ;;; actually update the user-visible screen until the next REFRESH or
    ;;; blocking function.
    (on draw ()
      #f)

    (on prop-change (name value prev veto)
      (case name
        [[cursor] (set-element-cursor! self value)]
        [[at shape]
         (unless initializing-origin? 
           ;; We want to not pass these messages to our parent classes until 
           ;; we're done mucking with AT and SHAPE ourselves. 
           (call-next-handler))]
        [[wants-cursor?]
         (set-wants-cursor! value)]
        [[dragging?]
         (set-in-drag-layer?! value)]
        [else (call-next-handler)]))
    
    ;; The way we want custom elements to work is that AT represents the
    ;; origin of a custom element, and the actual shape on the screen is
    ;; SHAPE offset by AT. The problem is that this is very inconvenient
    ;; for scripters, and doesn't match the basic zone interface that we've
    ;; always given them, which is that they just pass in the pre-offset
    ;; shape on the screen. In order to deal with this, we normalize the
    ;; parameters we are passed so that AT represents the origin of the
    ;; object and SHAPE is the shape relative to AT. We do this as follows.
    (define the-origin (shape-origin shape))
    ;; 1. Check to make sure we don't have both AT and SHAPE away from the 
    ;;    origin.
    (unless (or (equals? at (point 0 0))
                (equals? the-origin (point 0 0)))
      (error "Cannot specify AT unless SHAPE has its origin at 0,0: "
             (node-full-name self)))
    (define real-shape
      (if (equals? the-origin (point 0 0))
        ;; 2. If the origin of our SHAPE parameter is (0,0), then we'll leave
        ;;    SHAPE and AT alone, and just pass to our underlying object our 
        ;;    REAL-SHAPE, which is our SHAPE offset by our AT.
        (offset-by-point shape at)
        ;; 3. If our AT parameter is (0,0), then we want to move our AT to the 
        ;;    origin of our shape, and then move the shape such that its origin
        ;;    is (0,0). REAL-SHAPE will be our original shape. 
        (let ((orig-shape shape))
          (set! at the-origin)
          (set! shape (offset-by-point orig-shape 
                                       (point
                                        (- (point-x the-origin))
                                        (- (point-y the-origin)))))
          orig-shape)))
    ;; We're done mucking around with our AT parameter, so we can now allow 
    ;; prop change messages for AT to go to propagate to our parent classes. 
    (set! initializing-origin? #f)
    
    (cond
     [%nocreate?
      #f]
     [overlay?
      (call-5l-prim 'Overlay (node-full-name self)
                    (parent->card self real-shape)
                    (make-node-event-dispatcher self) cursor alpha?
                    clickable-where-transparent?)]
     [else
      (call-5l-prim 'Zone (node-full-name self)
                    (parent->card self (as <polygon> real-shape))
                    (make-node-event-dispatcher self) cursor)]))
  
  ;;; A box is a generic, invisible container element.  It exists only
  ;;; to be the parent of other elements.
  (define-element-template %box%
      []
      (%custom-element% :overlay? #f)
    #f)

  ;;; Create a %box% element.
  (define (box shape &key (name (gensym)) (parent (default-element-parent)))
    (create %box% :name name :parent parent :shape shape))

  ;;; A %clickable-zone% will run the specified ACTION when the user clicks on
  ;;; it.
  (define-element-template %clickable-zone%
      [[action :label "Action"]
       [overlay? :new-default #f]]
      (%custom-element%)
    (on prop-change (name value prev veto)
      (case name
        [[action] (void)]
        [else (call-next-handler)]))
    (on mouse-down (event)
      (action)))

  ;;; Create a %clickable-zone%.
  (define (clickable-zone shape action
                          &key (name (gensym)) (cursor 'hand)
                          (overlay? #f) (alpha? #f)
                          (parent (default-element-parent)))
    (create %clickable-zone%
            :name name
            :parent parent
            :shape shape
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
                                                      "/" name))))))
  

  ;;;======================================================================
  ;;;  Drawing Functions
  ;;;======================================================================
  ;;;  Most of these functions have a corresponding element, below.

  (provide draw-graphic measure-graphic
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
  (define-element-template %text-box%
      [[style :label "Style"]
       [text :type <string> :label "Text"]]
      (%custom-element% :alpha? #t)
    (on prop-change (name value prev veto)
      (case name
        [[style text]
         (send self invalidate)]
        [else (call-next-handler)]))
    (on draw ()
      (draw-text (dc-rect) style text)))
  
  ;;; Create a new %text% element.
  (define (text-box r style text
                    &key (name (gensym)) (parent (default-element-parent)))
    (create %text-box%
            :name name :parent parent
            :shape r :style style :text text))
  
  ;;; A text element just large enough to fit the specified text.
  (define-element-template %text%
      [style text
       [max-width :label "Max width" :default (rect-width $screen-rect)]]
      (%text-box% :shape (measure-text style text :max-width max-width))
    ;; TODO - Implement prop-change correctly once we can resize an
    ;; overlay.  For now, we pass it through to %text-box%, which is wrong.
    #f)
  
  ;;; Create a new %fitted-text% element.
  (define (text p style text
                &key (name (gensym)) (parent (default-element-parent))
                (max-width (rect-width $screen-rect)))
    (create %text%
            :name name :parent parent
            :at p :max-width max-width :style style :text text))
  
  ;;; A simple graphic.  For now, you must specify the :ALPHA? value you
  ;;; want; the engine can't compute a reasonable value automatically.
  (define-element-template %graphic%
      [[path :type <string> :label "Path"]]
      (%custom-element% :shape (measure-graphic path))
    ;; TODO - Optimize erase-background once we can update the graphic.
    (on draw ()
      (draw-graphic (point 0 0) (prop self path))))
  
  ;;; Create a new %graphic%.
  (define (graphic p path
                   &key (name (gensym)) (alpha? #f)
                   (parent (default-element-parent)))
    (create %graphic%
            :name name :parent parent
            :at p :alpha? alpha? :path path))
  
  ;;; A rectangular element, filled with a single color.
  (define-element-template %rectangle%
      [[color :type <color> :label "Color" :default $transparent]
       [outline-width :type <integer> :label "Outline width" :default 1]
       [outline-color :type <color> :label "Outline color"
                      :default $transparent]]
      (%custom-element% :alpha? #t)
    ;; TODO - Optimize erase-background once we can update our properties.
    (on draw ()
      (unless (transparent? color)
        (draw-rectangle (dc-rect) color))
      (unless (transparent? outline-color)
        (draw-rectangle-outline (dc-rect) outline-color outline-width))))
  
  ;;; Create a new %rectangle%.
  (define (rectangle r c
                     &key (name (gensym)) (parent (default-element-parent))
                     (outline-width 1) (outline-color $transparent))
    (create %rectangle%
            :name name :parent parent :shape r :color c
            :outline-width outline-width :outline-color outline-color))
  
  ;;; Create a new %rectangle% with an outline and a transparent center.
  (define (rectangle-outline r c width
                             &key (name (gensym))
                             (parent (default-element-parent)))
    (create %rectangle%
            :name name :parent parent :shape r :color $transparent
            :outline-width width :outline-color c))
  

  ;;;======================================================================
  ;;;  Animated Graphic Elements
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

  ;;; An animated graphic is a specialized overlay that can be animated
  ;;; under state-db control.  See the C++ source for details.
  (define-element-template %animated-graphic%
      [[state-path :type <string> :label "State DB Key Path"]
       [graphics :type <list> :label "Graphics to display"]]
      (%custom-element%
       :shape (animated-graphic-shape graphics)
       :%nocreate? #t)
    (call-5l-prim 'OverlayAnimated (node-full-name self)
                  (parent->card self
                                (offset-rect (prop self shape) (prop self at)))
                  (make-node-event-dispatcher self) (prop self cursor)
                  (prop self alpha?) state-path
                  (map (fn (p) (make-native-path "Graphics" p)) graphics)))


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
    (assert (instance-of? elem <element>))
    (call-5l-prim 'MouseGrab (node-full-name elem)))

  ;;; Ungrab the mouse, which must be currently grabbed by ELEM.
  ;;; @see grab-mouse
  (define (ungrab-mouse elem)
    (assert (instance-of? elem <element>))
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
  (define-element-template %activex%
      [[activex-id :type <string>]]
      (%widget%)

    ;;; Get a property of an ActiveX element.
    (on activex-prop (name)
      (call-5l-prim 'ActiveXPropGet (node-full-name self) name))

    ;;; Set a property of an ActiveX element.
    (on set-activex-prop! (name value)
      (call-5l-prim 'ActiveXPropSet (node-full-name self) name value))

    (call-5l-prim 'ActiveX (node-full-name self) 
                  (make-node-event-dispatcher self)
                  (parent->card self (prop self rect))
                  activex-id))

  ;;; Show a Macromedia Flash movie scaled to fit the current card.
  ;;; Requires that the user have an appropriate version of Flash
  ;;; installed.  Macromedia reserves the right to break this interface in
  ;;; future versions of Flash, so this is more for demo purposes than
  ;;; anything else.
  (define-card-template %flash-card%
      [[path :type <string> :label "Path"]]
      ()
    (define flash
      (create %activex%
              :name 'flash
              :rect $screen-rect
              :activex-id "ShockwaveFlash.ShockwaveFlash"))
    (send flash set-activex-prop! "movie"
          (build-path (current-directory) "Flash" path)))
  

  ;;;======================================================================
  ;;;  Web Browser Support
  ;;;======================================================================

  (provide %browser% browser)

  ;;; A web browser element.
  (define-element-template %browser%
      [[path :type <string> :label "Path" :default "about:blank"]
       [fallback? :type <boolean> :label "Use primitive fallback web browser?"
                  :default #f]]
      (%widget%)

    ;;; Load the specified page in the web browser.  Can be pointed
    ;;; to either the local HTML folder or to a URL.
    (on load-page (page)
      ;; TODO - This function should be replaced by a settable location
      ;; property.
      (let [[native (make-native-path "HTML" page)]]
        (check-file native)
        (call-5l-prim 'BrowserLoadPage (node-full-name self) native)))

    ;;; Return true if and only if COMMAND should be enabled.  Supported
    ;;; values are: BACK, FORWARD, RELOAD and STOP.
    (on command-enabled? (command)
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
         (call-next-handler)]))

    ;;; Go back to the previous page.
    (on back ()
      (call-5l-prim 'BrowserBack (node-full-name self)))
    ;;; Go forward.
    (on forward ()
      (call-5l-prim 'BrowserForward (node-full-name self)))
    ;;; Reload the currently displayed page.
    (on reload ()
      (call-5l-prim 'BrowserReload (node-full-name self)))
    ;;; Stop loading the currently displayed page.
    (on stop ()
      (call-5l-prim 'BrowserStop (node-full-name self)))

    (on setup-finished ()
      (define (update-command command)
        (send* self 'update-ui
               :ignorable? #t
               :arguments (list (make <update-ui-event> :command command))))
      (call-next-handler)
      (update-command 'back)
      (update-command 'forward)
      (update-command 'reload)
      (update-command 'stop))

    (call-5l-prim 'Browser (node-full-name self) 
                  (make-node-event-dispatcher self)
                  (parent->card self (prop self rect))
                  fallback?)
    (send self load-page path))

  ;;; Create a new %browser% object.
  (define (browser r path
                   &key (name (gensym)) (parent (default-element-parent)))
    (create %browser% :name name :parent parent :rect r :path path))


  ;;;======================================================================
  ;;;  Text Editing
  ;;;======================================================================

  (provide %edit-box% edit-box)

  ;;; A native GUI edit box.
  (define-element-template %edit-box%
      [[text :type <string> :label "Initial text" :default ""]
       [font-size :type <integer> :label "Font size" :default 9]
       [multiline? :type <boolean> :label "Allow multiple lines?" :default #f]
       [send-enter-event? :type <boolean> :default #t]]
      (%widget%)
    (on text ()
      (call-5l-prim 'EditBoxGetValue (node-full-name self)))
    (call-5l-prim 'EditBox (node-full-name self)
                  (make-node-event-dispatcher self)
                  (parent->card self (prop self rect)) text
                  font-size multiline? send-enter-event?))

  ;;; Create an %edit-box%.
  (define (edit-box r text
                    &key (name (gensym)) (font-size 9)
                    (multiline? #f) (send-enter-event? #t)
                    (parent (default-element-parent)))
    (create %edit-box% :name name :parent parent :rect r :text text
            :font-size font-size :multiline? multiline?
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
           
  ;;; The superclass of all audio-only elements.
  ;;; @see %movie%
  (define-element-template %audio-element%
      [[volume       :type <number> :default 1.0 :label "Volume (0.0 to 1.0)"]]
      (%invisible-element%)

    ;; BEGIN DUPLICATE CODE - Because we don't have multiple inheritence,
    ;; the API below is shared with %movie%.

    ;;; Pause playback.
    (on pause ()
      (media-pause self))

    ;;; Resume playback.
    (on resume ()
      (media-resume self))

    ;;; End playback. From the perspective of the WAIT function, this media
    ;;; element will skip immediately to the end of playback.
    (on end-playback ()
      (media-end-playback self))
    (on end-playback ()
      (media-end-playback self))

    ;;; Set the volume of a media element.  Channels may be LEFT, RIGHT,
    ;;; ALL, or something else depending on the exact type of media being
    ;;; played.  Volume ranges from 0.0 to 1.0.
    (on set-volume! (channel volume)
      (set-media-volume! self channel volume))

    ;; END DUPLICATE CODE

    ;; I'd like put a ON PROP-CHANGE handler here for audio volume, but
    ;; it's not quite so easy, because there may be multiple channels to
    ;; contend with. Ugh.
    )

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

  (define-element-template %geiger-audio%
      [[path :type <string> :label "Path"]]
      (%audio-element%)
    (on set-counts-per-second! (counts)
      (call-5l-prim 'AudioStreamGeigerSetCps (node-full-name self) counts))
    (call-5l-prim 'AudioStreamGeiger (node-full-name self)
                  (make-node-event-dispatcher self)
                  (build-path (current-directory) "LocalMedia" path)
                  (prop self volume)))

  (define (geiger-audio path &key (name (gensym)) (volume 1.0)
                        (parent (default-element-parent)))
    (create %geiger-audio%
            :name name :parent parent :path path :volume volume))

  (define-element-template %geiger-synth%
      [state-path chirp loops]
      (%audio-element%)
    (apply call-5l-prim 'GeigerSynth (node-full-name self) state-path
           (build-path (current-directory) "LocalMedia" chirp)
           (prop self volume)
           (* 512 1024)
           (map (fn (item)
                  (if (string? item)
                      (build-path (current-directory) "LocalMedia" item)
                      item))
                loops)))

  (define (geiger-synth state-path chirp loops
                        &key (name (gensym)) (parent (default-element-parent)))
    (create %geiger-synth%
            :name name :parent parent :state-path state-path
            :chirp chirp :loops loops))

  ;;; Plays a pure sine-wave tone.
  (define-element-template %sine-wave%
      [[frequency :type <integer> :label "Frequency (Hz)"]]
      (%audio-element%)
    (call-5l-prim 'AudioStreamSine (node-full-name self)
                  (make-node-event-dispatcher self)
                  (prop self volume) frequency))

  ;;; Create a %sine-wave%.
  (define (sine-wave frequency
                     &key (name (gensym)) (parent (default-element-parent))
                     (volume 1.0))
    (create %sine-wave%
            :name name :parent parent :frequency frequency :volume volume))


  ;;;======================================================================
  ;;;  Vorbis Audio
  ;;;======================================================================
  ;;;  Vorbis audio streams.  These are most useful for foley and for
  ;;;  background audio which should continue playing even if the engine is
  ;;;  otherwise occupied.
  
  (provide %vorbis-audio% vorbis-audio)

  ;;; Plays an Ogg Vorbis audio stream.
  (define-element-template %vorbis-audio%
      [[path :type <string>  :label "Path"]
       [buffer   :type <integer> :label "Buffer Size (K)" :default 512]
       [loop?    :type <boolean> :label "Loop this clip?" :default #f]]
      (%audio-element%)
    (let [[path (make-native-path "LocalMedia" path)]]
      (check-file path)
      (call-5l-prim 'AudioStreamVorbis (node-full-name self)
                    (make-node-event-dispatcher self) path
                    (prop self volume) (* 1024 buffer) loop?)))
  
  ;;; Create a %vorbis-audio% element.
  (define (vorbis-audio path
                        &key (name (gensym)) (parent (default-element-parent))
                        (loop? #f) (volume 1.0))
    (create %vorbis-audio%
            :name name :parent parent :path path :loop? loop? :volume volume))


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
  (define-element-template %movie%
      [[path     :type <string>  :label "Path"]
       [volume       :type <number>  :label "Volume (0.0 to 1.0)" :default 1.0]
       [controller?  :type <boolean> :label "Has movie controller" :default #f]
       [audio-only?  :type <boolean> :label "Audio only"        :default #f]
       [loop?        :type <boolean> :label "Loop movie"        :default #f]
       [interaction? :type <boolean> :label "Allow interaction" :default #f]
       [report-captions? :type <boolean> :label "Report captions?"
                         :default #t]]
      (%widget% :shown? (or (not audio-only?) controller?))
    (define (err title msg)
      (define result
        (native-dialog title msg
                       "&Skip Movie" "E&xit Program"))
      (if (= result 1)
          (send self end-playback)
          (send self user-exit-request)))      
    (define (network-problem type)
      (err (cat "Network Movie " type)
           (cat type " loading movie.  You may want to order "
                "a CD version\nof this program; see the "
                "README.")))

    ;;; Called when the user asks to exit the script because of movie
    ;;; errors.  If you want fancier behavior, you'll need to override this
    ;;; handler.
    (on user-exit-request ()
      (exit-script))

    ;;; Called when a network timeout occurs for a streaming movie.  Override
    ;;; if you want different behavior.
    (on media-network-timeout (event)
      (network-problem "Timeout"))
    ;;; Called when a network error occurs for a streaming movie.  Override
    ;;; if you want different behavior.
    (on media-network-error (event)
      (network-problem "Error"))
    ;;; Called when a local error occurs for a movie.  Override if you want
    ;;; different behavior.
    (on media-local-error (event)
      (error (cat "Error playing movie (" path ")")))

    ;;; Set the timeout for this movie, in seconds.  A timeout of 0
    ;;; turns off timeout handling.  (Timeouts are actually pretty
    ;;; sophisticated internally; this is the nominal length of time
    ;;; the user will be asked to wait without *something* happening.
    ;;; The controller bar turns off timeouts.)
    (on set-timeout! (seconds)
      (call-5l-prim 'MovieSetTimeout (node-full-name self) seconds))

    ;; BEGIN DUPLICATE CODE - Because we don't have multiple inheritence,
    ;; the API below is shared with %media-element%.

    ;;; Pause playback.
    (on pause ()
      (media-pause self))

    ;;; Resume playback.
    (on resume ()
      (media-resume self))

    ;;; End playback. From the perspective of the WAIT function, this media
    ;;; element will skip immediately to the end of playback.
    (on end-playback ()
      (media-end-playback self))

    ;;; Set the volume of a media element.  Channels may be LEFT, RIGHT,
    ;;; ALL, or something else depending on the exact type of media being
    ;;; played.  Volume ranges from 0.0 to 1.0.
    (on set-volume! (channel volume)
      (set-media-volume! self channel volume))

    ;; END DUPLICATE CODE

    (let [[path (media-path path)]]
      (check-file path)
      (call-5l-prim 'Movie (node-full-name self)
                    (make-node-event-dispatcher self)
                    (parent->card self (prop self rect))
                    path volume
                    controller? audio-only? loop? interaction?
                    report-captions?)))

  ;;; Create a %movie%.
  (define (movie r path
                 &key (name (gensym)) (volume 1.0)
                 controller? audio-only? loop? interaction?
                 (report-captions? #t) (parent (default-element-parent)))
    (create %movie%
            :name name :parent parent :rect r :path path
            :volume volume
            :controller? controller? 
            :audio-only? audio-only?
            :loop? loop?
            :interaction? interaction?
            :report-captions? report-captions?))

  
  ;;;======================================================================
  ;;;  General Animation Support
  ;;;======================================================================

  (provide number->integer interpolate-int make-object-mover animate)

  ;;; Convert any number to an integer.  Typically needed for use with
  ;;; ANIMATE.
  (define (number->integer n)
    (inexact->exact (round n)))

  ;;; Return a value FRACTION percent of the distance between VAL1 and
  ;;; VAL2, rounded to an integer.
  (define (interpolate-int fraction val1 val2)
    (number->integer (+ val1 (* fraction (- val2 val1)))))
  
  ;;; Given x(0), x(1), dx/dt(0), and dx/dt(1), fit a cubic polynomial to
  ;;; those four constraints and evaluate it.
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

  ;;; Apply a simple-minded transform to FRACTION, making it start and stop
  ;;; either slowly or quickly, as specified.
  (define (ease-in/out fraction ease-in? ease-out?)
    (cond
     [(and ease-in? ease-out?)
      (eval-cubic-1d-spline 0 1 0 0 fraction)]
     [ease-in?
      (eval-cubic-1d-spline 0 1 0 1 fraction)]
     [ease-out?
      (eval-cubic-1d-spline 0 1 1 0 fraction)]
     [#t
      fraction]))
  
  ;;; This function creates a simple DRAW-FUNC for use with ANIMATE.  The
  ;;; resulting function moves OBJs from the point FROM to the point TO
  ;;; over the duration of the animation.
  (define (make-object-mover obj to)
    (define from (prop obj at))
    (fn (fraction)
      (set! (prop obj at)
            (point (interpolate-int fraction (point-x from) (point-x to))
                   (interpolate-int fraction (point-y from) (point-y to))))))

  ;;; Call DRAW-FUNC with numbers from 0.0 to 1.0 over the course
  ;;; MILLISECONDS.
  (define (animate milliseconds draw-func &key ease-in? ease-out?)
    (define start-time (current-milliseconds))
    (define end-time (+ start-time milliseconds))
    (draw-func 0.0)
    (let loop []
      (let [[current-time (current-milliseconds)]]
        (when (< current-time end-time)
          (let* [[elapsed-time (- current-time start-time)]
                 [fraction (/ (* 1.0 elapsed-time) milliseconds)]]
            (draw-func (ease-in/out fraction ease-in? ease-out?))
            (idle)
            (loop)))))
    (draw-func 1.0))
  
  
  ;;;======================================================================
  ;;;  State DB Debugging Support
  ;;;======================================================================

  (provide state-db-debug)

  (define-element-template %state-db-debugger% [path report-fn] ()
    (define-state-db-listener (debug state-db)
      (report-fn (state-db path))))
  
  ;;; Here's a nasty little hack for reading the state database from
  ;;; outside an element.  Calling this from anywhere but the listener is
  ;;; definitely a bug.
  (define (state-db-debug path)
    (define result #f)
    (define (set-result! value)
      (set! result value))
    (define elem
      (create %state-db-debugger%
              :parent (root-node)
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
  ;;; script data files.
  (define (script-user-data-directory)
    (call-5l-prim 'DataPath))

  ;;; An abstract superclass which implements typical GUI button behavior.
  (define-element-template %basic-button%
      [[action :type <function> :label "Click action" :default (callback)]
       [enabled? :type <boolean> :label "Enabled?" :default #t]]
      (%custom-element% :wants-cursor? enabled?)

    ;;; Draw the button in the specified state.  Valid states are DISABLED,
    ;;; NORMAL, PRESSED and ACTIVE.  Must be overridden by subclasses.
    (on draw-button (state)
      (error "draw-button must be overridden"))

    (define mouse-in-button? #f)
    (on draw ()
      (send self draw-button 
            (cond [(not enabled?) 'disabled]
                  [(not mouse-in-button?) 'normal]
                  [(mouse-grabbed-by? self) 'pressed]
                  [#t 'active])))
    (define (do-draw refresh?)
      (send self invalidate)
      (when refresh?
        (refresh)))
    
    (on prop-change (name value prev veto)
      (case name
        [[enabled?]
         (set! (prop self wants-cursor?) enabled?) 
         (do-draw #f)]
        [else (call-next-handler)]))

    (on mouse-enter (event)
      (set! mouse-in-button? #t)
      (do-draw #t))
    (on mouse-leave (event)
      (set! mouse-in-button? #f)
      (do-draw #t))
    (on mouse-down (event)
      (grab-mouse self)
      (do-draw #t))
    (on mouse-up (event)
      (define was-grabbed? #f)
      (when (mouse-grabbed-by? self)
        (set! was-grabbed? #t)
        (ungrab-mouse self))
      (do-draw #t)
      (when (and mouse-in-button? was-grabbed?)
        (send self button-clicked event)))
    (on button-clicked (event)
      ((prop self action)))
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
