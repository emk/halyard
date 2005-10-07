;;=========================================================================
;;  Random Tamale Primitives
;;=========================================================================
;;  This is a collection of loosely documented and poorly-organized Tamale
;;  primitives, all subject to change at a moment's notice.

(module tamale (lib "lispish.ss" "5L")
  (require (lib "api.ss" "5L"))

  (provide make-path-from-abstract draw-picture measure-picture
           set-image-cache-size! modal-input with-dc
           dc-rect color-at %element% %invisible-element%
           %zone% %simple-zone% zone
           %animated-graphic% register-cursor mouse-position
           grab-mouse ungrab-mouse mouse-grabbed? mouse-grabbed-by?
           local->card
           delete-element delete-elements
           clear-screen point-in-shape? point-in-rect? point-in-poly?
           offset-rect offset-shape
           copy-rect rect-horizontal-center rect-vertical-center
           rect-center move-rect-left-to move-rect-top-to
           move-rect-horizontal-center-to move-rect-vertical-center-to
           move-rect-center-to center-text 
           %activex% activex-prop set-activex-prop!
           %flash-card%
           %browser% %edit-box-element% %movie-element%
           browser edit-box %vorbis-audio% vorbis-audio
           geiger-audio set-geiger-audio-counts-per-second!
           %geiger-synth% geiger-synth sine-wave
           media-is-installed? media-cd-is-available? search-for-media-cd
           set-media-base-url! movie 
           movie-pause movie-resume set-media-volume!
           wait tc 
           nap draw-line draw-box draw-box-outline inset-rect timeout
           current-card-name fade unfade opacity save-graphics restore-graphics
           copy-string-to-clipboard
           ensure-dir-exists screenshot element-exists? 
           delete-element-if-exists
           %basic-button%
           quicktime-component-version
           mark-unprocessed-events-as-stale!
           register-debug-report-file!
           number->integer interpolate-int make-object-mover animate
           state-db-debug state-db-seconds state-db-milliseconds
           update-state-db! inc-state-db! inc-state-db-if-true!)

  (define (url? path)
    (regexp-match "^(http|ftp|rtsp):" path))

  (define (make-path-from-abstract . args)
    (define reversed (reverse! args))
    (define abstract-component (car reversed))
    (define regular-components (reverse! (cdr reversed)))
    (apply build-path (append! regular-components
                               (regexp-split "/" abstract-component))))

  (define (make-path subdir path)
    (if (url? path)
        path
        (make-path-from-abstract (current-directory) subdir path)))

  (define (check-file path)
    (unless (or (url? path) (file-exists? path))
      (throw (cat "No such file: " path))))
  
  (define (draw-picture name p &key (subrect :rect #f))
    (let [[path (make-path "Graphics" name)]]
      (check-file path)
      (if subrect
          (call-5l-prim 'loadsubpic path p subrect)
          (call-5l-prim 'loadpic path p))))
  
  (define (measure-picture name)
    (let [[path (make-path "Graphics" name)]]
      (check-file path)
      (call-5l-prim 'MeasurePic path)))

  (define (set-image-cache-size! bytes)
    (call-5l-prim 'SetImageCacheSize bytes))

  (define (modal-input r size forecolor backcolor)
    (call-5l-prim 'input r size forecolor backcolor)
    (engine-var '_modal_input_text))

  (define-syntax with-dc
    (syntax-rules ()
      [(with-dc dc body ...)
       (dynamic-wind
           (lambda () (call-5l-prim 'DcPush (node-full-name dc)))
           (lambda () (begin/var body ...))
           (lambda () (call-5l-prim 'DcPop (node-full-name dc))))]))
  (define-syntax-indent with-dc 1)

  (define (dc-rect)
    (call-5l-prim 'DcRect))

  (define (color-at p)
    (call-5l-prim 'ColorAt p))

  (define (local->card node x)
    ;; Convert the co-ordinates of a point or shape X into card
    ;; co-ordinates.  We do this do that elements attached to elements
    ;; can interpret AT, RECT and similar parameters relative to their
    ;; parent elements.
    (if (element? node)
        (local->card (node-parent node) (offset-shape x (prop node at)))
        x))

  (define (parent->card node x)
    (local->card (node-parent node) x))

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
    (on setup-finished ()
      ;; TODO - This is technically too late to set this value, and we should
      ;; probably add a SHOWN? parameter to every object creation primitive.
      (set! (element-shown? self) shown?)))

  (define-element-template %invisible-element% []
      (%element% :at (point 0 0) :shown? #f))

  (define-element-template %audio-element%
      [[volume       :type <number> :default 1.0 :label "Volume (0.0 to 1.0)"]]
      (%invisible-element%)
    ;; I'd like put a ON PROP-CHANGE handler here for audio volume, but
    ;; it's not quite so easy, because there may be multiple channels to
    ;; content with. Ugh. See SET-MEDIA-VOLUME!.
    )

  (define-element-template %widget%
      [[rect :type <rect> :label "Rectangle"]]
      (%element% :at (rect-left-top rect)))

  (define-element-template %zone%
      [[at :new-default (point 0 0)]
       [shape :type <shape> :label "Shape"]
       [cursor :type <symbol> :default 'hand :label "Cursor"]
       [overlay? :type <boolean> :default #f :label "Has overlay?"]
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

    (on setup-finished ()
      (call-next-handler)
      ;; We need to postpone this until the underlying engine object
      ;; is created.
      (set-wants-cursor! wants-cursor?)
      (set-in-drag-layer?! dragging?))
    
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
    
    ;; The way we want zones to work is that AT represents the origin of a
    ;; zone, and the actual shape on the screen is SHAPE offset by AT. The 
    ;; problem is that this is very inconvenient for scripters, and doesn't
    ;; match the basic zone interface that we've always given them, which is 
    ;; that they just pass in the pre-offset shape on the screen. In order to 
    ;; deal with this, we normalize the parameters we are passed so that AT
    ;; represents the origin of the object and SHAPE is the shape relative to
    ;; AT. We do this as follows.
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
        (offset-shape shape at)
        ;; 3. If our AT parameter is (0,0), then we want to move our AT to the 
        ;;    origin of our shape, and then move the shape such that its origin
        ;;    is (0,0). REAL-SHAPE will be our original shape. 
        (let ((orig-shape shape))
          (set! at the-origin)
          (set! shape (offset-shape orig-shape 
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
      (call-5l-prim 'overlay (node-full-name self)
                    (parent->card self real-shape)
                    (make-node-event-dispatcher self) cursor alpha?
                    clickable-where-transparent?)]
     [else
      (call-5l-prim 'zone (node-full-name self)
                    (parent->card self (as <polygon> real-shape))
                    (make-node-event-dispatcher self) cursor)]))

  (define (animated-graphic-shape graphics)
    (define max-width 0)
    (define max-height 0)
    (foreach [graphic graphics]
      (define bounds (measure-picture graphic))
      (when (> (rect-width bounds) max-width)
        (set! max-width (rect-width bounds)))
      (when (> (rect-height bounds) max-height)
        (set! max-height (rect-height bounds))))
    (rect 0 0 max-width max-height))

  (define-element-template %animated-graphic%
      [[state-path :type <string> :label "State DB Key Path"]
       [graphics :type <list> :label "Graphics to display"]]
      (%zone%
       :shape (animated-graphic-shape graphics)
       :overlay? #t
       :%nocreate? #t)
    (call-5l-prim 'OverlayAnimated (node-full-name self)
                  (parent->card self
                                (offset-rect (prop self shape) (prop self at)))
                  (make-node-event-dispatcher self) (prop self cursor)
                  (prop self alpha?) state-path
                  (map (fn (p) (make-path "Graphics" p)) graphics)))

  (define-element-template %simple-zone% [action] (%zone%)
    (on prop-change (name value prev veto)
      (case name
        [[action] (void)]
        [else (call-next-handler)]))
    (on mouse-down (event)
      (action)))

  (define (zone name shape action
                &key (cursor 'hand) (overlay? #f) (alpha? #f))
    (create %simple-zone%
            :name name 
            :shape shape
            :cursor cursor
            :action action
            :overlay? overlay?
            :alpha? alpha?))
  
  (define (update-element-position elem)
    ;; Don't make me public.
    (call-5l-prim 'MoveElementTo (node-full-name elem)
                  (parent->card elem (prop elem at)))
    (foreach [e (node-elements elem)]
      (update-element-position e)))

  (define (set-element-cursor! elem cursor)
    ;; Don't make me public.
    (call-5l-prim 'SetZoneCursor (node-full-name elem) cursor))

  (define (register-cursor sym filename &key (hotspot (point -1 -1)))
    (let [[path (make-path "Graphics" (cat "cursors/" filename))]]
      (check-file path)
      (call-5l-prim 'RegisterCursor sym path hotspot)))

  (define (mouse-position)
    ;; XXX - This keeps returning exciting results even if we're in the
    ;; background.  Yuck.
    (call-5l-prim 'MousePosition))

  (define (set-element-shown?! elem show?)
    ;; Not all subclasses of %element% actually have a corresponding
    ;; engine object.
    (when (element-exists-in-engine? elem)
      (call-5l-prim 'ElementSetShown (node-full-name elem) show?)))

  (define (delete-element elem-or-name)
    ;; TODO - Get rid of elem-or-name-hack, and rename
    ;; delete-element-internal to delete-element.
    (delete-element-internal (find-node (elem-or-name-hack elem-or-name))))
  
  (define (delete-elements
           &opt (elems-or-names (node-elements (current-card))))
    (foreach [elem elems-or-names]
      (delete-element elem)))

  (define (grab-mouse elem)
    (assert (instance-of? elem <element>))
    (call-5l-prim 'MouseGrab (node-full-name elem)))

  (define (ungrab-mouse elem)
    (assert (instance-of? elem <element>))
    (call-5l-prim 'MouseUngrab (node-full-name elem)))

  (define (mouse-grabbed?)
    (call-5l-prim 'MouseIsGrabbed))
  
  (define (mouse-grabbed-by? elem)
    (call-5l-prim 'MouseIsGrabbedBy (node-full-name elem)))

  (define (clear-screen c)
    (call-5l-prim 'screen c))

  (defgeneric (point-in-shape? (p <point>) (shape <shape>)))
  
  (defmethod (point-in-shape? (p <point>) (r <rect>))
    (and (<= (rect-left r) (point-x p))
         (<  (point-x p)   (rect-right r))
         (<= (rect-top r)  (point-y p))
         (<  (point-y p)   (rect-bottom r))))

  (defmethod (point-in-shape? (p <point>) (poly <polygon>))
    (call-5l-prim 'PolygonContains poly p))
        
  ;; XXX - Backwards compatibility glue.
  (define point-in-rect? point-in-shape?)
  (define point-in-poly? point-in-shape?)
        
  (define (offset-rect r p)
    (rect (+ (rect-left r) (point-x p))
          (+ (rect-top r) (point-y p))
          (+ (rect-right r) (point-x p))
          (+ (rect-bottom r) (point-y p))))

  (define (offset-shape s p)
    (cond
     [(point? s) (point-offset s p)]
     [(rect? s) (offset-rect s p)]
     [(polygon? s) (apply polygon (map (fn (v) (point-offset v p))
                                       (polygon-vertices s)))]
     [else
      (error (cat "Don't know how to offset " s))]))

  (define (center-text stylesheet box msg &key (axis 'both))
    (define bounds (measure-text stylesheet msg :max-width (rect-width box)))
    (define r
      (case axis
        [[both]
         (move-rect-center-to bounds (rect-center box))]
        [[y]
         (move-rect-left-to
          (move-rect-vertical-center-to bounds (rect-vertical-center box))
          (rect-left box))]
        [[x]
         (move-rect-top-to
          (move-rect-horizontal-center-to bounds (rect-horizontal-center box))
          (rect-top box))]
        [else
         (throw (cat "center-text: Unknown centering axis: " axis))]))
    (draw-text stylesheet r msg))

  (define-element-template %activex%
      [[activex-id :type <string>]]
      (%widget%)
    (call-5l-prim 'ActiveX (node-full-name self) 
                  (make-node-event-dispatcher self)
                  (parent->card self (prop self rect))
                  activex-id))

  (define (activex-prop elem prop)
    (call-5l-prim 'ActiveXPropGet (node-full-name elem) prop))

  (define (set-activex-prop! elem prop value)
    (call-5l-prim 'ActiveXPropSet (node-full-name elem) prop value))

  (define-card-template %flash-card%
      [[location :type <string> :label "Location"]]
      ()
    (define flash
      (create %activex%
              :name 'flash
              :rect $screen-rect
              :activex-id "ShockwaveFlash.ShockwaveFlash"))
    (set! (activex-prop flash "movie")
          (build-path (current-directory) "Flash" location)))
  
  (define-element-template %browser%
      [[location :type <string> :label "Location" :default "about:blank"]
       [fallback? :type <boolean> :label "Use primitive fallback web browser?"
                  :default #f]]
      (%widget%)

    (on load-page (page)
      (let [[path (make-path "HTML" page)]]
        (check-file path)
        (call-5l-prim 'BrowserLoadPage (node-full-name self) path)))

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

    (on back ()
      (call-5l-prim 'BrowserBack (node-full-name self)))
    (on forward ()
      (call-5l-prim 'BrowserForward (node-full-name self)))
    (on reload ()
      (call-5l-prim 'BrowserReload (node-full-name self)))
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
    (send self load-page location))

  (define (browser name r location)
    (create %browser% :name name :rect r :location location))

  (define-element-template %edit-box-element%
      [[text :type <string> :label "Initial text"]
       [font-size :type <integer> :label "Font size"]
       [multiline? :type <boolean> :label "Allow multiple lines?"]]
      (%widget%)
    (call-5l-prim 'editbox (node-full-name self)
                  (parent->card self (prop self rect)) text
                  font-size multiline?))

  (define (edit-box name r text &key (font-size 9) (multiline? #f))
    (create %edit-box-element% :name name :rect r :text text
            :font-size font-size :multiline? multiline?))

  (define-element-template %geiger-audio%
      [[location :type <string> :label "Location"]]
      (%audio-element%)
    (call-5l-prim 'AudioStreamGeiger (node-full-name self)
                  (make-node-event-dispatcher self)
                  (build-path (current-directory) "Media" location)
                  (prop self volume)))

  (define (geiger-audio name location &key (volume 1.0))
    (create %geiger-audio% :name name :location location :volume volume))

  (define (set-geiger-audio-counts-per-second! elem-or-name counts)
    (call-5l-prim 'AudioStreamGeigerSetCps (elem-or-name-hack elem-or-name)
                  counts))

  (define-element-template %geiger-synth%
      [state-path chirp loops]
      (%audio-element%)
    (apply call-5l-prim 'GeigerSynth (node-full-name self) state-path
           (build-path (current-directory) "Media" chirp)
           (prop self volume)
           (* 512 1024)
           (map (fn (item)
                  (if (string? item)
                      (build-path (current-directory) "Media" item)
                      item))
                loops)))

  (define (geiger-synth name state-path chirp . loops)
    (create %geiger-synth%
            :name name :state-path state-path
            :chirp chirp :loops loops))

  (define-element-template %sine-wave-element%
      [[frequency :type <integer> :label "Frequency (Hz)"]]
      (%audio-element%)
    (call-5l-prim 'AudioStreamSine (node-full-name self)
                  (make-node-event-dispatcher self)
                  (prop self volume) frequency))

  (define (sine-wave name frequency &key (volume 1.0))
    (create %sine-wave-element%
            :name name :frequency frequency :volume volume))

  (define-element-template %vorbis-audio%
      [[location :type <string>  :label "Location"]
       [buffer   :type <integer> :label "Buffer Size (K)" :default 512]
       [loop?    :type <boolean> :label "Loop this clip?" :default #f]]
      (%audio-element%)
    (let [[path (build-path (current-directory) "Media" location)]]
      (check-file path)
      (call-5l-prim 'AudioStreamVorbis (node-full-name self)
                    (make-node-event-dispatcher self) path
                    (prop self volume) (* 1024 buffer) loop?)))
  
  (define (vorbis-audio name location &key (loop? #f) (volume 1.0))
    (create %vorbis-audio%
            :name name :location location :loop? loop? :volume volume))

  (define *cd-media-directory* #f)

  (define (media-is-installed?)
    (directory-exists? (build-path (current-directory) "Media")))

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
        (define file (make-path-from-abstract candidate pathname))
        (when (file-exists? file)
          (set! *cd-media-directory* candidate)
          (return)))))

  (define *media-base-url* #f)

  (define (set-media-base-url! url)
    (set! *media-base-url* url))

  (define (media-path location)
    ;; Create some of the paths we'll check.
    (define hd-path (make-path "Media" location))
    (define cd-path
      (if (media-cd-is-available?)
          (make-path-from-abstract *cd-media-directory* location)
          #f))

    (cond
     ;; Pass explicit URLs straight through.
     [(url? location)
      location]
     ;; Otherwise, first check our media directory for the file.
     [(file-exists? hd-path)
      hd-path]
     ;; Then check the CD, if we have one.
     [(and cd-path (file-exists? cd-path))
      cd-path]
     ;; If all else fails, and we've been told about a server, assume our
     ;; media is there.
     [*media-base-url*
      (cat *media-base-url* "/" location)]
     ;; OK, we give up.
     [#t
      (error (cat "Cannot find file " location))]))

  (define-element-template %movie-element%
      [[location     :type <string>  :label "Location"]
       [volume       :type <number>  :label "Volume (0.0 to 1.0)" :default 1.0]
       [controller?  :type <boolean> :label "Has movie controller" :default #f]
       [audio-only?  :type <boolean> :label "Audio only"        :default #f]
       [loop?        :type <boolean> :label "Loop movie"        :default #f]
       [interaction? :type <boolean> :label "Allow interaction" :default #f]]
      (%widget% :shown? (or (not audio-only?) controller?))
    (let [[path (media-path location)]]
      (check-file path)
      (call-5l-prim 'movie (node-full-name self)
                    (make-node-event-dispatcher self)
                    (parent->card self (prop self rect))
                    path volume
                    controller? audio-only? loop? interaction?)))

  (define (movie name r location
                 &key (volume 1.0) controller? audio-only? loop? interaction?)
    (create %movie-element%
            :name name :rect r :location location
            :volume volume
            :controller? controller? 
            :audio-only? audio-only?
            :loop? loop?
            :interaction? interaction?))

  ;; Note: these functions may not be happy if the underlying movie code
  ;; doesn't like to be paused.
  (define (movie-pause elem-or-name)
    (call-5l-prim 'moviepause (elem-or-name-hack elem-or-name)))

  (define (movie-resume elem-or-name)
    (call-5l-prim 'movieresume (elem-or-name-hack elem-or-name)))
  
  (define (set-media-volume! elem-or-name channel volume)
    (call-5l-prim 'MediaSetVolume (elem-or-name-hack elem-or-name)
                  channel volume))

  (define (wait elem-or-name &key frame)
    (when (or (not (symbol? elem-or-name)) (element-exists? elem-or-name))
      (if frame
          (call-5l-prim 'wait (elem-or-name-hack elem-or-name) frame)
          (call-5l-prim 'wait (elem-or-name-hack elem-or-name)))))
  
  ;; what is 'tc' short for?
  (define (tc arg1 &opt arg2 arg3)
    (cond
     [arg3 (+ (* (+ (* arg1 60) arg2) 30) arg3)]
     [arg2 (+ (* arg1 30) arg2)]
     [else arg1]))
  
  ;; XXX - Reimplement this function or get rid of it; the current
  ;; implementation is *horrible*.
  (define (nap tenths)
    (call-5l-prim 'nap tenths))

  (define (draw-line from to c width)
    (call-5l-prim 'drawline from to c width))

  (define (draw-box r c)
    (call-5l-prim 'drawboxfill r c))

  (define (draw-box-outline r c width)
    (call-5l-prim 'drawboxoutline r c width))
  
  (define (inset-rect r pixels)
    ;; TODO - Rename foo-offset to offset-foo.
    (rect (+ (rect-left r) pixels)
          (+ (rect-top r) pixels)
          (- (rect-right r) pixels)
          (- (rect-bottom r) pixels)))
  
  ;; XXX - The implementation of this function is exceedingly ugly,
  ;; and I'm not even sure it works.
  (define (timeout seconds card)
    (call-5l-prim 'timeout seconds (card-name card)))

  (define (current-card-name)
    (card-name (current-card)))

  (define (fade &key (ms 500))
    (clear-screen (color 0 0 0))
    (refresh :transition 'toblack :ms ms))

  (define (unfade &key (ms 500))
    (refresh :transition 'fromblack :ms ms))

  (define (opacity initial-color opacity-value)
    (color (color-red initial-color) (color-green initial-color) (color-blue initial-color) opacity-value)
    )

  ;;Save and Restore graphics deprecated, but are still being used by Widgets.ss
  (define (save-graphics &key (bounds $screen-rect))
    (call-5l-prim 'savegraphics bounds))
  
  (define (restore-graphics &key (bounds $screen-rect))
    (call-5l-prim 'restoregraphics bounds))
        
  ;; Copy a string to the clip board
  (define (copy-string-to-clipboard string)
    (call-5l-prim 'copystringtoclipboard string))

  (define (three-char-print n)
    (cond 
     ((> n 999) "000")
     ((> n 99) (format "~a" n))
     ((> n 9) (format "0~a" n))
     ((> n -1) (format "00~a" n))
     (else "000")))

  (define (ensure-dir-exists name)
    (define dir (build-path (current-directory) name))
    (when (not (directory-exists? dir))
      (make-directory dir))
    dir)
  
  (define (screenshot)
    (define dir (ensure-dir-exists "Screenshots"))
    (call-5l-prim 
     'screenshot 
     (let loop ((count 0))
       (define path (build-path dir (cat (three-char-print count) ".png")))
       (cond
        ((= count 1000) path)
        ((or (file-exists? path) (directory-exists? path))
         (loop (+ count 1)))
        (else path)))))

  (define (element-exists? name &key (parent (default-element-parent)))
    (and (memq name (map node-name (node-elements parent)))
         #t))

  (define (element-exists-in-engine? elem)
    (call-5l-prim 'ElementExists (node-full-name elem)))
    
  (define (delete-element-if-exists name
                                    &key (parent (default-element-parent)))
    (when (element-exists? name :parent parent)
      (delete-element (find-node (string->symbol (cat (node-full-name parent)
                                                      "/" name))))))
  
  (define-element-template %basic-button%
      [[action :type <function> :label "Click action" :default (callback)]
       [enabled? :type <boolean> :label "Enabled?" :default #t]]
      (%zone% :wants-cursor? enabled?)

    (define mouse-in-button? #f)
    (on draw ()
      (send self draw-button 
            (cond [(not enabled?) 'disabled]
                  [(not mouse-in-button?) 'normal]
                  [(mouse-grabbed-by? self) 'pressed]
                  [#t 'active])))
    (define (do-draw refresh?)
      (send self draw)
      (when refresh?
        (refresh)))
    
    (on prop-change (name value prev veto)
      (case name
        [[enabled?]
         (set! (prop self wants-cursor?) enabled?) 
         (do-draw #f)]
        [else (call-next-handler)]))

    (on setup-finished ()
      (call-next-handler)
      (do-draw #f))
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

  (define (mark-unprocessed-events-as-stale!)
    (call-5l-prim 'MarkUnprocessedEventsAsStale))

  ;;; Register a file to be included in any debug (i.e., crash) reports
  ;;; generated for either the script or the engine itself.
  ;;;
  ;;; The description should be a short lowercase string.  Don't bother
  ;;; to capitalize the first word, or your description will look funny
  ;;; in the list of descriptions.
  (define (register-debug-report-file! file description)
    (let [[path (make-path-from-abstract (current-directory) file)]]
      (call-5l-prim 'DebugReportAddFile path description)))

  
  ;;-----------------------------------------------------------------------
  ;;  Animation
  ;;-----------------------------------------------------------------------

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
  
  
  ;;-----------------------------------------------------------------------
  ;;  State DB Debugging Support
  ;;-----------------------------------------------------------------------
  ;;  Here's a nasty little hack for reading the state database from
  ;;  outside an element.  Calling this from anywhere but the listener
  ;;  is definitely a bug.

  (define-element-template %state-db-debugger% [path report-fn] ()
    (define-state-db-listener (debug state-db)
      (report-fn (state-db path))))
    
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

  
  ;;-----------------------------------------------------------------------
  ;;  State DB Time Support
  ;;-----------------------------------------------------------------------
  ;;  The state-db's /system/clock/seconds and /system/clock/milliseconds
  ;;  values do not use the same units as CURRENT-MILLISECONDS.
  ;;
  ;;  XXX - THESE SHOULD NOT USE STATE-DB-DEBUG!!! It's extremely slow.
  ;;  Ask one of the C++ programmers to add primitives which fetch
  ;;  these values.
  
  (define (state-db-seconds)
    (state-db-debug '/system/clock/seconds))

  (define (state-db-milliseconds)
    (state-db-debug '/system/clock/milliseconds))

  
  ;;-----------------------------------------------------------------------
  ;;  State DB Update Support
  ;;-----------------------------------------------------------------------
  ;;  XXX - THIS SHOULD NOT USE STATE-DB-DEBUG!!! It's extremely slow.
  ;;  Ask one of the C++ programmers to add a primitive to do this.
  
  ;;; Apply FUNC 
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
  )