;;=========================================================================
;; Random Tamale Primitives
;;=========================================================================
;; This is a collection of loosely documented and poorly-organized Tamale
;; primitives, all subject to change at a moment's notice.

(module tamale (lib "5l.ss" "5L")
  (require (lib "shapes.ss" "5L"))

  (provide draw-picture measure-picture
           set-image-cache-size! modal-input with-dc
           dc-rect color-at %element% %invisible-element%
           %zone% zone %animated-graphic% register-cursor mouse-position
           grab-mouse ungrab-mouse mouse-grabbed? mouse-grabbed-by?
           element-shown? set-element-shown?!
           delete-element delete-elements
           clear-screen point-in-poly?
           offset-rect offset-shape
           rect-horizontal-center rect-vertical-center
           rect-center move-rect-left-to move-rect-top-to
           move-rect-horizontal-center-to move-rect-vertical-center-to
           move-rect-center-to point-in-rect? center-text 
           %browser% %edit-box-element% %movie-element% %foreground-movie%
           browser edit-box %vorbis-audio% vorbis-audio
           geiger-audio set-geiger-audio-counts-per-second!
           %geiger-synth% geiger-synth
           sine-wave set-media-base-url! movie 
           movie-pause movie-resume set-media-volume!
           wait tc nap draw-line draw-box draw-box-outline inset-rect timeout
           current-card-name fade unfade save-graphics restore-graphics
           ensure-dir-exists screenshot element-exists? 
           delete-element-if-exists
           %basic-button% symcat
           state-db-debug state-db-seconds state-db-milliseconds)

  (define (url? path)
    (regexp-match "^(http|ftp|rtsp):" path))

  (define (make-path subdir path)
    (if (url? path)
        path
        (apply build-path (current-directory) subdir (regexp-split "/" path))))

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
           (lambda () body ...)
           (lambda () (call-5l-prim 'DcPop (node-full-name dc))))]))

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
      [[at :type <point> :label "Position"]] ()
    (on prop-change (name value prev veto)
      (if (eq? name 'at)
          (update-element-position self)
          (call-next-handler))))

  (define-element-template %invisible-element% [] (%element% :at (point 0 0)))

  (define-element-template %widget%
      [[rect :type <rect> :label "Rectangle"]]
      (%element% :at (rect-left-top rect)))

  (define-element-template %zone%
      [[at :new-default (point 0 0)]
       [shape :type <shape> :label "Shape"]
       [cursor :type <symbol> :default 'hand :label "Cursor"]
       [overlay? :type <boolean> :default #f :label "Has overlay?"]
       [alpha? :type <boolean> :default #f :label "Overlay transparent?"]
       [%nocreate? :type <boolean> :default #f
                   :label "Set to true if subclass creates in engine"]]
      (%element%)

    ;; Deal with our bizarre arguments.
    (define origin-rect?
      (and (rect? shape)
           (equals? (rect-left-top shape) (point 0 0))))
    (unless (or (equals? at (point 0 0)) origin-rect?)
      (error "Cannot specify AT unless SHAPE is a rectangle at 0,0: "
             (node-full-name self)))
    (define real-shape (offset-shape shape at))
    (define movable? (and overlay? origin-rect?))
               
    (on prop-change (name value prev veto)
      (case name
        [[cursor] (set-element-cursor! self value)]
        [[at]
         (unless movable?
           (veto (cat "Can't move element: " (node-full-name self))))
         (call-next-handler)]
        [else (call-next-handler)]))
    (cond
     [%nocreate?
      #f]
     [overlay?
      (call-5l-prim 'overlay (node-full-name self)
                    (parent->card self real-shape)
                    (make-node-event-dispatcher self) cursor alpha?)]
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
                &key (cursor 'hand) (overlay? #f) (alpha? #f) (at (point 0 0)))
    (create %simple-zone%
            :at at
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

  (define (element-shown? elem)
    (call-5l-prim 'ElementIsShown (node-full-name elem)))

  (define (set-element-shown?! elem show?)
    (call-5l-prim 'ElementSetShown (node-full-name elem) show?))

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
  
  (define (point-in-poly? p poly)
    (call-5l-prim 'PolygonContains poly p))

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

  (define (point-in-rect? p r)
    (and (<= (rect-left r) (point-x p))
         (<  (point-x p)   (rect-right r))
         (<= (rect-top r)  (point-y p))
         (<  (point-y p)   (rect-bottom r))))

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
      (%invisible-element%)
    (call-5l-prim 'AudioStreamGeiger (node-full-name self)
                  (build-path (current-directory) "Media" location)))

  (define (geiger-audio name location)
    (create %geiger-audio% :name name :location location))

  (define (set-geiger-audio-counts-per-second! elem-or-name counts)
    (call-5l-prim 'AudioStreamGeigerSetCps (elem-or-name-hack elem-or-name)
                  counts))

  (define-element-template %geiger-synth%
      [state-path chirp loops]
      (%invisible-element%)
    (apply call-5l-prim 'GeigerSynth (node-full-name self) state-path
           (build-path (current-directory) "Media" chirp) (* 512 1024)
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
      (%invisible-element%)
    (call-5l-prim 'AudioStreamSine (node-full-name self) frequency))

  (define (sine-wave name frequency)
    (create %sine-wave-element% :name name :frequency frequency))

  (define-element-template %vorbis-audio%
      [[location :type <string>  :label "Location"]
       [buffer   :type <integer> :label "Buffer Size (K)" :default 512]
       [loop?    :type <boolean> :label "Loop this clip?" :default #f]]
      (%invisible-element%)
    (call-5l-prim 'AudioStreamVorbis (node-full-name self)
                  (build-path (current-directory) "Media" location)
                  (* 1024 buffer) loop?))
  
  (define (vorbis-audio name location &key (loop? #f))
    (create %vorbis-audio% :name name :location location :loop? loop?))

  (define *media-base-url* #f)

  (define (set-media-base-url! url)
    (set! *media-base-url* url))

  (define (media-path location)
    ;; Find the remote version of a movie.
    ;; This will need quite a bit of rethinking.
    (if *media-base-url*
        (cat *media-base-url* "/" location)
        (make-path "Media" location)))

  (define-element-template %movie-element%
      [[location     :type <string>  :label "Location"]
       [controller?  :type <boolean> :label "Has movie controller" :default #f]
       [audio-only?  :type <boolean> :label "Audio only"        :default #f]
       [loop?        :type <boolean> :label "Loop movie"        :default #f]
       [interaction? :type <boolean> :label "Allow interaction" :default #f]]
      (%widget%)
    (let [[path (media-path location)]]
      (check-file path)
      (call-5l-prim 'movie (node-full-name self)
                    (make-node-event-dispatcher self)
                    (parent->card self (prop self rect))
                    path
                    controller? audio-only? loop? interaction?)))

  (define-element-template %foreground-movie%
      [[self-destruct? :default #t]]
      (%movie-element%)
    (set! (state-db '/sim/voiceover-playing?) #t)
    (on exit ()
      (set! (state-db '/sim/voiceover-playing?) #f))
    (on media-finished (event)
      (when self-destruct?
        (delete-element self)))
    (call-at-safe-time
     (fn ()
       (send* self 'run-actions :ignorable? #t))))

  (define (movie name r location
                 &key controller? audio-only? loop? interaction?)
    (create %movie-element%
            :name name :rect r :location location
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
  
  (define (tc arg1 &opt arg2 arg3)
    (cond
     [arg3 (+ (* (+ (* arg1 60) arg2) 30) arg3)]
     [arg2 (+ (* arg1 30) arg2)]
     [else arg1]))
  
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
  
  (define (timeout seconds card)
    (call-5l-prim 'timeout seconds (card-name card)))

  (define (current-card-name)
    (card-name (current-card)))

  (define (fade &key (ms 500))
    (clear-screen (color 0 0 0))
    (refresh :transition 'toblack :ms ms))

  (define (unfade &key (ms 500))
    (refresh :transition 'fromblack :ms ms))

  (define (save-graphics &key (bounds $screen-rect))
    (call-5l-prim 'savegraphics bounds))
  
  (define (restore-graphics &key (bounds $screen-rect))
    (call-5l-prim 'restoregraphics bounds))

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

  (define (delete-element-if-exists name
                                    &key (parent (default-element-parent)))
    (when (element-exists? name :parent parent)
      (delete-element (find-node (string->symbol (cat (node-full-name parent)
                                                      "/" name))))))
  
  (define-element-template %basic-button%
      [[action :type <function> :label "Click action"]
       [enabled? :type <boolean> :label "Enabled?" :default #t]]
      (%zone%)

    (define mouse-in-button? #f)
    (define (do-draw refresh?)
      (send self draw-button 
            (cond [(not enabled?) 'disabled]
                  [(not mouse-in-button?) 'normal]
                  [(mouse-grabbed-by? self) 'pressed]
                  [#t 'active]))
      (when refresh?
        (refresh)))
    
    (on prop-change (name value prev veto)
      (case name
        [[enabled?] (do-draw #f)]
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
      (when enabled?
        (grab-mouse self)
        (do-draw #t)))
    (on mouse-up (event)
      (define was-grabbed? #f)
      (when (mouse-grabbed-by? self)
        (set! was-grabbed? #t)
        (ungrab-mouse self))
      (do-draw #t)
      (when (and mouse-in-button? was-grabbed?)
        ((prop self action))))
    )

  (define (symcat . args)
    (string->symbol (apply cat args)))

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
  ;;  values do not use the same units as 
  ;;
  ;;  XXX - THESE SHOULD NOT USE STATE-DB-DEBUG!!! It's extremely slow.
  ;;  Ask one of the C++ programmers to add primitives which fetch
  ;;  these values,
  
  (define (state-db-seconds)
    (state-db-debug '/system/clock/seconds))

  (define (state-db-milliseconds)
    (state-db-debug '/system/clock/milliseconds))

  )