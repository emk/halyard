(module features (lib "5l.ss" "5L")
  ;;(require (lib "drag.ss" "5L"))
  ;;(require (lib "q-and-a.ss" "5L"))
  ;;(require (lib "animate.ss" "5L"))
  (require (file "base.ss"))
  ;;(require (lib "deprecated.ss" "5L"))

  (sequence features)
  
  (card features/controls (%standard-test-card% :title "Controls")
    (text description 
        ((below (.title-elem) 20) $text16 
         "We have fairly straight-forward access to a variety of native controls for use in 5L programs.  In most cases, we can change the color and the borders.  In addition to the controls shown here, we can have list boxes and drop-down edit fields.  Except for the HTML control, most of these should be 508-compliant with minimal work."
         :max-width 310))
    (edit-box multi-line
        ((rect 330 10 790 110) 
         "Multiple, multi-line edit fields.  You should be able to tab between them, but I'm not sure that works.  Word wrap works fine."
         :multiline? #t 
         :font-size 18))
    (edit-box single-line
        ((rect 330 120 790 220) "Hello!  Single-line."))
    (browser browser ((rect 10 230 790 590) "sample.html"))
    )

  (card features/text-formatting
      (%standard-test-card% :title "Text Formatting")
    (text format-demo
        ((point 10 100) $text16
         "We support <b>bold</b>, <i>italic</i> and <h>highlighted</h> text.  We automatically insert \"smart quotes\"--em-dashes--and ellipses...  We also support XML entities: &lt;&gt;&quot;&apos;&amp; &copy;&reg;&dagger;&micro;&gamma;&lambda;&sup3; &frac34;&plusmn;&there4;"
         :max-width 780))
    (text escape-demo
        ((point 10 200) $text16
         (string->xml "<i>Escaped XML & other goodness</i>")))
    )

  (define-class %edit-box-demo% (%edit-box%)
    (default font-size 18))

  (define-class %text-input-display% (%text-box%)
    (value shape (rect 0 0 200 20))
    (value style $caption-style)
    (value text ""))
  
  (card features/text-input
      (%standard-test-card% :title "Text Input")

    ;; Column 1.
    (elem edit
        (%edit-box-demo%
         :rect (move-rect-left-top-to (rect 0 0 200 32)
                                      (below (.title-elem) 20))
         :text "Hello!")
      (def (update-display display)
        (set! (display .text) (string->xml (.text))))
      ;; This gets run whenever our contents change (even if we change them
      ;; with SET-TEXT!).
      (def (text-changed event)
        (.update-display ((.parent) .display)))
      ;; This gets run whenever the user presses Enter in a text box.
      (def (text-enter event)
        (.update-display ((.parent) .display2))))

    (elem display (%text-input-display% :at (below (.edit) 10)))
    (elem display2 (%text-input-display% :at (below (.display) 10)))

    ;; Column 2.
    (elem edit2 
        (%edit-box-demo%
         :rect (move-rect-left-top-to (rect 0 0 200 32)
                                      (to-the-right-of (.edit) 20))))

    (elem set-text-button
        (%text-button% :at (below (.edit2) 10) :label "Set Text"
                       :command 'set-text))
    (def (set-text)
      (set! ((.edit2) .text) "New Text"))

    (text-button focus-button
        ((below (.set-text-button) 10) "Focus" :command 'focus))
    (def (focus)
      ((.edit2) .focus))

    (text-button set-point-button
        ((below (.focus-button) 10) "Set Insertion Point" :command 'set-point))
    (def (set-point)
      (set! ((.edit2) .insertion-point) -1))
    
    (text-button set-selection-button
        ((below (.set-point-button) 10) "Set Selection"
         :command 'set-selection))
    (def (set-selection)
      ((.edit2) .set-selection! 0 -1))

    (run
      ;; Focus the first edit box in the line.
      ((.edit) .focus))
    )

  
  ;;=======================================================================
  ;;  Browser
  ;;=======================================================================
  
  (define (browser-toolbar-button-shape graphic-prefix)
    (measure-graphic (cat "browser/" graphic-prefix "-normal.png")))
  
  (define-class %browser-toolbar-button% (%basic-button%)
    (attr graphic-prefix :type <symbol> :label "Base name for graphics")
    (value alpha? #t)
    (value shape (browser-toolbar-button-shape (.graphic-prefix)))
    
    (def (draw)
      (draw-graphic (point 0 0)
                    (cat "browser/" (.graphic-prefix) "-" (.button-state) 
                         ".png")))
    )
  
  (define-class %browser-toolbar-command-button% (%browser-toolbar-button%)
    (attr browser-command :type <symbol>)
      
    (value enabled? #f)
    (value graphic-prefix (.browser-command))
    (value action (callback (@browser-elem .send (.browser-command) '()))))
  
  (define-stylesheet $browser-style :base $base-style :family "Times")
  
  (card features/browser ()
    (setup
      (draw-default-background))
    
    (def (draw-string r str)
      (draw-rectangle r $color-black)
      (draw-text (inset-rect r 2) $browser-style (string->xml str)))
    
    (def (draw-url url)
      (.draw-string (rect 0 50 800 70) url))
    
    (def (draw-title title)
      (.draw-string (rect 0 30 800 50) title))
    
    (def (draw-status-text msg)
      (.draw-string (rect 0 580 600 600) msg))
    
    (def (draw-progress-bar done? value)
      (define r (rect 600 580 800 600))
      (draw-rectangle r $color-black)
      (unless done?
        (let [[right (+ (rect-left r)
                        (inexact->exact (round (* value (rect-width r)))))]]
          (draw-rectangle (rect (rect-left r) (rect-top r)
                                right (rect-bottom r))
                          (color #x0 #x0 #x80)))))
    
    (def (browser-navigate event)
      (if (equal? (event-url event) "http://www.nowhere.org/")
        (begin
          (non-fatal-error (cat "Access to " (event-url event)
                                " is restricted.  Sorry!"))
          (veto-event! event))
        (super)))
    
    (def (browser-page-changed event)
      (.draw-url (event-url event)))
    
    (def (browser-title-changed event)
      (.draw-title (event-text event)))
    
    (def (status-text-changed event)
      (.draw-status-text (event-text event)))
    
    (def (progress-changed event)
      (.draw-progress-bar (event-progress-done? event)
                          (event-progress-value event)))
    
    (def (update-ui event)
      (define command (event-command event))
      (case command
        [[back forward reload stop]
         (set! ((@* (symbol->string command)) .enabled?)
               (@browser-elem .command-enabled? command))]))
    
    (elem back (%browser-toolbar-command-button%
                :at (point 0 0)
                :browser-command 'back))
    (elem forward (%browser-toolbar-command-button%
                   :at (point 100 0)
                   :browser-command 'forward))
    (elem reload (%browser-toolbar-command-button%
                  :at (point 200 0)
                  :browser-command 'reload))
    (elem stop (%browser-toolbar-command-button%
                :at (point 300 0)
                :browser-command 'stop))
    (elem home (%browser-toolbar-button%
                :at (point 400 0)
                :graphic-prefix 'home
                :action (callback (@browser-elem .load-page "sample.html"))))
    
    (elem browser-elem (%browser%
                        :rect (rect 0 70 800 580)
                        :path "sample.html"
                        :fallback? #f))
    )

  
  ;;=======================================================================
  ;;  External Browser
  ;;=======================================================================
  
  (card features/launch-browser (%standard-test-card% :title "Launch Browser")
    (text-button open-button
                 ((below (.title-elem) 20) 
                  "Open Default Browser"
                  :action
                  (callback
                   (set! (@result .text)
                         (if (open-in-browser "http://iml.dartmouth.edu")
                             "Success."
                             "Error opening URL.")))))
    (text-box result
              ((move-rect-left-top-to (rect 0 0 200 20) 
                                      (below (.open-button) 10))
               $caption-style 
               ""))
    )

  
  ;;=======================================================================
  ;;  ActiveX
  ;;=======================================================================
  
  (card features/activex (%standard-test-card% :title "ActiveX")
    (elem cartoon (%activex%
                   :rect (rect 100 100 700 450)
                   :activex-id "ShockwaveFlash.ShockwaveFlash"))
    (run
      (@cartoon .set-activex-prop! "movie"
                "http://mirrors.creativecommons.org/reticulum_rex/cc.remixculture.101906.swf")
    ))
  
  
  ;;=======================================================================
  ;;  Transparency
  ;;=======================================================================

  (card features/transparency (%standard-test-card% :title "Tranparency")
    (setup 
      (draw-graphic (point 0 0) "trans/aud_on.png") ; 1-bit mask
      (draw-graphic (point 0 10) "trans/base.png") ; full alpha channel
      (draw-graphic (point 0 0) "trans/funky.png") ; complicated transparency
      (draw-graphic (point 0 0) "trans/grey.png") ; Simple 50% black
      (draw-graphic (point 400 100) "trans/alphacorner.png") ; Test cases for bug
      (draw-graphic (point 400 0) "trans/alphatop.png")
      (draw-rectangle (rect 75 125 150 200) (color #x00 #xFF #xFF #x80))
      (draw-rectangle (rect 100 150 175 225) (color #xFF #x00 #xFF #x80))
      (draw-rectangle (rect 50 150 125 225) (color #xFF #xFF #x00 #x80))
      (draw-rectangle (rect 200 200 300 300) ; This tests the default, which is 
                      (color #x00 #xFF #x00)) ; fully opaque.
    ))


  ;;=======================================================================
  ;;  Transitions
  ;;=======================================================================

  (sequence features/transitions)

  (define (transition-text msg)
    (define t (new-text (point 0 0) $transition-style msg))
    (t .center-on-parent!))

  (define (show-trans msg trans)
    (draw-white-background)
    (transition-text msg)
    (refresh :transition trans :ms 1000))

  (card features/transitions/crossfade () 
    (run (show-trans "Crossfade"  'crossfade)))
  (card features/transitions/wipeleft ()  
    (run (show-trans "Wipe Left"  'wipeleft)))
  (card features/transitions/wiperight () 
    (run (show-trans "Wipe Right" 'wiperight)))
  (card features/transitions/wipeup ()    
    (run (show-trans "Wipe\nUp"   'wipeup)))
  (card features/transitions/wipedown ()  
    (run (show-trans "Wipe\nDown" 'wipedown)))
  (card features/transitions/pushleft ()  
    (run (show-trans "Push Left"  'pushleft)))
  (card features/transitions/pushright () 
    (run (show-trans "Push Right" 'pushright)))
  (card features/transitions/pushup ()    
    (run (show-trans "Push\nUp"   'pushup)))
  (card features/transitions/pushdown ()  
    (run (show-trans "Push\nDown" 'pushdown)))
  (card features/transitions/toblack ()   
    (run (show-trans "To Black"   'toblack)))
  (card features/transitions/fromblack () 
    (run (show-trans "From Black" 'fromblack)))


  ;;=======================================================================
  ;;  Templates and Events
  ;;=======================================================================

  (define (point->boring-button-rectangle text)
    (offset-rect
     (inset-rect (measure-text $login-button-style text) -5)
     (point 5 5)))

  (define-class %boring-button% (%custom-element%)
    (attr text) 
    (attr action)
    (value shape (point->boring-button-rectangle (string->xml (.text))))
    
    (def (draw)
      (draw-rectangle (dc-rect) $color-white)
      (draw-text (inset-rect (dc-rect) 5) $login-button-style
                 (string->xml (.text))))
    (def (mouse-down event)
      ((.action))))

  (define-class %click-me-button% (%basic-button%)    
    (value action (callback (jump @index)) :alpha? #t)
    (def (draw)
      (draw-graphic (point 0 0)
                    (case (.button-state)
                      [[normal]   "click-me-normal.png"]
                      [[active]   "click-me-active.png"]
                      [[pressed]  "click-me-pressed.png"]
                      [[disabled] "click-me-disabled.png"])))
    )

  (card features/templates-events
      (%standard-test-card% :title "Templates & Events")
    (elem boring-button (%boring-button%
                         :at (rect-center $screen-rect)
                         :text "Click Me"
                         :action (callback (jump @index))))
    (elem click-1 (%click-me-button% :bounds (rect 100 100 176 126)))
    (elem click-2 (%click-me-button% :bounds (rect 100 130 176 156)))
    (elem click-3 (%click-me-button% :bounds (rect 100 160 176 186)))
    (elem click-4 (%click-me-button% :bounds (rect 100 190 176 216)))
    )

  ;;=======================================================================
  ;;  Widgets, zones, etc.
  ;;=======================================================================

  ;; Testing polygonal Zones.
  (card features/zones (%standard-test-card% :title "Clickable Zones")
    (clickable-zone rect-1 ((rect 5 5 5 5) (callback (jump @index))))
    (clickable-zone rect-2 ((rect 10 10 20 20) (callback (jump @index))))
    (clickable-zone rect-3 ((rect 30 30 30 40) (callback (jump @index))))
    (clickable-zone rect-4 ((rect 10 30 20 31) (callback (jump @index))))
    (clickable-zone triangle ((polygon (point 200 200)
                                       (point 250 100)
                                       (point 300 200)) 
                              (callback (jump @index))))
    (clickable-zone star ((polygon (point 450 300)
                                   (point 450 0)
                                   (point 650 300)
                                   (point 350 150)
                                   (point 650 0))
                          (callback (jump @index))))
    )

  (card features/overlays (%standard-test-card% :title "Overlays")
    (clickable-zone overlay1 ((rect 10 10 500 100)
                              (callback (delete-element @overlay1))
                              :overlay? #t)
      (def (draw)
        (clear-dc $color-white)
        (draw-text (dc-rect) $login-button-style 
                   "Overlay.  Click to delete.")))
    
    (clickable-zone lens ((rect 10 100 135 225)
                          (callback (delete-element @lens))
                          :overlay? #t :alpha? #t)
      (def (draw) (draw-graphic (point 0 0) "lens.png")))
    
    (clickable-zone overlay2 ((rect 10 110 500 200)
                              (callback (delete-element @overlay2))
                              :overlay? #t :alpha? #t)
      (def (draw)
        (clear-dc (color #x80 #x00 #x00 #x40))
        (draw-text (rect 0 0 490 45) $caption-style "Transparent overlay.")
        (draw-graphic (point 330 -10) "lens.png")
        (draw-rectangle (rect 400 0 490 90) (color #x00 #xFF #x00 #x30))))
    
    (setup
      (draw-rectangle (rect 0 155 65 250) $color-white)
      (draw-text (rect 10 155 500 200) $caption-style "Covered text.")))

  (define (movable-lens-shape p)
    (rect (point-x p) (point-y p) (+ 125 (point-x p)) (+ 125 (point-y p))))

  (require (lib "initialize-slot.ss" "5L"))
  
  (define-class %movable-lens% (%custom-element%)
    (value alpha? #t) 
    (value shape (measure-graphic "lens.png"))
    
    (.initialize-slot 'offset-x 0)
    (.initialize-slot 'offset-y 0)
    
    (def (apply-drag-offset p)
      (point (+ (point-x p) (slot 'offset-x)) (+ (point-y p) (slot 'offset-y))))
    (def (mouse-down event)
      (define p (event-position event))
      (set! (slot 'offset-x) (- (point-x (.at)) (point-x p)))
      (set! (slot 'offset-y) (- (point-y (.at)) (point-y p)))
      (grab-mouse self))
    (def (mouse-moved event)
      (when (mouse-grabbed-by? self)
        (set! (.at) (.apply-drag-offset (event-position event)))))
    (def (mouse-up event)
      (when (mouse-grabbed-by? self)
        (ungrab-mouse self)))
    (def (draw)
      (draw-graphic (point 0 0) "lens.png")))

  (card features/dragndrop ()
    (text-box title ($screen-rect $audio-stream-style "Simple\nDrag and Drop"))
    (elem lens (%movable-lens% :at (point 350 300)))
    (setup 
      (draw-white-background)))

  (card features/geometric ()
    (setup
      (draw-white-background)
      (draw-line (point 10 10) (point 10 50) $color-black 1) 
      (draw-line (point 20 10) (point 20 50) $color-black 2) 
      (draw-line (point 30 10) (point 30 50) $color-black 3) 
      
      (draw-line (point 10 60) (point 50 60) $color-black 1) 
      (draw-line (point 10 70) (point 50 70) $color-black 2) 
      (draw-line (point 10 80) (point 50 80) $color-black 3) 
      
      (draw-rectangle-outline (rect 100 100 200 200) (color 0 0 0 128) 2))
    )


  ;;=======================================================================
  ;;  Masking and Erasing
  ;;=======================================================================

  (define (graphic-center-offset path)
    (define bounds (measure-graphic path))
    (point (/ (rect-width bounds) -2) (/ (rect-height bounds) -2)))

  (sequence features/mask)

  (card features/mask/light ()
    (elem light (%custom-element%
                 :at (point 0 0)
                 :shape (measure-graphic "mask/mask.png")
                 :alpha? #t)
      (after-updating at (.invalidate))
      (def (draw)
        (draw-graphic (point (- (point-x (.at))) (- (point-y (.at))))
                      "mask/blend-foreground.png")
        (mask (point 0 0) "mask/mask.png")))
      
    (setup
      (draw-graphic (point 0 0) "mask/blend-background.png"))
      
    (def (mouse-moved event)
      (define offset (graphic-center-offset "mask/mask.png"))
      (define at (offset-by-point (event-position event) offset))
      (set! ((.light) .at) at)
      (refresh))
    )

  (define-class %erasable% (%custom-element%)
    (value at (point 0 0)) 
    (value shape $screen-rect) 
    (value alpha? #t)
    (value clickable-where-transparent? #t)
    
    (def (draw)
      (clear-dc (color 0 0 0 48))
      (draw-graphic (point 0 0) "mask/blend-foreground.png"))
    
    (def (mouse-moved event)
      (define offset (graphic-center-offset "mask/eraser.png"))
      (define at (offset-by-point (event-position event) offset))
      (with-dc self
        (mask at "mask/eraser.png"))))

  (card features/mask/eraser ()
    (elem erasable (%erasable%))
    (setup
      (draw-graphic (point 0 0) "mask/blend-background.png")))


  ;;=======================================================================
  ;;  Custom cursors
  ;;=======================================================================

  (define-class %cursor-test-rect% (%rectangle%)
    (value bounds (rect 100 100 300 500))
    (value color $color-white)
    (value wants-cursor? #t)
    (value cursor 'lens)
    
    (def (mouse-down event)
      (if (eq? (.cursor) 'lens)
        (set! (.cursor) 'hand)
        (set! (.cursor) 'lens))))
  
  (define-class %deleting-test-rect% (%rectangle%)
    (value bounds (rect 400 100 600 500))
    (value color $color-white)
    (value wants-cursor? #t)
    (value cursor 'lens)
    
    (def (mouse-down event)
      (if (element-exists? 'lens)
        (delete-element @lens)
        (%lens-cursor% .new :name 'lens))))
  
  (define-class %lens-cursor% (%cursor-element%)
    (value shape (measure-graphic "lens.png"))
    (value hotspot (shape-center (measure-graphic "lens.png")))
    
    (def (draw)
      (draw-graphic (point 0 0) "lens.png")))
  
  (card features/cursor-elements
      (%standard-test-card% :title "Cursor Elements")
    (elem lens (%lens-cursor%))

    (elem cursor-test (%cursor-test-rect%))
    (elem delete-test (%deleting-test-rect%))
    
    (elem button (%click-me-button% :bounds (rect 100 550 176 576) 
                                    :cursor 'lens))
    )


  ;;=======================================================================
  ;;  Changing the Z-Order
  ;;=======================================================================

  (define-class %self-raising-square% (%custom-element% )
    (attr color) 
    (attr label)
    (value shape (rect 0 0 100 100))
    
    (def (draw)
      (clear-dc (.color)))
    (def (mouse-down event)
      (self .raise-to-top!))
    
    ;; This needs to be a child element.  We want to see if it gets raised
    ;; along with its parent.
    (text text-elem ((point 10 10) $title-style (.label))))

  (card features/z-order
      (%standard-test-card% :title "Changing the Z-Order")
    (elem a (%self-raising-square%
             :at (point 200 200) :color (color 255 0 0) :label "A"))
    (elem b (%self-raising-square%
             :at (point 250 250) :color (color 0 255 0) :label "B")))

#|
  ;;=======================================================================
  ;;  Primitive Layout Support
  ;;=======================================================================

  (card features/primitive-layout
      (%standard-test-card% :title "Primitive Layout Support")
    (define (square parent size color &opt (at (point 0 0)))
      (create %rectangle%
              :parent parent
              :at at
              :shape (rect 0 0 size size)
              :color color))
    (define outer (square (current-card) 200 (color 255 0 0)))
    (define inner (square outer          100 (color 0 0 255)))
    (send outer center-on-parent!)
    (send inner center-on-parent!)
    (box (rect 0 400 800 600) :name 'box)
    (define background (square @box 100 (color 255 255 0)))
    (define first  (square background 50 (color 0 255 0)))
    (define second (square background 50 (color 255 0 0) (below first 10)))
    (define third  (square background 50 (color 0 0 255) 
                           (to-the-right-of first 10)))
    (define fourth (square background 50 (color 255 0 255) (below third 10)))
    (send background fit-to-children! 10)
    (send background center-on-parent!)
    )


  ;;=======================================================================
  ;;  Resizing Elements
  ;;=======================================================================
  
  (define-element-template %opaque-overlay% [] (%custom-element%)
    (on draw ()
      (clear-dc (color #xFF #x00 #x00))))

  (card features/resizing-elements
      (%standard-test-card% :title "Resizing Elements")

    ;; Our standard shapes
    (define small-rect (rect 0 0 100 100))
    (define big-rect (rect 0 0 110 110))
    (define small-poly (polygon (point 0 0) (point 100 100) (point 0 100)))
    (define big-poly (polygon (point 0 0) (point 110 110) (point 0 110)))

    ;; Our elements.
    (create %opaque-overlay%
            :name 'opaque-overlay
            :at (below @title 20)
            :shape small-rect)
    (create %rectangle%
            :name 'transparent-overlay
            :at (to-the-right-of @opaque-overlay 20)
            :shape small-rect
            :color (color #xFF #x00 #x00 #x80))
    (create %custom-element%
            :name 'square-zone
            :at (to-the-right-of @transparent-overlay 20)
            :shape small-rect
            :overlay? #f
            :cursor 'hand)
    (create %custom-element%
            :name 'poly-zone
            :at (to-the-right-of @square-zone 20)
            :shape small-poly
            :overlay? #f
            :cursor 'hand)

    ;; Update all our elements to have the specified shapes.
    (define (set-shapes! square-shape poly-shape)
      (set! (@opaque-overlay .shape) square-shape)
      (set! (@transparent-overlay .shape) square-shape)
      (set! (@square-zone .shape) square-shape)
      (set! (@poly-zone .shape) poly-shape))
      
    ;; Buttons to change element size.
    (text-button (below @opaque-overlay 20) "Small"
                 (callback (set-shapes! small-rect small-poly))
                 :name 'small)
    (text-button (to-the-right-of @small 10) "Big"
                 (callback (set-shapes! big-rect big-poly))
                 :name 'big)
    )


  ;;=======================================================================
  ;;  StateDB test cards
  ;;=======================================================================

  (define-stylesheet $state-db-style
    :base $base-style
    :color $color-highlight
    :size 24)

  (define-element-template %state-db-zone%
      []
      (%custom-element% :shape $screen-rect :alpha? #t)
 
    (define center-x (/ (rect-width (dc-rect)) 2))
    (define center-y (/ (rect-height (dc-rect)) 2))

    (define height #f)
    (define width #f)

    ;; TODO - OK, this isn't really an optimal interface for elements which
    ;; redraw under state-db control.
    (define-state-db-listener [update-size state-db]
      (set! height (state-db '/test/height))
      (set! width  (state-db '/test/width))
      (send self invalidate))

    (on draw ()
      (let* [[x (- center-x (/ (* width 50) 2))]
             [y (- center-y (/ (* height 50) 2))]
             [r (rect x y (+ x (* width 50)) (+ y (* height 50)))]]
        (draw-rectangle r (color 255 0 0 #x40))
        (draw-text (rect 300 10 500 100) $state-db-style 
                   (cat "Area of rect " (* width height) " units"))))
    )

  (card features/state-db (%standard-test-card% :title "State DB")
    (center-text $state-db-style (dc-rect)
                 "Num = Width of rect\nAlt+Num = Height of rect")
    
    (set! (state-db '/test/width) 5)
    (set! (state-db '/test/height) 5)

    (create %state-db-zone%)

    (on char (event)
      (let ((c (event-character event))
            (mods (event-modifiers event)))
        (cond
         [(and (char-numeric? c) (equal? mods '(alt)))
          ;; set height?
          (set! (state-db '/test/height) (string->number (string c)))]
         [(and (char-numeric? c) (equal? mods '()))
          ;; set width?     
          (set! (state-db '/test/width) (string->number (string c)))]
         [else
          (call-next-handler)])))
    )


  ;;=======================================================================
  ;;  State DB Listeners
  ;;=======================================================================
  
  (card features/state-db-listeners
      (%standard-test-card% :title "State DB Listeners")

    ;; This listener fires every time the clock ticks, and updates
    ;; our blinker state.
    (define-state-db-listener (blink state-db)
      (set! (state-db '/blinker/on?)
            (even? (state-db '/system/clock/seconds))))
  
    ;; This listener fires every time the blinker state changes, and redraws
    ;; the screen.
    (define-state-db-listener (draw state-db)
      (draw-rectangle (rect 10 10 50 50)
                      (if (state-db '/blinker/on?)
                          (color 255 0 0)
                          (color 255 255 255))))
  
    )

  ;;=======================================================================
  ;;  Animated Overlay test cards
  ;;=======================================================================
  
  (card features/animated-overlay (%standard-test-card%
                                   :title "Animated Overlay")
  
    (define motion-step 10)
  
    (define left-key #\h)
    (define right-key #\l)
    (define up-key #\k)
    (define down-key #\j)
    (define index-key #\space)
  
    (define (index->string n)
      (if (< n 10)
          (cat "0" n)
          (cat n)))
  
    ;; generate a list of png files for the ludlum meter
    (define (ludlum-png-files)
      (let recurse [[index 0]]
        (if (<= index 20)
            (cons (cat "anim/lud03_gauge_" (index->string index) ".png")
                  (recurse (+ index 1)))
            '())))

    (define max-index 20)
    (define next-index 0)
    (define (get-index)
      (let [[result next-index]]
        (set! next-index (modulo (+ next-index 1) (+ max-index 1)))
        result))

    (define offset-x 10)
    (define max-x (- (rect-width $screen-rect) (+ 185 offset-x)))
    (define current-x 0)
    (define (get-x move)
      (set! current-x (modulo (+ current-x move) (+ max-x 1)))
      (+ offset-x current-x))

    (define offset-y 10)
    (define max-y (- (rect-height $screen-rect) (+ 108 offset-y)))
    (define current-y 0)
    (define (get-y move)
      (set! current-y (modulo (+ current-y move) (+ max-y 1)))
      (+ offset-y current-y))
  
    (set! (state-db '/animated-overlay/x) (get-x 0))
    (set! (state-db '/animated-overlay/y) (get-y 0))
    (set! (state-db '/animated-overlay/index) (get-index))
  
    (center-text $state-db-style (dc-rect)
                 (cat "space = Update needle\n'j' = Move down\n"
                      "'k' = Move up\n'h' = Move left\n" 
                      "'l' = Move right"))

    (create %animated-graphic%
            :name 'meter
            :at (point 0 0)
            :state-path '/animated-overlay
            :graphics (ludlum-png-files))

    (on char (event)
      (let [[c (event-character event)]]
        (cond [(equals? c index-key)
               (set! (state-db '/animated-overlay/index) (get-index))]
              [(equals? c up-key)
               (set! (state-db '/animated-overlay/y)
                     (get-y (- motion-step)))]
              [(equals? c down-key)
               (set! (state-db '/animated-overlay/y)
                     (get-y motion-step))]
              [(equals? c left-key)
               (set! (state-db '/animated-overlay/x)
                     (get-x (- motion-step)))]
              [(equals? c right-key)
               (set! (state-db '/animated-overlay/x)
                     (get-x motion-step))]
              [else
               (call-next-handler)])))


    )

  ;;=======================================================================
  ;;  Hooks
  ;;=======================================================================
  ;;  These are a rarely-used developer feature, similar to Emacs hooks.
  ;;  There are more of these lurking in kernel.ss.  Use at your own
  ;;  risk.
  
  (define *last-node-defined* #f)

  (hook-add-function! *node-defined-hook* 'test
                      (fn (node)
                        (set! *last-node-defined* node)))

  (card features/hooks (%standard-test-card% :title "Hooks")
    ;; Nothing to do for now.
    )

  (assert (eq? *last-node-defined* features/hooks))


  ;;=======================================================================
  ;;  Idle Events
  ;;=======================================================================

  (defclass <flasher> ()
    (on? :initvalue #f)
    (last-updated :initializer current-seconds)
    rect)

  (define (flasher-idle flasher)
    (define now (current-seconds))
    (define on? (flasher-on? flasher))
    (when (> now (flasher-last-updated flasher))
      (draw-rectangle (flasher-rect flasher)
                      (color #x00 #x00 (if on? #xFF #x00)))
      (set! (flasher-last-updated flasher) now)
      (set! (flasher-on? flasher) (not on?))))

  (define-element-template %flasher% [rect] ()
    (define flasher (make <flasher> :rect rect))
    (on idle (event)
      (flasher-idle flasher)))

  (card features/idle-events
      (%standard-test-card% :title "Idle Events (Two Flashing Lights)")
    (create %flasher% :rect (rect 200 100 250 150))

    (define flasher (make <flasher> :rect (rect 100 100 150 150)))
    (on idle (event)
      (flasher-idle flasher)))

  ;;=======================================================================
  ;;  Animation
  ;;=======================================================================

  (sequence features/animation)
  
  (define-element-template %example-button% 
      [[text :type <string>]]
      (%basic-button% :shape (rect 0 0 150 20))
    (on draw-button (state)
      (draw-rectangle-outline (dc-rect) $color-black 1)
      (draw-rectangle (dc-rect) $color-white)
      (draw-text (center-shape-on (measure-text $login-style text) (dc-rect))
                 $login-style
                 text)))
  
  (define (example-button n text func)
    (create %example-button% 
            :at (point 20 (+ 40 (* n 25))) 
            :text text
            :action (deferred-callback 
                      (send (current-card) setup-elements)
                      (func)
                      (nap 5)
                      (send (current-card) setup-elements))))
  
  (define-card-template %animation-demo%
      []
      (%standard-test-card%)
    (on setup-elements ()
      (map delete-element-if-exists '(rect foo bar sprite)) 
      (rectangle (rect 400 50 500 150) (color 0 0 255) :name 'rect)
      (text (point 400 160) $splash-style "Foo" :name 'foo)
      (text (point 400 200) $splash-style "Bar" :name 'bar)
      (sprite (point 400 240) 
              (prefix-and-suffix "anim/lud03_gauge_" 
                                 (map (fn (x) (zero-pad 2 x)) (range 0 20)) 
                                 ".png")
              :name 'sprite))
    (send self setup-elements))
  
  (card features/animation/basic
      (%animation-demo% :title "Animations - Basic")
    (example-button
     0
     "Slide"
     (callback 
       (animate 1000 (slide @rect (point 600 50)))))
    (example-button
     1
     "Reshape"
     (callback
       (animate 1000 (reshape @rect (rect 0 0 100 200)))))
    (example-button
     2
     "Slide &amp; Reshape"
     (callback
       (animate 1000 
         (slide-and-reshape @rect (center-shape-on (rect 0 0 200 200) 
                                                   (send @rect bounds))))))
    (example-button
     3
     "Play"
     (callback 
       (animate 1000
         (play-sprite @sprite))))
    (example-button
     4
     "Interpolate"
     (callback 
       (animate 1000 
         (interpolate (@rect .color) (color 255 0 0 0))))))
     
  (card features/animation/combined
      (%animation-demo% :title "Animations - Combined")
    (example-button 
     0
     "Ease In/Out"
     (callback 
       (animate 1000
         (ease-in/out
           (slide @rect (point 600 50))
           (slide @foo (point 500 50))))))
    (example-button
     1
     "Simultaneously"
     (callback
       (animate 1000
         (slide @foo (point 370 180))
         (slide @bar (point 430 180)))))
    (example-button
     2
     "After"
     (callback
       (animate 1000
         (after
           [0.0 (slide @foo (point 740 574))]
           [0.5 (slide @bar (point 0 574))]))))
    (example-button
     3
     "Nested"
     (callback 
       (animate 4000
         (play-sprite @sprite)
         (after
           [0.0  (slide @sprite (point 600 100))]
           [0.25 (slide @sprite (point 600 400))]
           [0.5  (slide @sprite (point 100 400))]
           [0.75 (slide @sprite (point 100 100))]))))
    (example-button
     4
     "Instant"
     (callback
       (animate 2000
         (after
           [0.0 (slide @foo (point 200 60))]
           [0.5 (slide @foo (point 400 60))]
           [0.5 (slide @foo (point 400 550))]))))
    (example-button
     5
     "Do Nothing"
     (callback
       (animate 5000
         (slide @sprite (point 60 400))
         (after
           [0.2 (play-sprite @sprite)]
           [0.4 (do-nothing)]
           [0.6 (play-sprite @sprite :reverse? #t)]
           [0.8]))))
    (example-button
     6
     "Immediately &amp; Finally"
     (callback
       (animate 2000
         (after
           [0.0
            (immediately (slide @sprite (point 0 0)))
            (slide @foo (point 100 200))]
           [0.5
            (finally (slide @sprite (point 200 300)))
            (slide @foo (point 200 100))]
           [0.75]))))
    (example-button
     7
     "Quantize"
     (callback
       (animate 2000
         (quantize 3.2
           (slide @foo (point 0 0))
           (slide @bar (point 150 400)))))))
  |#
  )
