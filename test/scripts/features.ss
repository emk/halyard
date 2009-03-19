;; PORTED
(module features (lib "halyard.ss" "halyard")
  (require (lib "drag.ss" "halyard"))
  (require (lib "q-and-a.ss" "halyard"))
  (require (lib "animate.ss" "halyard"))
  (require (lib "state-db.ss" "halyard"))
  (require (lib "base.ss" "halyard-test"))

  ;; Experimental slot initializer library.
  (require (lib "initialize-slot.ss" "halyard"))
  
  (group /features)
  
  (card /features/controls (%standard-test-card% :title "Controls")
    (text description 
        ((below (.title-elem) 20) $text16 
         "We have fairly straight-forward access to a variety of native controls for use in Halyard programs.  In most cases, we can change the color and the borders.  In addition to the controls shown here, we can have list boxes and drop-down edit fields.  Except for the HTML control, most of these should be 508-compliant with minimal work."
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

  (card /features/text-formatting
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
  
  (card /features/text-input
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
        (.update-display ((.parent) .display2)))
      (def (next-control)
        ((.parent) .edit2))
      )

    (elem display (%text-input-display% :at (below (.edit) 10)))
    (elem display2 (%text-input-display% :at (below (.display) 10)))

    ;; Column 2.
    (elem edit2 
        (%edit-box-demo%
         :rect (move-rect-left-top-to (rect 0 0 200 32)
                                      (to-the-right-of (.edit) 20)))
      (def (prev-control)
        ((.parent) .edit)))

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
    (def (click)
      (send @browser-elem (.browser-command))))
  
  (define-stylesheet $browser-style :base $base-style :family "Times")
  
  (card /features/browser ()
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
      (if (equal? (event .url) "http://www.nowhere.org/")
        (begin
          (logger 'error #f "Access to " (event .url) " is restricted.  Sorry!")
          (event .veto!))
        (super)))
    
    (def (browser-page-changed event)
      (.draw-url (event .url)))
    
    (def (browser-title-changed event)
      (.draw-title (event .text)))
    
    (def (status-text-changed event)
      (.draw-status-text (event .text)))
    
    (def (progress-changed event)
      (.draw-progress-bar (event .done?) (event .value)))
    
    (def (update-ui event)
      (define command (event .command))
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
  
  (card /features/launch-browser (%standard-test-card% :title "Launch Browser")
    (text-button open-button ((below (.title-elem) 20) "Open Default Browser"
                              :skip-test-actions? #t)
      (def (click)
        (set! (@result .text)
              (if (open-in-browser "http://iml.dartmouth.edu")
                "Success."
                "Error opening URL."))))
    (text-box result
              ((move-rect-left-top-to (rect 0 0 200 20) 
                                      (below (.open-button) 10))
               $caption-style 
               ""))
    )

  
  ;;=======================================================================
  ;;  ActiveX
  ;;=======================================================================
  
  (card /features/activex (%standard-test-card% :title "ActiveX")
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

  (card /features/transparency (%standard-test-card% :title "Tranparency")
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

  (group /features/transitions)

  (define (transition-text msg)
    (new-centered-text $transition-style msg))

  (define (show-trans msg trans)
    (draw-white-background)
    (transition-text msg)
    (refresh :transition trans :ms 1000))

  (card /features/transitions/crossfade () 
    (run (show-trans "Crossfade"  'crossfade)))
  (card /features/transitions/wipeleft ()  
    (run (show-trans "Wipe Left"  'wipeleft)))
  (card /features/transitions/wiperight () 
    (run (show-trans "Wipe Right" 'wiperight)))
  (card /features/transitions/wipeup ()    
    (run (show-trans "Wipe\nUp"   'wipeup)))
  (card /features/transitions/wipedown ()  
    (run (show-trans "Wipe\nDown" 'wipedown)))
  (card /features/transitions/pushleft ()  
    (run (show-trans "Push Left"  'pushleft)))
  (card /features/transitions/pushright () 
    (run (show-trans "Push Right" 'pushright)))
  (card /features/transitions/pushup ()    
    (run (show-trans "Push\nUp"   'pushup)))
  (card /features/transitions/pushdown ()  
    (run (show-trans "Push\nDown" 'pushdown)))
  (card /features/transitions/toblack ()   
    (run (show-trans "To Black"   'toblack)))
  (card /features/transitions/fromblack () 
    (run (show-trans "From Black" 'fromblack)))


  ;;=======================================================================
  ;;  Templates and Events
  ;;=======================================================================

  (define (point->boring-button-shape text)
    (offset-rect
     (inset-rect (measure-text $login-button-style text) -5)
     (point 5 5)))

  (define-class %boring-button% (%clickable-zone%)
    (attr text)
    (value overlay? #t)
    (value shape (point->boring-button-shape (string->xml (.text))))
    
    (def (draw)
      (draw-rectangle (dc-rect) $color-white)
      (draw-text (inset-rect (dc-rect) 5) $login-button-style
                 (string->xml (.text)))))

  (define-class %click-me-button% (%basic-button%)
    (value alpha? #t)
    (def (click)
      (jump @index))
    (def (draw)
      (draw-graphic (point 0 0)
                    (case (.button-state)
                      [[normal]   "click-me-normal.png"]
                      [[active]   "click-me-active.png"]
                      [[pressed]  "click-me-pressed.png"]
                      [[disabled] "click-me-disabled.png"])))
    )

  (card /features/templates-events
      (%standard-test-card% :title "Templates & Events")
    (elem boring-button (%boring-button%
                         :at (rect-center $screen-rect)
                         :text "Click Me")
      (def (click)
        (set! (.parent.message.text) "Thanks!")))
    (elem click-1 (%click-me-button% :bounds (rect 100 100 176 126)))
    (elem click-2 (%click-me-button% :bounds (rect 100 130 176 156)))
    (elem click-3 (%click-me-button% :bounds (rect 100 160 176 186)))
    (elem click-4 (%click-me-button% :bounds (rect 100 190 176 216)))
    (elem message (%text-box% :at (below (.boring-button) 10)
                              :shape (shape 200 20)
                              :text ""
                              :style $base-style))
    )

  ;;=======================================================================
  ;;  Widgets, zones, etc.
  ;;=======================================================================

  ;; Testing polygonal Zones.
  (card /features/zones (%standard-test-card% :title "Clickable Zones")
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

  (card /features/overlays (%standard-test-card% :title "Overlays")
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

  (define-class %movable-lens% (%draggable-object%)
    (value alpha? #t) 
    (value shape (measure-graphic "lens.png"))
    (def (draw)
      (draw-graphic (point 0 0) "lens.png")))
  
  (define-class %lens-box% (%drag-target%)
    (value shape (shape 200 200))

    (attr has-been-dragged-to? #f :writable? #t)

    (centered-text note ($text16 "It works!" :shown? #f))
    (after-updating has-been-dragged-to?
      (set! ((.note) .shown?) (.has-been-dragged-to?)))

    (def (draw)
      (clear-dc $color-white)
      (draw-rectangle-outline (dc-rect) $color-black 1))

    (def (drag-allowed? event draggable)
      #t)
    (def (drag-succeeded event draggable)
      (set! (.has-been-dragged-to?) #t))
    )

  (card /features/dragndrop (%fancy-white-test-card%
                            :title "Simple\nDrag and Drop")
    (elem target (%lens-box% :at (point 10 10)))
    (elem lens (%movable-lens% :home-point (point 350 300))))


  (define-class %color-answer-box% (%answer%)
    (attr color :type <color>)
    (value shape (shape 100 100))

    (def (draw)
      (draw-rectangle (dc-rect) (.color))
      (define border-color
        (case (.answer-state)
          [[correct]   (color 255 255 0)]
          [[incorrect] (color 255 0 0)]
          [[normal]    (color 0 0 0 0)]
          [[active]    (color 128 128 128)]
          [[pressed]   (color 0 0 0)]
          [[disabled]  (color 128 128 128)]))
      (draw-rectangle-outline (dc-rect) border-color 2))
    )

  (card /features/q-and-a (%standard-test-card% :title "Which box is green?")
    (elem question (%question% :at (below (.title-elem) 20)
                               :bounds (shape 0 0) ; Will recalculate.
                               :overlay? #f)
      (elem green-box
          (%color-answer-box% :at (point 0 0) :color (color 0 255 0)
                              :correct? #t))
      (elem blue-box
          (%color-answer-box% :at (to-the-right-of (.green-box) 10)
                              :color (color 0 0 255)))
      (setup
        (.fit-to-children!)))
    (text result ((below (.question) 10) $text16 "Pick one."))
    (def (correct-answer answer)
      (set! ((.result) .text) "That's right!"))
    (def (incorrect-answer answer)
      (set! ((.result) .text) "Nope.")))

  (card /features/geometric (%white-test-card% :title "Geometric primitives")
    (setup
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

  (group /features/mask)

  (card /features/mask/light ()
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
      (define at (offset-by-point (event .position) offset))
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
      (define at (offset-by-point (event .position) offset))
      (with-dc self
        (mask at "mask/eraser.png"))))

  (card /features/mask/eraser ()
    (elem erasable (%erasable%))
    (setup
      (draw-graphic (point 0 0) "mask/blend-background.png")))


  ;;=======================================================================
  ;;  Custom cursors
  ;;=======================================================================

  (register-cursor 'eyeball "eyeball.png" 
                   :hotspot (point 7 15))

  (card /features/custom-cursors
      (%standard-test-card% :title "Custom Cursors")
    (elem button (%click-me-button% :bounds (rect 100 250 176 276) 
                                    :cursor 'eyeball)))

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
  
  (card /features/cursor-elements
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

  (card /features/z-order
      (%standard-test-card% :title "Changing the Z-Order")
    (elem a (%self-raising-square%
             :at (point 200 200) :color (color 255 0 0) :label "A"))
    (elem b (%self-raising-square%
             :at (point 250 250) :color (color 0 255 0) :label "B")))


  ;;=======================================================================
  ;;  Primitive Layout Support
  ;;=======================================================================

  (card /features/primitive-layout
      (%standard-test-card% :title "Primitive Layout Support")
    (define (square size)
      (shape size size))
    (rectangle outer ((square 200) (color 255 0 0))
      (setup (.center-on-parent!))
      (rectangle inner ((square 100) (color 0 0 255))
        (setup (.center-on-parent!))))

    (box box ((rect 0 400 800 600))
      (rectangle background ((square 100) (color 255 255 0))
        (rectangle first  ((square 50) (color 0 255 0)))
        (rectangle second ((square 50) (color 255 0 0) 
                           :at (below (.first) 10)))
        (rectangle third  ((square 50) (color 0 0 255) 
                           :at (to-the-right-of (.first) 10)))
        (rectangle fourth ((square 50) (color 255 0 255) 
                           :at (below (.third) 10)))
        (setup 
          (.fit-to-children! 10)
          (.center-on-parent!))))
    )

  
  ;;=======================================================================
  ;;  Resizing Elements
  ;;=======================================================================
  
  (define-class %opaque-overlay% (%custom-element%)
    (def (draw)
      (clear-dc (color #xFF #x00 #x00))))

  (card /features/resizing-elements
      (%standard-test-card% :title "Resizing Elements")

    ;; Our standard shapes
    (define $small-rect (rect 0 0 100 100))
    (define $big-rect (rect 0 0 110 110))
    (define $small-poly (polygon (point 0 0) (point 100 100) (point 0 100)))
    (define $big-poly (polygon (point 0 0) (point 110 110) (point 0 110)))

    ;; Our elements.
    (elem opaque-overlay (%opaque-overlay%
                          :at (below (.title-elem) 20)
                          :shape $small-rect))
    (elem transparent-overlay (%rectangle%
                               :at (to-the-right-of (.opaque-overlay) 20)
                               :shape $small-rect
                               :color (color #xFF #x00 #x00 #x80)))
    (elem square-zone (%custom-element%
                       :at (to-the-right-of (.transparent-overlay) 20)
                       :shape $small-rect
                       :overlay? #f
                       :cursor 'hand))
    (elem poly-zone (%custom-element%
                     :at (to-the-right-of (.square-zone) 20)
                     :shape $small-poly
                     :overlay? #f
                     :cursor 'hand))

    ;; Update all our elements to have the specified shapes.
    (def (set-shapes! square-shape poly-shape)
      (set! ((.opaque-overlay) .shape) square-shape)
      (set! ((.transparent-overlay) .shape) square-shape)
      (set! ((.square-zone) .shape) square-shape)
      (set! ((.poly-zone) .shape) poly-shape))
      
    ;; Buttons to change element size.
    (text-button small ((below @opaque-overlay 20) "Small")
      (def (click)
        (.parent.set-shapes! $small-rect $small-poly)))
    (text-button big ((to-the-right-of @small 10) "Big")
      (def (click)
        (.parent.set-shapes! $big-rect $big-poly)))
    )


  ;;=======================================================================
  ;;  StateDB test cards
  ;;=======================================================================

  (define-stylesheet $state-db-style
    :base $base-style
    :color $color-highlight
    :size 24)

  (define-class %state-db-zone% (%custom-element%)
    (value bounds $screen-rect) 
    (value alpha? #t)
 
    (define center-x (/ (rect-width $screen-rect) 2))
    (define center-y (/ (rect-height $screen-rect) 2))

    (attr height 0 :writable? #t)
    (attr width 0 :writable? #t)

    (setup 
      ;; TODO - Should state-db listeners be registered on the static node 
      ;; instead?
      ;; TODO - OK, this isn't really an optimal interface for elements which
      ;; redraw under state-db control.
      (define-state-db-listener [update-size state-db]
        (set! (.height) (state-db '/test/height))
        (set! (.width)  (state-db '/test/width))
        (.invalidate)))

    (def (draw)
      (let* [[x (- center-x (/ (* (.width) 50) 2))]
             [y (- center-y (/ (* (.height) 50) 2))]
             [r (rect x y (+ x (* (.width) 50)) (+ y (* (.height) 50)))]]
        (draw-rectangle r (color 255 0 0 #x40))
        (draw-text (rect 300 10 500 100) $state-db-style 
                   (cat "Area of rect " (* (.width) (.height)) " units"))))
    )

  (card /features/state-db (%standard-test-card% :title "State DB")
    (centered-text legend ($state-db-style 
                           "Num = Width of rect\nAlt+Num = Height of rect"))
  
    (def (initialize &rest keys)
      (super)
      (set! (state-db '/test/width) 5)
      (set! (state-db '/test/height) 5))

    (elem zone (%state-db-zone%))

    (def (char event)
      (let ((c (event .character))
            (mods (event .modifiers)))
        (cond
         [(and (char-numeric? c) (equal? mods '(alt)))
          ;; set height?
          (set! (state-db '/test/height) (string->number (string c)))]
         [(and (char-numeric? c) (equal? mods '()))
          ;; set width?     
          (set! (state-db '/test/width) (string->number (string c)))]
         [else
          (super)])))
    )

  ;;=======================================================================
  ;;  State DB Listeners
  ;;=======================================================================
  
  (card /features/state-db-listeners
      (%standard-test-card% :title "State DB Listeners")

    (run
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
  
    )

  ;;=======================================================================
  ;;  Animated Overlay test cards
  ;;=======================================================================
  
  (card /features/animated-overlay (%standard-test-card%
                                   :title "Animated Overlay")

    (centered-text instructions
      ($state-db-style
       (cat "space = Update needle\n'j' = Move down\n"
            "'k' = Move up\n'h' = Move left\n" 
            "'l' = Move right")))
  
    (def (index->string n)
      (if (< n 10)
          (cat "0" n)
          (cat n)))
  
    ;; generate a list of png files for the ludlum meter
    (def (ludlum-png-files)
      (let recurse [[index 0]]
        (if (<= index 20)
            (cons (cat "anim/lud03_gauge_" (.index->string index) ".png")
                  (recurse (+ index 1)))
            '())))

    (attr max-index 20 :writable? #t)
    (attr next-index 0 :writable? #t)
    (def (get-index)
      (let [[result (.next-index)]]
        (set! (.next-index) (modulo (+ (.next-index) 1) (+ (.max-index) 1)))
        result))

    (define $original-offset-x 10)
    (attr offset-x $original-offset-x :writable? #t)
    (attr max-x (- (rect-width $screen-rect) (+ 185 $original-offset-x))
      :writable? #t)
    (attr current-x 0 :writable? #t)
    (def (get-x move)
      (set! (.current-x) (modulo (+ (.current-x) move) (+ (.max-x) 1)))
      (+ (.offset-x) (.current-x)))

    (define $original-offset-y 10)
    (attr offset-y $original-offset-y :writable? #t)
    (attr max-y (- (rect-width $screen-rect) (+ 185 $original-offset-y))
       :writable? #t)
    (attr current-y 0 :writable? #t)
    (def (get-y move)
      (set! (.current-y) (modulo (+ (.current-y) move) (+ (.max-y) 1)))
      (+ (.offset-y) (.current-y)))    

    
    (define $motion-step 10)
  
    (define $left-key #\h)
    (define $right-key #\l)
    (define $up-key #\k)
    (define $down-key #\j)
    (define $index-key #\space)

    (def (char event)
      (let [[c (event .character)]]
        (cond [(equals? c $index-key)
               (set! (state-db '/animated-overlay/index) (.get-index))]
              [(equals? c $up-key)
               (set! (state-db '/animated-overlay/y)
                     (.get-y (- $motion-step)))]
              [(equals? c $down-key)
               (set! (state-db '/animated-overlay/y)
                     (.get-y $motion-step))]
              [(equals? c $left-key)
               (set! (state-db '/animated-overlay/x)
                     (.get-x (- $motion-step)))]
              [(equals? c $right-key)
               (set! (state-db '/animated-overlay/x)
                     (.get-x $motion-step))]
              [else
               (super)])))

  
    (run
      (set! (state-db '/animated-overlay/x) (.get-x 0))
      (set! (state-db '/animated-overlay/y) (.get-y 0))
      (set! (state-db '/animated-overlay/index) (.get-index))
      
      (%animated-graphic% .new :name 'meter
                               :at (point 0 0)
                               :state-path '/animated-overlay
                               :graphics (.ludlum-png-files)))
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

  (card /features/hooks (%standard-test-card% :title "Hooks")
    (centered-text note
        ($text16 "If you can load the script, this card is OK."))
    )

  (assert (eq? *last-node-defined* /features/hooks))


  ;;=======================================================================
  ;;  Idle Events
  ;;=======================================================================

  (define-class %flasher% (%custom-element%)
    (value shape (shape 50 50))
    
    (attr on? #f :writable? #t)
    (after-updating on? (.invalidate))
    
    (attr last-updated (current-seconds) :writable? #t)
    
    (def (draw)
      (draw-rectangle (dc-rect) (color #x00 #x00 (if (.on?) #xFF #x00))))
    
    (def (maybe-update-flasher)
      (define now (current-seconds))
      (when (> now (.last-updated))
        (set! (.last-updated) now)
        (set! (.on?) (not (.on?))))))

  ;; Test IDLE handlers on elements, cards, and nested elements.
  (card /features/idle-events
      (%standard-test-card% :title "Idle Events (Three Flashing Lights)")
 
    (elem flasher-1 (%flasher% :at (point 200 100))
      (def (idle)
        (.maybe-update-flasher)))
    
    (elem flasher-2 (%flasher% :at (point 100 100)))
    
    (box layout ((rect 100 150 250 250))
      (elem flasher-3 (%flasher% :at (point 0 0))
        (setup (.center-on-parent!))
        (def (idle) (.maybe-update-flasher))))
 
    (def (idle)
      ((.flasher-2) .maybe-update-flasher)))
  
  ;;=======================================================================
  ;;  Animation
  ;;=======================================================================
  
  (group /features/animation)
  
  (define-class %example-animation% (%text-button%)
    (def (click)
      (run-deferred
       (fn ()
         ((current-card) .reset-elements)
         (.play)
         (nap 5)
         ((current-card) .reset-elements))))
    )

  (provide example-animation)
  (define-node-helper example-animation (at label) %example-animation%)
  
  (define-class %animation-demo% (%standard-test-card%)
    (def (reset-elements)
      (map delete-element-if-exists '(rect foo bar sprite)) 
      (new-rectangle (rect 400 50 500 150) (color 0 0 255) :name 'rect)
      (new-text (point 400 160) $splash-style "Foo" :name 'foo)
      (new-text (point 400 200) $splash-style "Bar" :name 'bar)
      (new-sprite (point 400 240) 
        (prefix-and-suffix "anim/lud03_gauge_" 
                           (map (fn (x) (zero-pad 2 x)) (range 0 20)) 
                           ".png")
        :name 'sprite))
    (setup
      (.reset-elements)))
  
  (card /features/animation/basic
      (%animation-demo% :title "Animations - Basic")
    
    (example-animation slide ((below (.title-elem) 20) "Slide")
      (def (play) 
        (animate 1000 (slide @rect (point 600 50)))))
    
    (example-animation reshape ((below (.slide) 10) "Reshape")
      (def (play)
        (animate 1000 (reshape @rect (rect 0 0 100 200)))))
    
    (example-animation slide-and-reshape ((below (.reshape) 10) 
                                          "Slide &amp; Reshape")
      (def (play)
        (animate 1000 
          (slide-and-reshape @rect (center-shape-on (rect 0 0 200 200) 
                                                    (@rect .bounds))))))
    
    (example-animation play ((below (.slide-and-reshape) 10) "Play")
      (def (play)
        (animate 1000
          (play-sprite @sprite))))
    
    (example-animation interpolate ((below (.play) 10) "Interpolate")
      (def (play) 
        (animate 1000 
          (interpolate (@rect .color) (color 255 0 0 0))))))
  
  
  (card /features/animation/combined
      (%animation-demo% :title "Animations - Combined")
    (example-animation ease-in-out ((below (.title-elem) 20) "Ease In/Out")
      (def (play)
        (animate 1000
          (ease-in/out
            (slide @rect (point 600 50))
            (slide @foo (point 500 50))))))
    (example-animation simultaneously ((below (.ease-in-out) 10) 
                                       "Simultaneously")
      (def (play)
        (animate 1000
          (slide @foo (point 370 180))
          (slide @bar (point 430 180)))))
    (example-animation after ((below (.simultaneously) 10) "After")
      (def (play)
        (animate 1000
          (after
            [0.0 (slide @foo (point 740 574))]
            [0.5 (slide @bar (point 0 574))]))))
    (example-animation nested ((below (.after) 10) "Nested")
      (def (play) 
        (animate 4000
          (play-sprite @sprite)
          (after
            [0.0  (slide @sprite (point 600 100))]
            [0.25 (slide @sprite (point 600 400))]
            [0.5  (slide @sprite (point 100 400))]
            [0.75 (slide @sprite (point 100 100))]))))
    (example-animation instant ((below (.nested) 10) "Instant")
      (def (play)
        (animate 2000
          (after
            [0.0 (slide @foo (point 200 60))]
            [0.5 (slide @foo (point 400 60))]
            [0.5 (slide @foo (point 400 550))]))))
    (example-animation do-nothing ((below (.instant) 10) "Do Nothing")
      (def (play)
        (animate 5000
          (slide @sprite (point 60 400))
          (after
            [0.2 (play-sprite @sprite)]
            [0.4 (do-nothing)]
            [0.6 (play-sprite @sprite :reverse? #t)]
            [0.8]))))
    (example-animation immediately-and-finally 
        ((below (.do-nothing) 10) "Immediately &amp; Finally")
      (def (play)
        (animate 2000
          (after
            [0.0
             (immediately (slide @sprite (point 0 0)))
             (slide @foo (point 100 200))]
            [0.5
             (finally (slide @sprite (point 200 300)))
             (slide @foo (point 200 100))]
            [0.75]))))
    (example-animation quantize 
        ((below (.immediately-and-finally) 10) "Quantize")
      (def (play)
       (animate 2000
         (quantize 3.2
           (slide @foo (point 0 0))
           (slide @bar (point 150 400))))))
    )


  ;;=======================================================================
  ;;  Empty groups & external nodes
  ;;=======================================================================

  (group /features/empty-group)
  (external-group /features/empty-external-group)
  (external-card /features/external-card)

  )
