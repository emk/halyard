;; PORTED
(module base (lib "halyard.ss" "halyard")

  (provide $color-black $color-white $color-paper $color-offwhite
           $color-highlight)

  ;; Some typical global variables we'll be using.
  (define $color-black (color #x00 #x00 #x00))
  (define $color-white (color #xFF #xFF #xFF))
  (define $color-paper (color #xE4 #xDD #xD2))
  (define $color-offwhite (color #xF0 #xF0 #xF0))
  (define $color-highlight (color #xFF #xD8 #x45))
  
  (provide $base-style $splash-style $transition-style $audio-stream-style
           $menu-style $title-style $text16 $caption-style $login-style
           $login-button-style)

  ;; A stylesheet for text.  For now, you can get a list of available
  ;; font names by looking in Fonts/cache.dat.  We'll probably want to
  ;; improve this font interface in the future.
  (define-stylesheet $base-style
    :family "Nimbus Roman No9 L"
    :size 12
    :flags '()
    :justification 'left
    :shadow-offset 0
    :color $color-white
    :shadow-color $color-black
    :highlight-color $color-highlight
    :highlight-shadow-color $color-black
    :height-adjustment (percent -20))
  
  ;; Stylesheets can inherit from other stylesheets and override a
  ;; few properties.
  (define-stylesheet $splash-style
    :base $base-style :family "URW Bookman L" :size 36 :justification 'center)
  
  (define-stylesheet $transition-style
    :size 128
    :base $splash-style
    :color $color-black
    :shadow-color $color-offwhite
    :highlight-color $color-highlight
    :highlight-shadow-color $color-offwhite)
  
  (define-stylesheet $audio-stream-style
    :base $transition-style
    :size 64)
  
  (define-stylesheet $menu-style :base $splash-style :shadow-offset 2)
  
  (define-stylesheet $title-style 
    :base $base-style :size 18 :height-adjustment -4 :shadow-offset 2)
  
  (define-stylesheet $text16 :base $base-style :size 16 :shadow-offset 1)
  
  (define-stylesheet $caption-style
    :base $base-style
    :size 18
    :justification 'center
    :color $color-white)
  
  (define-stylesheet $button-style
    :base $base-style
    :size 18
    :flags '()
    :justification 'center)

  (define-stylesheet $login-style
    :base $base-style
    :size 16
    :flags '()
    :justification 'center
    :color $color-black
    :highlight-color (color #xFF #xFF #xCC))
  
  (define-stylesheet $login-button-style
    :base $login-style
    :flags '()
    :justification 'left
    :size 18)


  ;;=======================================================================
  ;;  Global Functions
  ;;=======================================================================
  ;;  Here are some simple global functions we'll use throughout the
  ;;  script.
  
  (provide draw-default-background draw-black-background draw-white-background)

  (define (draw-default-background)
    (draw-graphic (point 0 0) "back.png"))
  
  (define (draw-black-background)
    (clear-dc $color-black))
  
  (define (draw-white-background)
    (clear-dc $color-white))

  
  ;;=======================================================================
  ;;  Element Templates
  ;;=======================================================================

  (provide %title% new-title)

  (define-class %title% (%text%)
    (attr title :writable? #t)
    (value at (point 10 10))
    (value style $title-style)

    ;; TODO - Wouldn't it be nice to handle property dependencies
    ;; automatically?  See case 2353.
    (value text (cat "<h>" (string->xml (.title)) "</h>"))
    (after-updating title
      (set! (.text) (cat "<h>" (string->xml (.title)) "</h>"))))
  
  (define (new-title str &key name)
    (%title% .new :title str :name name))
  
  
  ;;=======================================================================
  ;;  Card Templates
  ;;=======================================================================
  
  (provide %test-card% %standard-test-card% %black-test-card%
           %white-test-card%
           %centered-text% centered-text new-centered-text
           %fancy-white-test-card%)

  (define-class %test-card% (%card%)
    (attr title :type <string> :writable? #t)
    
    ;; TODO - Here's another kind of slightly different property
    ;; dependency.  Hmm.  See case 2353.
    (elem title-elem (%title% :title (.title)))
    (after-updating title
      (set! ((.title-elem) .title) (.title)))

    ;; TODO - Add button which jumps back to index.
    )

  (define-class %standard-test-card% (%test-card%)
    (setup
      (draw-default-background)))
  
  (define-class %black-test-card% (%test-card%)
    (setup
      (draw-black-background)))

  (define-class %white-test-card% (%test-card%)
    (setup
      (draw-white-background)))

  (define-class %centered-text% (%text%)
    (default at (point 0 0))
    (after-updating shape
      (.center-on-parent!))
    (setup
      (.center-on-parent!)))

  (define-node-helper centered-text (style text) %centered-text%)

  (define (new-centered-text style text)
    (%centered-text% .new :style style :text text))
  
  (define-class %fancy-white-test-card% (%card%)
    (attr title :type <string>)
    (centered-text title-elem ($audio-stream-style (.title)))
    (setup
      (draw-white-background)))


  ;;=======================================================================
  ;;  color Utilities
  ;;=======================================================================

  (define (map-channels f c)
    (color (f (color-red c)) (f (color-green c)) (f (color-blue c))))

  (define (lighten c)
    (map-channels (fn (v) (+ v #x20)) c))

  (define (darken c)
    (map-channels (fn (v) (- v #x20)) c))
  
  
  ;;=======================================================================
  ;;  Element Templates
  ;;=======================================================================
  
  (provide %text-button% text-button new-text-button)

  ;;; A simple and ugly button which doesn't require loading any image
  ;;; files.  We'd obviously like something better than this.
  (define-class %text-button% (%basic-button%)
    (attr label :type <string>)
    (value shape 
           (move-rect-left-top-to 
            (inset-rect
             (measure-text $button-style (.label)) -7)
            (point 0 0)))
    
    (def (draw)
      (define style (.button-state))
      (define base (color #x80 #x80 #x80))
      (define text-base $color-black)
      ;; Select the colors to use.
      (define back-color (case style
                           [[disabled normal] base]
                           [[active pressed]  (lighten base)]))
      (define lt-color   (case style
                           [[disabled normal] (lighten base)]
                           [[active]          (lighten (lighten base))]
                           [[pressed]         base]))
      (define rb-color   (case style
                           [[disabled normal] (darken base)]
                           [[active]          base]
                           [[pressed]         (lighten base)]))
      (define text-color (case style
                           [[disabled]        (lighten base)]
                           [[normal active pressed] $color-white]))

      (clear-dc lt-color)
      (draw-rectangle (move-rect-left-top-to (dc-rect) (point 2 2))
                      rb-color)
      (draw-rectangle (inset-rect (dc-rect) 2) back-color)
      (draw-text (inset-rect (dc-rect) 7)
                 (stylesheet :base $button-style :color text-color)
                 (.label))))

  (define-node-helper text-button (at label) %text-button%)
  
  (define (new-text-button at label action &key name)
    (%text-button% .new :at at :label label :action action :name name))

  )
