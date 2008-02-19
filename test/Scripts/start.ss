;;=========================================================================
;;  Tamale Demo and Test Script
;;=========================================================================
;;  The goal of this script is to demonstrate good Tamale coding style,
;;  show how all of Tamale's features are used, and provide a way for
;;  Tamale programmers to test whether a given feature is working.
;;
;;  Tamale is still very much in flux, and we won't freeze any APIs until
;;  version 1.0 is released.  And quite frankly, we *do* need to clean up
;;  a bunch of stuff before it's ready for use by the general public.
;;  We apologize for any temporary inconveniences.


;;=========================================================================
;;  Global Setup and Initialization
;;=========================================================================
;;  At the top of each script, we can include any modules we'll need,
;;  define global variables, and set a few internal parameters.

;; This file is only included here so that the syntax highlighter knows
;; about it.
(require (lib "animate.ss" "5L"))

;; If you want to access any Quake 2 features, you need to include this
;; module manually.
;;(require (lib "quake2.ss" "5L"))

;; XXX - These modules are used in various cards below, but they're
;; probably obsolete and are expected to go away or at least be
;; removed from the core.
;;(require (lib "data-file.ss" "5L"))

;;(require (lib "updater.ss" "5L"))

(require "base.ss")


;; Other non-Runtime files we'll be using appear further down in the script.

;; Tamale has an internal graphics cache.  If you make this cache
;; larger, Tamale uses more memory but displays graphics faster.
;; The default size is probably reasonable.
(set-image-cache-size! (* 4 1024 1024))


;;=========================================================================
;;  Global Event Handlers
;;=========================================================================
;;  Tamale uses a HyperCard-like event-handling model: Events are first
;;  passed to individual elements on the screen (in the case of mouse
;;  events), or to the card itself (in the case of key events).  If an
;;  event isn't handled, it travels up the containment hierarchy:
;;
;;    Elements... -> Card -> Groups/Sequences... -> Program
;;
;;  Here are our global, program-level event handlers.

(with-instance %root-node%
  (def (char event)
    (cond
     [(and (equal? (event-character event) #\i)
           (equal? (event-modifiers event) '(alt)))
      (jump index)]
     [else (super)])))


;;=========================================================================
;;  Cards
;;=========================================================================
;;  Our actual script.

;; Scripts begin running at the start card.  In our case, we'll just
;; immediately JUMP to our index card, below, without bothering to display
;; anything.  The JUMP function changes the current card.
(card start (%card%)
  (run
    (debug-log "Hello, world!  IT'S ALIIIIVE!")
    (jump index)))

(define-class %menu-item% (%box%)
  (attr y :type <integer>)
  (attr text :type <string>)
  (attr jump-to) ; TODO - Add :type specifier.  See case 1394.
  
  (value at (point 0 (.y)))
  (value shape (shape 800 100))
  (value clickable-where-transparent? #t)
  
  (centered-text label ($menu-style (.text) :max-width (rect-width (.shape))))

  (def (mouse-down event)
    (jump (.jump-to))))

(define-node-helper menu-item (y text jump-to) %menu-item%)

;; The index card is based on our %simple-card% template.
(card index (%black-test-card% :title "Tamale Features (updated)")
  (menu-item controls ( 80 "Controls"    @features/controls))
  (menu-item movies   (180 "More Movies" @media/qt/movies))
  (text release-id
      ((point 10 580) $title-style (or #| (program-release-id) |# "")))
  )


;;=========================================================================
;;  Other Files in This Script
;;=========================================================================

(require (file "media.ss"))
(require (file "features.ss"))
(require (file "ruby-objects-test.ss"))
(require (file "bugs.ss"))
(require (file "experiments.ss"))
(require (file "test-cases.ss"))

;; only for appropriate highlighting/indentation
;;(require (lib "tamale-unit.ss" "5L"))

;;(require (lib "tamale-unit-test.ss" "5L"))
;;(require (lib "updater-test.ss" "5L"))


;;=========================================================================
;;  Updater-Related Cards
;;=========================================================================

#|
(sequence updater ())

(define *error-message* "")

(define (updater-handler exn)
  (5l-log (cat exn)) 
  (set! *error-message* (if (exn? exn)
                          (exn-message exn)
                          (cat exn)))
  (jump @error))
  
(card updater/check ()
  (draw-black-background)
  (unless (auto-update-possible? (current-directory))
    (set! *error-message* (cat "Automatic updates are not possible. Please "
                               "obtain an up to date copy of the installer "
                               "and reinstall the program."))
    (jump @error))
  (with-handlers [[exn:fail? updater-handler]]
    (init-updater!)
    (if (check-for-update) 
      (begin 
        (draw-black-background)
        (title "An update is available. Would you like to download it?")
        (draw-menu-item 'install 80 "Install update" @download)
        (draw-menu-item 'skip 180 "Skip update" index))
      (begin 
        (title "No updates available at this time.")
        (draw-menu-item 'continue 80 "Continue" index)))))

(card updater/download ()
  (draw-black-background)
  (with-handlers [[exn:fail? updater-handler]]
    (define r (rect 0 80 640 180))
    
    (title "Downloading update")
    (draw-text r $menu-style "Cancel")
    (clickable-zone r (callback (cancel-download) (jump @index)))
    
    (download-update (fn (file percent) #f))
    (jump @install)))

(card updater/install ()
  (draw-black-background)
  (title (cat "Updates were successfully downloaded. Press Continue to "
                   "quit the program, install the updates, and restart the "
                   "program."))
  (draw-menu-item 'continue 80 "Continue" @apply))

(card updater/apply ()
  (apply-update))

(card updater/error ()
  (draw-black-background)
  (title 
   (cat "An error occured while checking for or trying to apply updates: "
        *error-message*))
  (draw-menu-item 'ok 80 "OK" index))


;;=========================================================================
;;  Quake 2
;;=========================================================================

(group quake2)

(define-stylesheet $q2-debug-style
  :base $base-style
  :family "Times"
  :size 12
  :color $color-black)

(group quake2/demo1 (%quake2-level% :game "testq2" :level "demo1")

  ;; You can define any console commands you want here.
  (define-quake2-command (hide)
    (quake2-hide))

  ;; You can run various console commands before the level loads.
  (quake2-command "+mlook")
  (quake2-command "bind leftarrow +moveleft")
  (quake2-command "bind rightarrow +moveright")

  (quake2-command "bind h hide")
  (quake2-command "bind i jump index")
  (quake2-command "bind b jump @bullets")

  ;; If, for some reason, you need to run code after the level is loaded,
  ;; you can define a SETUP-FINISHED handler.
  (on setup-finished ()
    (call-next-handler)
    (quake2-command "give railgun")
    (quake2-command "use railgun"))
  )

(card quake2/demo1/run (%quake2-level-run%))

(card quake2/demo1/bullets ()
  (on char (event)
    (if (and (equal? (event-character event) #\Return)
             (null? (event-modifiers event)))
        (jump @run)
        (call-next-handler)))

  (draw-rectangle (dc-rect) (color 0 0 0 #x80))
  (banner "Quake 2")
  (bullet2 "Emdedded directly in engine.")
  (bullet2 "Switch between Quake2 and traditional content.")
  (bullet2 "Run Quake 2 commands from Scheme.")
  (clickable-zone (rect 0 0 640 480) (callback (jump @run))))

;; Toss in another level, just for kicks.
(group quake2/demo2 (%quake2-level% :game "testq2" :level "demo2"))
(card  quake2/demo2/run (%quake2-level-run%))

(group quake2/trigger (%quake2-level% :game "testq2" :level "triggertest")
  (quake2-command "bind l trigger lights")
  (quake2-command "bind b trigger bridge")
  (quake2-command "bind mouse2 reticle")
  (quake2-command "bind leftarrow +moveleft")
  (quake2-command "bind rightarrow +moveright")
  (quake2-command "+mlook")
  )
(card quake2/trigger/run (%quake2-level-run%))

(group quake2/region (%quake2-level% :game "testq2" :level "regiontest"))
(card quake2/region/run (%quake2-level-run%))

(group quake2/path (%quake2-level% :game "testq2" :level "pathtest")
  (quake2-command "vid_gamma 0.5") 
  (on setup-finished ()
    (call-next-handler)
    (quake2-command "notarget")))
(card quake2/path/run (%quake2-level-run%))

(define $watchdir-text "Looking in watched direction.")

(define-element-template %watchdir-display% []
    (%custom-element% :shape (measure-text $q2-debug-style $watchdir-text))
  (on draw ()
    (clear-dc $color-white)
    (draw-text (dc-rect) $q2-debug-style $watchdir-text))
  (define-state-db-listener (watchdir-change state-db)
    (set! (.shown?)
          (state-db '/quake2/looking-in-watched-direction?))))

(group quake2/watchdir (%quake2-level% :game "testq2" :level "regiontest")
  (on setup-finished ()
    (call-next-handler)
    (set! (state-db '/quake2/looking-in-watched-direction?) #f)
    (create %watchdir-display% :parent self)
    ;; You'll probably want to try a bunch of other cases by hand.
    (quake2-command "watchdir 165 -165 -10 10")))
(card quake2/watchdir/run (%quake2-level-run%))

(define-element-template %weapon-name-display%
    []
    (%custom-element%
     :shape (measure-text $q2-debug-style "Really long weapon name")
     :cursor 'arrow)

  ;; TODO - Another state-db redraw interface to clean up.
  (define weapon #f)
  (define-state-db-listener [weapon-change state-db]
    (set! weapon (state-db '/quake2/weapon))
    (send self invalidate)
    (refresh))
  
  (on erase-background ()
    (clear-dc $color-white))
  (on draw ()
    (draw-text (dc-rect) $q2-debug-style
               (state-db '/quake2/weapon))))

|#
