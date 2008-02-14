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


(card custom-element-demo ()
  (setup
    (clear-dc (color 0 0 0))
    (clickable-zone (inset-rect $screen-rect 10)
                    (callback (jump duck-and-cover)))
    (%custom-element% .new :bounds (inset-rect $screen-rect 100) :name 'inner)
    (with-dc @inner
      (draw-graphic (point 0 0) "mask/blend-background.png"))
    )
  )

(require (lib "tamale-unit.ss" "5L"))

(define-class %advised% ()
  (attr events '() :writable? #t)
  (def (record-event! name)
    (set! (.events) (append (.events) (list name))))

  (def (foo)
    (.record-event! 'foo)
    'foo-result)
  (advise before (foo)
    (.record-event! 'before-foo))
  (advise after (foo)
    (.record-event! 'after-foo))

  (def (call-super-from-advice)
    (void))
  (advise after (call-super-from-advice)
    (super))

  #| If we _do_ decide that ADVISE AROUND is worth the trouble, here is our
     proposed syntax.  But this actually takes a bit more machinery to
     implement, and offers a greater opportunity for evil.  So we'll try to
     live without it for now.

  (def (double x)
    (.record-event! 'double)
    (* 2 x))

  (advise around (double x)
    (.record-event! 'before-double)
    (let [[result (original x)]]
      (.record-event! 'after-double)
      result))

  |#

  (def (not-overridden)
    (.record-event! 'not-overridden))
  )

(define-class %another-advised% (%advised%)
  (def (foo)
    (.record-event! 'another-foo-1)
    (super)
    (.record-event! 'another-foo-2))
  (advise before (foo)
    (.record-event! 'before-another-foo))
  (advise after (foo)
    (.record-event! 'after-another-foo))

  (advise after (not-overridden)
    (.record-event! 'after-not-overridden))
  )

(define-test-case <advise-test> () []
  (test "ADVISE should run code before and after the original method"
    (define advised (%advised% .new))
    (assert-equals '() (advised .events))
    (advised .foo)
    (assert-equals '(before-foo foo after-foo) (advised .events)))

  (test "ADVISE should not affect the return value"
    (define advised (%advised% .new))
    (assert-equals 'foo-result (advised .foo)))

  (test "ADVISE should interleave with method calls when using SUPER"
    (define another (%another-advised% .new))
    (another .foo)
    (assert-equals '(before-another-foo another-foo-1
                     before-foo foo after-foo
                     another-foo-2 after-another-foo)
                   (another .events)))

  (test "ADVISE AFTER should not pass SUPER to advice"
    (define advised (%advised% .new))
    (assert-raises exn:fail? (advised .call-super-from-advice)))

  (test "ADVISE AFTER should work in the absense of an actual method"
    (define advised (%advised% .new))
    (advised .not-overridden)
    (assert-equals '(not-overridden) (advised .events))

    (define another (%another-advised% .new))
    (another .not-overridden)
    (assert-equals '(not-overridden after-not-overridden) (another .events)))
  )

(card advise-test (%test-suite%)
  (value tests (list <advise-test>)))

(define-class %initialize-without-super% (%custom-element%)
  (def (initialize &rest args)
    (debug-log "Calling the bad initialize method.")))

(define-test-case <initialize-without-super-test> () []
  (test-elements "Failing to call SUPER shouldn't cause an infinite loop"
    (assert-raises exn:fail?
      (%initialize-without-super% .new :name 'bad-init
                                       :at (point 0 0)
                                       :shape (shape 200 300)))))

(card initialize-without-super-test (%test-suite%)
  (value tests (list <initialize-without-super-test>)))


#|
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
(require (lib "quake2.ss" "5L"))

;; XXX - These modules are used in various cards below, but they're
;; probably obsolete and are expected to go away or at least be
;; removed from the core.
(require (lib "data-file.ss" "5L"))

(require (lib "updater.ss" "5L"))
|#
;; As long as these functions still exist, we need to test them.  However,
;; none of these functions should be used in new code.
(require (lib "deprecated.ss" "5L"))

(require "base.ss")
#|(require "countdown.ss")


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

(with-instance (running-root-node)
  (on char (event)
    (cond
     ((and (equal? (event-character event) #\i)
           (equal? (event-modifiers event) '(alt)))
      (jump index))
     ((equal? (event-character event) #\return)
      ;; XXX - this is a really evil hack, that won't work in a lot of
      ;; cases and might screw things up. It might leave screenshot
      ;; widgets littered all over the place, might just fail to work,
      ;; etc. Use at your own risk. For debugging purposes only.
      (if (element-exists? 'screenshot)
          (begin
            (screenshot)
            (send @screenshot cleanup)
            (delete-element @screenshot))
          (create %rect-drawing% 
                  :shape $screen-rect 
                  :border 0
                  :name 'screenshot)))
     (call-next-handler))))

;; TODO - Move these elsewhere.
(define (start-ambient)
  (movie (rect 0 0 0 0) "ambient4.wav"
         :name 'ambient :audio-only? #t :loop? #t))

(define (kill-ambient)
  (delete-element @ambient))
|#

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

(define (draw-menu-item name y text card)
  (define r (move-rect-top-to (rect 0 0 800 100) y))
  (center-text $menu-style r text)
  (clickable-zone r (callback (jump card)) :name name))

;; The index card is based on our %simple-card% template.  Notice how
;; we specify the title.
(card index (%black-test-card% :title "Tamale Features (updated)")
  (setup
    (draw-menu-item 'controls 80 "Controls" @features/controls)
    (draw-menu-item 'movies 180 "More Movies" @media/qt/movies)
    (draw-menu-item 'about 280 "About" @bullets/about)
    (text (point 10 580) $title-style (or #| (program-release-id) |# 
                                          ""))
  ))


#|
;;=========================================================================
;;  Bullets
;;=========================================================================
;;  This whole bullets API is very, very obsolete.  So are the slides
;;  themselves.

(sequence bullets)

(define-stylesheet $bullet-1
  :family "Nimbus Roman No9 L"
  :size 21
  :flags '()
  :justification 'left
  :shadow-offset 2
  :align 'left
  :color $color-white
  :highlight-color (color #xFF #xD8 #x45)
  :height-adjustment -50)

(define-stylesheet $bullet-2 :base $bullet-1 :size 19 :height-adjustment -40)
(define-stylesheet $bullet-3 :base $bullet-1 :size 17 :height-adjustment -40)

(define-stylesheet $banner-style
  :base $bullet-1
  :size 24
  :color (color #xFF #xD8 #x45)
  :highlight-color (color #xF0 #xF0 #xF0))

(define (banner title)
  (draw-text (rect 52 46 690 103) $banner-style title)
  ;(line 55 77 590 77 0 1) #black dropshadow line
  ;(line 52 76 588 76 248 1) #yellow stroke
  (set! *text-y* 80))

(define (bullet1 msg)
  (draw-text (rect 65 (+ *text-y* 19) 580 420) $bullet-1 msg))

(define (bullet2 msg)
  (draw-graphic (point 65 (+ *text-y* 19)) "Lbul_yellow.png")
  (draw-text (rect 85 (+ *text-y* 16) 580 420) $bullet-2 msg))

(define (bullet3 msg)
  (draw-graphic (point 85 (+ *text-y* 16)) "dash_yellow.png")
  (draw-text (rect 100 (+ *text-y* 11) 580 420) $bullet-3 msg))

(define (bullet-background)
  (draw-graphic (point 0 0) "back_02.png")
  )

(card bullets/about ()
  (bullet-background)
  (banner "Ingredients for This Demo")
  (bullet2 "\"Common\" code from existing engine")
  (bullet3 "Scheme-based runtime")
  (bullet3 "Typography library")
  (bullet3 "No Win32 code--it's a new front-end, like Win32 &amp; Mac")
  (bullet2 "New QuickTime layer (from this summer)")
  (bullet3 "Lots of features")
  (bullet3 "Not yet used in a production engine")
  (bullet2 "8 days of hard hacking...")
  (clickable-zone (rect 320 400 640 480) (callback (jump-next)))
  )

(card bullets/features ()
  (bullet-background)
  (banner "Features")
  (bullet2 "QuickTime")
  (bullet3 "Movie controller")
  (bullet3 "Ambient audio")
  (bullet3 "Multiple movies")
  (bullet3 "Interaction during movies")
  (bullet2 "Developer &amp; Testing")
  (bullet3 "Built-in grid")
  (bullet3 "Active borders")
  (bullet3 "Listener")
  (bullet3 "More flexible APIs")
  (clickable-zone (rect 320 400 640 480) (callback (jump-next)))
  )

(card bullets/missing ()
  (bullet-background)
  (banner "What's Needed If We Want to Use This Engine")
  (bullet2 "Integrate lots of code from Win32 engine")
  (bullet2 "Optimize low-level graphics (for text drawing)")
  (bullet2 "Improve full-screen mode")
  (bullet2 "<i>Lots</i> of testing")
  (set! *text-y* (+ *text-y* 100))
  (bullet2 "(Probably a month or two)")
  (clickable-zone (rect 320 400 640 480) (callback (jump index)))
  )


;;=========================================================================
;;  Other Files in This Script
;;=========================================================================
|#
(require (file "media.ss"))
#|
(require (file "features.ss"))
(require (file "bugs.ss"))
(require (file "experiments.ss"))
(require "test-cases.ss")

;; only for appropriate highlighting/indentation
(require (lib "tamale-unit.ss" "5L"))

(require (lib "tamale-unit-test.ss" "5L"))
(require (lib "updater-test.ss" "5L"))


;;=========================================================================
;;  Updater-Related Cards
;;=========================================================================

(sequence updater ())

(define *error-message* "")

(define (updater-handler exn)
  (5l-log (cat exn)) 
  (set! *error-message* (if (exn? exn)
                          (exn-message exn)
                          (cat exn)))
  (jump @error))
  
(card updater/check ()
  (black-background)
  (unless (auto-update-possible? (current-directory))
    (set! *error-message* (cat "Automatic updates are not possible. Please "
                               "obtain an up to date copy of the installer "
                               "and reinstall the program."))
    (jump @error))
  (with-handlers [[exn:fail? updater-handler]]
    (init-updater!)
    (if (check-for-update) 
      (begin 
        (black-background)
        (title "An update is available. Would you like to download it?")
        (draw-menu-item 'install 80 "Install update" @download)
        (draw-menu-item 'skip 180 "Skip update" index))
      (begin 
        (title "No updates available at this time.")
        (draw-menu-item 'continue 80 "Continue" index)))))

(card updater/download ()
  (black-background)
  (with-handlers [[exn:fail? updater-handler]]
    (define r (rect 0 80 640 180))
    
    (title "Downloading update")
    (draw-text r $menu-style "Cancel")
    (clickable-zone r (callback (cancel-download) (jump @index)))
    
    (download-update (fn (file percent) #f))
    (jump @install)))

(card updater/install ()
  (black-background)
  (title (cat "Updates were successfully downloaded. Press Continue to "
                   "quit the program, install the updates, and restart the "
                   "program."))
  (draw-menu-item 'continue 80 "Continue" @apply))

(card updater/apply ()
  (apply-update))

(card updater/error ()
  (black-background)
  (title 
   (cat "An error occured while checking for or trying to apply updates: "
        *error-message*))
  (draw-menu-item 'ok 80 "OK" index))


;;=========================================================================
;;  countdown test card
;;=========================================================================

(card test-countdown ()
  (white-background)
  (countdown 'countdown
             $screen-rect 
             (list 0 0 10) $transition-style
             (callback 
               (delete-element @countdown)
               (center-text $transition-style (dc-rect) 
                            "Your time is up!"))))


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


;;=========================================================================
;;  Login Screen
;;=========================================================================

(define-struct role (name x-position))

(define *first-name* "")
(define *last-name* "")

(define *roles*
  (list (make-role 'counselor 161)
        (make-role 'nurse 175)
        (make-role 'physician 189)
        (make-role 'outreach 203)
        (make-role 'other 217)))

(define (role-zone role)
  (define x (role-x-position role))
  (define (show-login-question)
    ;; XXX - Be careful about deleting the active zone!
    (delete-elements
     (map (fn (role) (@* (symbol->string (role-name role)))) *roles*))
    (draw-graphic (point 450 x) "chksm.png")
    (login-text (cat "Would you like to sign in as a new user,\n<i>"
                     *first-name* " " *last-name* "</i>, or sign "
                     "in again?"))
    
    (draw-graphic (point 153 299) "lmboxb.png")
    (clickable-zone (rect 148 294 285 316) (callback (jump intro))
                    :name 'newuser)
    (draw-text (rect 177 297 298 316) $login-button-style "New Trainee")
    
    (draw-graphic (point 301 299) "lmboxb.png")
    (clickable-zone (rect 296 294 440 316) (callback (jump login))
                    :name 'again)
    (draw-text (rect 322 297 445 316) $login-button-style
               "Sign In Again"))
  
  (clickable-zone (rect 450 x 550 (+ x 11))
                  (deferred-callback
                    (show-login-question))
                  :name (role-name role)))

(define (login-text msg)
  (draw-rectangle (rect 147 232 555 276) $color-paper)
  (center-text $login-style (rect 147 232 555 276) msg))  

(card login ()
  (draw-graphic (point 0 0) "login5.png")
  (login-text "Please type your <i>first</i> name and press ENTER.")
  ;; TODO - Update to use new input subsystem.
  (edit-box (rect 145 200 280 220) "" :name 'first-name :font-size 18)
  (set! *first-name* #f)
  (clickable-zone (rect 145 200 280 220) (callback (jump login))
                  :name 'reenter)
  (login-text "Please type your <i>last</i> name and press ENTER.")
  (edit-box (rect 295 200 425 220) "" :name 'last-name :font-size 18)
  (set! *last-name* #f)
  ;;(delete-elements)
  ;;(clickable-zone (rect 145 200 425 220) (callback (jump login))
  ;;                :name 'reenter)
  ;;(foreach [role *roles*]
  ;;  (role-zone role))
  ;;(login-text "Please indicate your <i>profession</i>.")
  )

;;(card on-exit ()
;;  (black-background)
;;  (on exit ()
;;    ;; This is so not allowed.
;;    (jump index)))
|#