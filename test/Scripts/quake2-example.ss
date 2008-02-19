(module quake2-example (lib "5l.ss" "5L")

  ;; If you want to access any Quake 2 features, you need to include this
  ;; module manually.
  (require (lib "quake2.ss" "5L"))
  
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

  )

