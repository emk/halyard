;; PORTED
(module quake2 (lib "halyard.ss" "halyard")
  (require (lib "base.ss" "halyard-test"))

  ;; If you want to access any Quake 2 features, you need to include this
  ;; module manually.
  (require (lib "quake2.ss" "halyard"))
  
  (group /quake2 (%card-group% :ordered? #f))
  
  (define-stylesheet $q2-debug-style
    :base $base-style
    :family "Times"
    :size 12
    :color $color-black)
  
  (group /quake2/demo1 (%quake2-level% :game "testq2" :level "demo1"
                                      :ordered? #f)
    (setup
      ;; You can define any console commands you want here.
      (define-quake2-command (hide)
        (quake2-hide))
      
      ;; You can run various console commands before the level loads.
      (quake2-command "+mlook")
      (quake2-command "bind leftarrow +moveleft")
      (quake2-command "bind rightarrow +moveright")
      
      (quake2-command "bind h hide")
      (quake2-command "bind i jump index")
      (quake2-command "bind b jump @bullets"))
    
    ;; If, for some reason, you need to run code after the level is loaded,
    ;; you can define a SETUP-FINISHED handler.
    (def (setup-finished)
      (super)
      (quake2-command "give railgun")
      (quake2-command "use railgun"))
    )
  
  (card /quake2/demo1/run (%quake2-level-run%))
  
  (card /quake2/demo1/bullets ()
    (def (char event)
      (if (and (equal? (event-character event) #\Return)
               (null? (event-modifiers event)))
        (jump @run)
        (super)))
    
    ;; We used to have some nicer-looking bullets here, but that support
    ;; code is long dead...
    (text title ((point 50 50) (stylesheet :base $caption-style
                                           :justification 'left)
                 (cat "Quake 2\n"
                      "- Emdedded directly in engine.\n"
                      "- Switch between Quake2 and traditional content.\n"
                      "- Run Quake 2 commands from Scheme.\n")))
    (clickable-zone leave ($screen-rect (callback (jump @run))))

    (setup
      ;; Due to some low-level sneakiness, our card is initialized with a
      ;; screenshot of Quake.  We break our otherwise invariant rule, and
      ;; refrain from completely clearing the card--we just dim the
      ;; existing image.
      (draw-rectangle (dc-rect) (color 0 0 0 #x80))))
  
  ;; Toss in another level, just for kicks.
  (group /quake2/demo2 (%quake2-level% :game "testq2" :level "demo2"
                                      :ordered? #f))
  (card /quake2/demo2/run (%quake2-level-run%))
  
  (group /quake2/trigger (%quake2-level% :game "testq2" :level "triggertest"
                                        :ordered? #f)
    (setup
      (quake2-command "bind l trigger lights")
      (quake2-command "bind b trigger bridge")
      (quake2-command "bind mouse2 reticle")
      (quake2-command "bind leftarrow +moveleft")
      (quake2-command "bind rightarrow +moveright")
      (quake2-command "+mlook")))
  (card /quake2/trigger/run (%quake2-level-run%))
  
  (group /quake2/region (%quake2-level% :game "testq2" :level "regiontest"
                                       :ordered? #f))
  (card /quake2/region/run (%quake2-level-run%))
  
  (group /quake2/path (%quake2-level% :game "testq2" :level "pathtest"
                                     :ordered? #f)
    (setup
      ;; Try "trigger start_patrol" or "trigger pause_point".
      (quake2-command "vid_gamma 0.5") )
    (def (setup-finished)
      (super)
      (quake2-command "notarget")))
  (card /quake2/path/run (%quake2-level-run%))
  
  (define $watchdir-text "Looking in watched direction.")
  
  (define-class %watchdir-display% (%custom-element%)
    (value bounds (measure-text $q2-debug-style $watchdir-text))
    (def (draw)
      (clear-dc $color-white)
      (draw-text (dc-rect) $q2-debug-style $watchdir-text))
    (setup
      (define-state-db-listener (watchdir-change state-db)
        (set! (.shown?)
              (state-db '/quake2/looking-in-watched-direction?)))))
  
  (group /quake2/watchdir (%quake2-level% :game "testq2" :level "regiontest"
                                         :ordered? #f)
    (def (setup-finished)
      (super)
      (set! (state-db '/quake2/looking-in-watched-direction?) #f)
      (%watchdir-display% .new :parent self)
      ;; You'll probably want to try a bunch of other cases by hand.
      (quake2-command "watchdir 165 -165 -10 10")))
  (card /quake2/watchdir/run (%quake2-level-run%))
  
  (provide %weapon-name-display%)

  (define-class %weapon-name-display% (%custom-element%)
    (value bounds (measure-text $q2-debug-style "Really long weapon name"))

    (attr weapon "" :writable? #t)
    (after-updating weapon
      (.invalidate))
    
    (setup
      (define-state-db-listener [weapon-change state-db]
        (set! (.weapon) (state-db '/quake2/weapon))))
  
    (def (erase-background)
      (clear-dc $color-white))
    (def (draw)
      (draw-text (dc-rect) $q2-debug-style (.weapon)))
    )

  )

