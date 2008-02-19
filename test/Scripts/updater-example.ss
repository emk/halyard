;; Some very minimal test cards for the updater.  It can do quite a bit
;; more, but unfortunately, much of the necessary example code for the
;; fancier features is tied into various boring in-house libraries,
;; graphics, etc.
(module updater-example (lib "5l.ss" "5L")

  (require (lib "updater.ss" "5L"))

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
  )