;; PORTED
;; Some very minimal test cards for the updater.  It can do quite a bit
;; more, but unfortunately, much of the necessary example code for the
;; fancier features is tied into various boring in-house libraries,
;; graphics, etc.
;;
;; Also, these cards are very old, and should not be used as an example of
;; good coding style.
(module updater-example (lib "halyard.ss" "halyard")

  (require (lib "updater.ss" "halyard"))
  (require (file "base.ss"))

  (sequence updater ())
  
  (define *error-message* "")
  
  (define (updater-handler exn)
    (app-log (cat exn)) 
    (set! *error-message* (if (exn? exn)
                              (exn-message exn)
                              (cat exn)))
    (jump @error))
  
  (card updater/check ()
    (run
      (draw-black-background)
      (unless (auto-update-possible? (current-directory))
        (set! *error-message*
              (cat "Automatic updates are not possible. Please "
                   "obtain an up to date copy of the installer "
                   "and reinstall the program."))
        (jump @error))
      (with-handlers [[exn:fail? updater-handler]]
        (init-updater!)
        (if (check-for-update) 
          (begin 
            (draw-black-background)
            (new-title
             "An update is available. Would you like to download it?"
             :name 'title)
            (new-text-button (below @title 20) "Install update"
                             (fn () (jump @download)) :name 'install)
            (new-text-button (below @install 10) "Skip update"
                             (fn () (jump @index))))
          (begin 
            (new-title "No updates available at this time."
                       :name 'title)
            (new-text-button (below @title 20) "Continue"
                             (fn () (jump @index))))))))
    
  (card updater/download ()
    (run
      (draw-black-background)
      (with-handlers [[exn:fail? updater-handler]]
        (define r (rect 0 80 640 180))
        
        (new-title "Downloading update" :name 'title)
        (draw-text r $menu-style "Cancel")
        (new-clickable-zone r (callback (cancel-download) (jump @index)))
        
        (download-update (fn (file percent) #f))
        (jump @install))))
    
  (card updater/install ()
    (run
      (draw-black-background)
      (new-title
       (cat "Updates were successfully downloaded. Press Continue to "
            "quit the program, install the updates, and restart the "
            "program.")
       :name 'title)
      (new-text-button (below @title 20) "Continue"
                       (fn () (jump @apply)))))
    
  (card updater/apply ()
    (run
      (apply-update)))
    
  (card updater/error ()
    (run
      (draw-black-background)
      (new-title 
       (cat "An error occured while checking for or trying to apply updates: "
            *error-message*)
       :name 'title)
      (new-text-button (below @title 20) "OK" (fn () (jump @index)))))
  )