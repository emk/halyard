;; Quake 2 game engine interface.  Note that this code doesn't really use
;; the new node system all that well--it's based on an older model of doing
;; things, and hasn't been fully integrated into the newer parts of the
;; engine.
(module quake2 (lib "5l.ss" "5L")

  (provide quake2-driver set-quake2-driver!
           quake2-launch quake2-shown? quake2-show quake2-hide
           quake2-command quake2-background-load-command
           quake2-should-run-in-background?
           set-quake2-should-run-in-background?!
           quake2-running?
           quake2-float set-quake2-float! quake2-string set-quake2-string!
           quake2-print quake2-print-line quake2-register-command
           define-quake2-command quake2-jump-helper
           %quake2-level% %quake2-level-run%)

  (define *quake2-initialized?* #f)
  (define *quake2-driver* 'soft)

  (define (quake2-driver)
    *quake2-driver*)

  (define (set-quake2-driver! val)
    (set! *quake2-driver* val))
  
  (define (quake2-launch game)
    (call-prim 'Quake2Init game *quake2-driver*)
    (set! *quake2-initialized?* #t))

  (define (quake2-shown?)
    (call-prim 'Quake2IsShown))

  (define (quake2-show)
    (call-prim 'Quake2Show))

  (define (quake2-hide)
    (call-prim 'Quake2Hide))

  (define (quake2-command cmd)
    (call-prim 'Quake2Command cmd))

  (define (quake2-background-load-command cmd)
    (call-prim 'Quake2BackgroundLoadCommand cmd)
    (while (quake2-loading-in-background?)
      (idle)))

  (define (quake2-loading-in-background?)
    (call-prim 'Quake2IsLoadingInBackground))

  (define (quake2-should-run-in-background?)
    (call-prim 'Quake2GetShouldRunInBackground))

  (define (set-quake2-should-run-in-background?! run?)
    (call-prim 'Quake2SetShouldRunInBackground run?))

  (define (quake2-running?)
    (and *quake2-initialized?*
         (or (quake2-shown?)
             (quake2-should-run-in-background?))))

  (define (quake2-float cvar)
    (call-prim 'Quake2GetFloatVar cvar))

  (define (set-quake2-float! cvar value)
    (call-prim 'Quake2SetFloatVar cvar value))

  (define (quake2-string cvar)
    (call-prim 'Quake2GetStringVar cvar))

  (define (set-quake2-string! cvar value)
    (call-prim 'Quake2SetStringVar cvar value))

  (define (quake2-print msg)
    (call-prim 'Quake2Print msg))

  (define (quake2-print-line msg)
    (quake2-print msg)
    (quake2-print "\n"))

  (define (quake2-register-command name func)
    (define (func-wrapper . args)
      (with-errors-blocked (quake2-print-line)
                           (apply func args)))
    (call-prim 'Quake2RegisterCommand name func-wrapper))


  (define-syntax define-quake2-command
    (syntax-rules ()
      [(define-quake2-command (name . args) . body)
       (quake2-register-command 'name (fn args . body))]))

  (define (quake2-jump-helper card-name)
    (if (and (> (string-length card-name) 0)
             (eq? (string-ref card-name 0) #\@))
        (jump (@* (substring card-name 1 (string-length card-name))))
        (jump (find-card card-name))))
  
  (define-class %quake2-level% (%card-group%)
    (attr game :type <string> :label "Game directory")
    (attr level :type <string> :label "Level name")

    (setup
      ;; Make sure the necessary directories exist.
      (foreach [dir (list (.game) "baseq2")]
        (unless (directory-exists? (build-path (current-directory) dir))
          (error (cat "Missing Quake 2 game directory: " dir))))

      ;; Boot the Quake 2 game engine (if it hasn't been launched already).
      (quake2-launch (.game))
      
      ;; Initialize /quake2 state-db variables to plausible values.
      (set! (state-db '/quake2/weapon) "none")
      (set! (state-db '/quake2/rad/meter-flux) '())
      (set! (state-db '/quake2/rad/body-dose) '(0.0 0.0 0.0 0.0 0.0))
      
      ;; Define any universally useful console commands.
      (define-quake2-command (jump card-name)
        (quake2-jump-helper card-name)))
  
    ;; Next, give the groups based on this template a chance to initialize.
    ;; Once they've done any extra initialization, *then* we can respond
    ;; to setup-finished and actually load the level.
    (def (setup-finished)
      (super)
      (quake2-command "killserver")
      (quake2-background-load-command (cat "map " (.level)))
      (set! (quake2-should-run-in-background?) #t))

    ;; When exiting this group, shut down Quake 2.
    (def (exit)
      (super)
      (quake2-command "killserver")
      (set! (quake2-should-run-in-background?) #f))
    )

  (define-class %quake2-level-run% (%card%)
    (setup
      (quake2-show))
    (def (exit)
      (super)
      (quake2-hide)))

  )

