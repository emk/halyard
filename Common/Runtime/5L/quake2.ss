(module quake2 (lib "5L.ss" "5L")

  (provide quake2-launch quake2-shown? quake2-show quake2-hide
           quake2-command quake2-background-load-command
           quake2-should-run-in-background?
           set-quake2-should-run-in-background?!
           quake2-float set-quake2-float! quake2-string set-quake2-string!
           quake2-print quake2-print-line quake2-register-command
           define-quake2-command quake2-jump-helper
           %quake2-level% %quake2-level-run%)

  (define (quake2-launch game)
    (call-5l-prim 'Quake2Init game))

  (define (quake2-shown?)
    (call-5l-prim 'Quake2IsShown))

  (define (quake2-show)
    (call-5l-prim 'Quake2Show))

  (define (quake2-hide)
    (call-5l-prim 'Quake2Hide))

  (define (quake2-command cmd)
    (call-5l-prim 'Quake2Command cmd))

  (define (quake2-background-load-command cmd)
    (call-5l-prim 'Quake2BackgroundLoadCommand cmd)
    (while (quake2-loading-in-background?)
      (idle)))

  (define (quake2-loading-in-background?)
    (call-5l-prim 'Quake2IsLoadingInBackground))

  (define (quake2-should-run-in-background?)
    (call-5l-prim 'Quake2GetShouldRunInBackground))

  (define (set-quake2-should-run-in-background?! run?)
    (call-5l-prim 'Quake2SetShouldRunInBackground run?))

  (define (quake2-float cvar)
    (call-5l-prim 'Quake2GetFloatVar cvar))

  (define (set-quake2-float! cvar value)
    (call-5l-prim 'Quake2SetFloatVar cvar value))

  (define (quake2-string cvar)
    (call-5l-prim 'Quake2GetStringVar cvar))

  (define (set-quake2-string! cvar value)
    (call-5l-prim 'Quake2SetStringVar cvar value))

  (define (quake2-print msg)
    (call-5l-prim 'Quake2Print msg))

  (define (quake2-print-line msg)
    (quake2-print msg)
    (quake2-print "\n"))

  (define (quake2-register-command name func)
    (define (func-wrapper . args)
      (with-errors-blocked (quake2-print-line)
                           (apply func args)))
    (call-5l-prim 'Quake2RegisterCommand name func-wrapper))


  (define-syntax define-quake2-command
    (syntax-rules ()
      [(define-quake2-command (name . args) . body)
       (quake2-register-command 'name (fn args . body))]))

  (define (quake2-jump-helper card-name)
    (if (and (> (string-length card-name) 0)
             (eq? (string-ref card-name 0) #\@))
        (jump (@* (substring card-name 1 (string-length card-name))))
        (jump (find-card card-name))))
  
  (define-group-template %quake2-level%
      [[game :type <string> :label "Game directory"]
       [level :type <string> :label "Level name"]]
      ()

    ;; Make sure the necessary directories exist.
    (foreach [dir (list game "baseq2")]
      (unless (directory-exists? (build-path (current-directory) dir))
        (error (cat "Missing Quake 2 game directory: " dir))))

    ;; Boot the Quake 2 game engine (if it hasn't been launched already).
    (quake2-launch game)

    ;; Initialize /quake2 state-db variables to plausible values.
    (set! (state-db '/quake2/weapon) "none")

    ;; Define any universally useful console commands.
    (define-quake2-command (jump card-name)
      (quake2-jump-helper card-name))
  
    ;; Next, give the groups based on this template a chance to initialize.
    ;; Once they've done any extra initialization, *then* we can respond
    ;; to setup-finished and actually load the level.
    (on setup-finished ()
      (call-next-handler)
      (quake2-command "killserver")
      (quake2-background-load-command (cat "map " level))
      (set! (quake2-should-run-in-background?) #t))

    ;; When exiting this group, shut down Quake 2.
    (on exit ()
      (call-next-handler)
      (quake2-command "killserver")
      (set! (quake2-should-run-in-background?) #f))
    )

  (define-card-template %quake2-level-run% [] ()
    (quake2-show)
    (on exit ()
      (call-next-handler)
      (quake2-hide)))

  )

