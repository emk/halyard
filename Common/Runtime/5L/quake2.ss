(module quake2 (lib "5L.ss" "5L")

  (provide quake2-launch quake2-shown? quake2-show quake2-hide
           quake2-command quake2-background-load-command quake2-float
           set-quake2-float! quake2-string set-quake2-string! quake2-print
           quake2-print-line quake2-register-command)

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
  
  )
