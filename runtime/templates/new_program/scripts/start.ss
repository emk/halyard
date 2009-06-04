(module start (lib "halyard.ss" "halyard")
  
  ;; Define a stylesheet for our text.
  (define-stylesheet $base-style
    :family "Nimbus Sans L"
    :size 18
    :color (color #x00 #x00 #x00))

  ;; Scripts begin running at the /start card.
  (card /start (%card%)

    (rectangle background ($screen-rect (color #xFF #xFF #xFF)))

    ;; Display a large welcome message.
    (text hello ((point 20 20)
                 (stylesheet :base $base-style :size 48)
                 "Hello!"))

    (text help ((below (.hello) 10)
                $base-style
                "Type Alt-. to view this script."))

    ;; When the /start card is run, change our message.
    (run
      (set! (.hello.text) "Hello, world!"))
    )

  )
