(module install mzscheme
  (require (lib "launcher.ss" "launcher"))
  (provide installer)
  (define (installer base)
    (make-mzscheme-launcher '("-mM" "swindle")
                            (mzscheme-program-launcher-path "Swindle"))))
