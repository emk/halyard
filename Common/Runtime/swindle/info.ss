;;; Written by Eli Barzilay: Maze is Life!  (eli@barzilay.org)

(module info (lib "infotab.ss" "setup")
  ;;
  (define name "Swindle")
  (define blurb
    '("Swindle extensions for MzScheme -- CLOS and more."))
  (define help-desk-message
    "Mz/Mr: (require (lib \"swindle.ss\" \"swindle\"))")
  ;;
  ;; Can't use these since the startup parameters are hard-wired
  ;; (define mzscheme-launcher-names     '("swindle"))
  ;; (define mzscheme-launcher-libraries '("swindle.ss"))
  (define install-collection "install.ss")
  ;;
  (define compile-omit-files '("install.ss"))
  ;;
  ;; This simple interface is not enough, use tool.ss instead
  ;; (define drscheme-language-modules
  ;;   '(("swindle.ss" "swindle")
  ;;     ("turbo.ss" "swindle")
  ;;     ("html.ss" "swindle")))
  ;; (define drscheme-language-positions
  ;;   '(("Svindle" "Full Swindle")
  ;;     ("Svindle" "Swindle without CLOS")
  ;;     ("Svindle" "HTML Swindle")))
  ;; (define drscheme-language-numbers
  ;;   '((-900 0) (-900 1) (-900 2)))
  ;; (define drscheme-language-one-line-summaries
  ;;   '("Scheme with Full Swindle extensions"
  ;;     "Scheme with Swindle without the object system"
  ;;     "Scheme with the HTML and Swindle extensions"))
  ;; (define drscheme-language-urls
  ;;   '("http://www.barzilay.org/Swindle/"
  ;;     "http://www.barzilay.org/Swindle/"
  ;;     "http://www.barzilay.org/Swindle/"))
  ;;
  (define tools      '(("tool.ss")))
  (define tool-names '("Swindle"))
  (define tool-icons '(("swindle-icon.gif" "swindle" "icons")))
  (define tool-urls  '("http://www.barzilay.org/Swindle/"))
  )
