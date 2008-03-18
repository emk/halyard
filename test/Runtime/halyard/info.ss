;;=========================================================================
;;  DrScheme Editor Integration
;;  =========================================================================
;;  This file takes care of integrating our Halyard language with the
;;  DrScheme IDE.  We do this by implementing a variety of interfaces
;;  defined in the "PLT Tools: DrScheme Extension Manual", available
;;  in the DrScheme help desk.

(module info (lib "infotab.ss" "setup")

  ;; Specify the name of this extension.
  (define name "halyard")

  ;; Define a tool which installs our language.
  (define tools (list "tool.ss"))
  (define tool-icons (list '("5L.gif" "halyard")))
  (define tool-names (list "Halyard Multimedia Programming Language"))

  ;; Register our language.
  ;(define drscheme-language-modules (list '("halyard.ss" "halyard")))
  ;(define drscheme-language-positions
  ;  (list '("Halyard Multimedia Programming Language")))
  ;(define drscheme-language-one-line-summaries
  ;  (list "For use with FiveL.exe from the DMS Interactive Media Lab"))
  )
