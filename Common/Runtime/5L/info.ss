;;=========================================================================
;;  DrScheme Editor Integration
;;=========================================================================
;;  This file takes care of integrating our 5L language with the DrScheme
;;  IDE.  We do this by implementing a variety of interfaces defined in
;;  the "PLT Tools: DrScheme Extension Manual", available in the DrScheme
;;  help desk.

(module info (lib "infotab.ss" "setup")

  ;; Specify the name of this extension.
  (define name "5L")

  ;; Define a tool which installs our language.
  (define tools (list "5L-tool.ss"))
  (define tool-icons (list '("5L.gif" "5L")))
  (define tool-names (list "5L Multimedia Programming Language"))

  ;; Register our language.
  ;(define drscheme-language-modules (list '("5L.ss" "5L")))
  ;(define drscheme-language-positions
  ;  (list '("5L Multimedia Programming Language")))
  ;(define drscheme-language-one-line-summaries
  ;  (list "For use with FiveL.exe from the DMS Interactive Media Lab"))
  )
