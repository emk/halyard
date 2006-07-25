(module url mzscheme
  (require (lib "unitsig.ss")
           "url-sig.ss"
           "url-unit.ss"
           "tcp-sig.ss"
           "tcp-unit.ss")
  (provide-signature-elements net:url^)

  (define-values/invoke-unit/sig
   net:url^
   (compound-unit/sig
     (import)
     (link
      [T : net:tcp^ (tcp@)]
      [U : net:url^ (url@ T)])
     (export (open U)))))

