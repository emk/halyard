;;=========================================================================
;;  Fake #%fivel-engine Module for DrScheme
;;=========================================================================
;;  When running under DrScheme, we need to provide a low-budget immitation
;;  of our standalone engine.

(module #%fivel-engine mzscheme
  (provide %call-5l-prim)
  
  (define (%call-5l-prim name . args)
    (error "No 5L primitives are currently available inside DrScheme.")))
