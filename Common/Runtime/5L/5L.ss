;;=========================================================================
;;  The 5L Programming Language
;;=========================================================================
;;  The actual 5L programming language, including both the API and the
;;  special syntax.

(module |5L| (lib "lispish.ss" "5L")
  (require (lib "5L-API.ss" "5L"))
  (require (lib "interpolate.ss" "5L"))

  (provide (all-from-except (lib "lispish.ss" "5L") #%datum))
  (provide (all-from (lib "5L-API.ss" "5L")))
  (provide (rename interpolating-#%datum #%datum))
  )
