;;=========================================================================
;;  Capturing Variables in Macro Expansion
;;=========================================================================
;;  On rare occasions, we'd like to introduce new variables during a
;;  macro expansion, and have them automatically appear within nested code.
;;  For example, the classic "if-it" macro:
;;
;;    (if-it (node-parent node) (node-name it) "No parent.")
;;
;;  In this macro, "it" is bound to the result of (node-parent node).  In
;;  the best of times, this trick requires some abuse of SYNTAX-CASE.
;;  Unfortunatly, PLT203 makes this even harder than usual because of
;;  of how it handles syntax information for ellipsis patterns.
;;
;;  We define two functions:
;;    
;;    (make-capture-var stx name)
;;    (make-capture-var/ellipsis ellispis-stx name)
;;
;;  These will will create a variable in the context of STX using the
;;  symbol NAME to specify what to capture.
;;
;;  The latter form *must* be used if and only if ELLISPIS-STX was matched
;;  as ". body" or "body ..." in a syntax pattern.  This is because PLT203
;;  tends to misplace syntax information during either kind of match, and
;;  we have to try various tricky techniques to extract it.
;;
;;  Example:
;;
;;    (define-syntax (if-it stx)
;;      (syntax-case stx ()
;;        [(if-it cond then . else)
;;         (with-syntax [[it (make-capture-var #'if-it 'it)]]
;;           #'(let [[it cond]] (if it then . else)))]))
;;
;;  Unless you're doing something really odd (perhaps involving helper
;;  macros), you'll usually want to use the macro name as the first
;;  argument to MAKE-CAPTURE-VAR.
;;
;;  Eric Kidd

(module capture mzscheme

  (provide make-capture-var make-capture-var/ellipsis)

  (define (make-capture-var stx name)
    (datum->syntax-object stx name))

  ;; XXX - There's a gross hack here--we have to use the context of
  ;; (car (syntax-e #'body)) instead of just #'body, because PLT203
  ;; loses all syntax information on pattern variables of the form
  ;; '. body' and 'body ...'.  Aiyeee!  (If #'body isn't a syntax
  ;; pair, it doesn't really matter what context we pick.)
  (define (make-capture-var/ellipsis stx name)
    (datum->syntax-object (if (pair? (syntax-e stx))
                              (car (syntax-e stx))
                              stx)
                          name))

  )
