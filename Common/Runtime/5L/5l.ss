;;=========================================================================
;;  The 5L Programming Language
;;=========================================================================
;;  The actual 5L programming language, including both the API and the
;;  special syntax.

(module 5L (lib "lispish.ss" "5L")
  (require (lib "api.ss" "5L"))

  ;; We want to export most of lispish, but override a few definitions
  ;; locally to get some decidedly non-Scheme behavior.
  (provide (all-from-except (lib "lispish.ss" "5L")
                            ;; begin/var hacks.
                            lambda define let
                            ))

  (provide (all-from (lib "api.ss" "5L")))


  ;;=======================================================================
  ;;  begin/var Hacks
  ;;=======================================================================
  ;;  We want to redefine a number of the most common "body" macros to
  ;;  accept (var ...) declarations.
  
  (provide (rename lambda/var lambda)
           (rename define/var define)
           (rename let/var let))

  (define-syntax lambda/var
    (syntax-rules ()
      [(lambda/var args body ...)
       (lambda args (begin/var body ...))]))

  ; define/var comes from begin-var.ss.

  (define-syntax let/var
    (syntax-rules ()
      [(let/var [decl ...] body ...)
       (let [decl ...] (begin/var body ...))]
      [(let/var loop-name [decl ...] body ...)
       (let loop-name [decl ...] (begin/var body ...))]))

  )
