(module after-updating (lib "language.ss" "5L")
  (require (lib "nodes.ss" "5L"))
  (require-for-syntax (lib "capture.ss" "5L"))
  
  (provide after-updating)

  ;;; When a %node% ATTR is updated, we frequently need to redraw the node,
  ;;; pass the update to C++, or run some other small snippet of code.  This
  ;;; can be achieved with code like the following:
  ;;;
  ;;;   (after-updating at
  ;;;     (set-engine-pos! self (.at)))
  ;;;
  ;;;   (after-updating [style text]
  ;;;     (.invalidate))
  ;;;
  ;;; Note that we also provide a non-macro version of this API.  See
  ;;; %node% .after-updating.
  ;;;
  ;;; Code installed by AFTER-UPDATING won't do anything until _after_
  ;;; .initialize has finished, to prevent object initialization from
  ;;; accidentally triggering update handlers.
  (define-syntax (after-updating stx)
    (syntax-case stx ()
      [(_ [name ...] body ...)
       (quasisyntax/loc stx
         (#,(make-self #'(name ...)) .after-updating '(name ...)
                                                     (method () body ...)))]
      [(_ name body ...)
       (quasisyntax/loc stx
         (after-updating [name] body ...))]))

  (with-instance (%node% .class)
    ;;; The non-macro version of AFTER-UPDATING.
    (def (after-updating names meth)
      (foreach [name names]
        (define setter (symcat "set-" name "!"))
        (.advise-method 'after setter
                        (method (value)
                          (when (.initialized?)
                            (instance-exec self meth))))))
    )

  )
