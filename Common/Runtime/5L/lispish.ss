;;========================================================================
;;  Nice LISP Features Typically Missing From Scheme
;;========================================================================
;;  Scheme is a simplified, cleaned-up LISP dialect.  This is both good
;;  and bad--Common LISP contained a lot of unnecessary cruft, but it also
;;  contained some very useful features.
;;
;;  This used to be a nice, lighweight file implementing Common LISP-like
;;  features for Scheme, inspired by Eli's excellent Swindle modules.
;;  However, Eli's modules have finally been updated for use with PLT 2xx,
;;  so this file has been gutted.  See lispish-orig.ss for the original
;;  goodies.
;;
;;  Eric Kidd <eric.kidd@pobox.com>

(module lispish (lib "swindle.ss" "swindle")
  
  ;; Export the normal Swindle language, minus a few specific features
  ;; we override below.  We also omit 'while', because we have our
  ;; own definition later on.
  (provide (all-from-except (lib "swindle.ss" "swindle") while defclass))
  
  ;; Here are some additional LISP-like features we provide.
  (provide define-symbol-macro let-symbol-macro)
  
  ;; Set up Swindle to have a reasonable behavior for defclass. We're
  ;; trying to be as much like Dylan as possible, except also defining
  ;; classname?  as a predicate to test for objects of type
  ;; <classname>. We tried doing this the way Eli suggests, but it
  ;; breaks in all sorts of exciting ways. 
  (provide (rename lispish-defclass defclass))
  (define-syntax lispish-defclass
    (syntax-rules ()
      ((_ args ...) (defclass args ... 
                      :auto #t
                      :printer #t))))
  
  
  ;;----------------------------------------------------------------------
  ;; Symbol Macros and Generalized Setters
  ;;----------------------------------------------------------------------
  ;; A "symbol macro" is an identifier which expands into a more complex
  ;; expression:
  ;;
  ;;   (define-symbol-macro NAME EXPR)
  ;;   (let-symbol-macro ((NAME EXPR)...) BODY...)
  ;;   NAME => EXPR
  ;;
  ;; This allows library developers to create fancy kinds of variables--
  ;; in particular, "smart" variables that transparently update values
  ;; stored outside of Scheme.
  
  (define-syntax-set (define-symbol-macro let-symbol-macro)
    
     ;; define-symbol-macro and let-symbol-macro actually expand into
     ;; syntax definitions themselves.  So our macro will need to
     ;; contain syntax-case forms that generate *other*
     ;; syntax-case forms.  This function does the tricky bit. 
     (define (make-symbol-transformer id-and-expansion)
       (syntax-case id-and-expansion ()
         [(id expansion)
          ;; Return a two-item list containing the ID and the
          ;; syntax rules to bind it to.  (This two-item list can be
          ;; inserted directly into the expansion of both DEFINE- and
          ;; LET-SYMBOL-MACRO if we're clever.)
          #'(id
             (lambda (stx)
               (syntax-case stx ()
                 ;; Expand the symbol when it appears in the first position
                 ;; of a form.  This is standard Scheme.
                 [(_ . arg)
                  #'(expansion . arg)]
                 ;; Expand the symbol when it appears in any other position.
                 ;; This is an mzscheme extension to syntax-rules.
                 [_
                  #'expansion])))]))
     
     ;; Handy utility function: Unpack a syntax list, run map over it
     ;; and pack it back up again.
     (define (map-syntax func syntax-list)
       (datum->syntax-object syntax-list
                             (map func (syntax->list syntax-list))
                             syntax-list
                             syntax-list))
     
     (define (define-symbol-macro/proc stx)
       (syntax-case stx ()
         [(_ . binding)
          #`(define-syntax #,@(make-symbol-transformer #'binding))]))
      
     (define (let-symbol-macro/proc stx)
       (syntax-case stx ()
         [(_ bindings body ...)
          #`(let-syntax (#,@(map-syntax make-symbol-transformer #'bindings))
              body ...)])))
  
  )
