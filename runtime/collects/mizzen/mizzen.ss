;; @BEGIN_LICENSE
;;
;; Mizzen - Scheme object system
;; Copyright 2006-2009 Trustees of Dartmouth College
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as
;; published by the Free Software Foundation; either version 2.1 of the
;; License, or (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;; 
;; You should have received a copy of the GNU Lesser General Public
;; License along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
;; USA.
;;
;; @END_LICENSE

;;========================================================================
;;  Mizzen language
;;========================================================================
;; 
;;  The mizzen language provides many features from Swindle, along with 
;;  its own, Smalltalk-style object system, and a few extra syntactic 
;;  enhancements.

(module mizzen (lib "swindle.ss" "swindle")
  
  ;;---------------------------------------------------------------------- 
  ;; Swindle Features
  ;;----------------------------------------------------------------------
  ;; This gets us keyword arguments, and support for CLOS-like objects.
  ;; We're trying to phase the CLOS-like stuff out in favor of the
  ;; Ruby-style objects, as defined in the next section.

  ;; Export the normal Swindle language, minus a few specific features
  ;; we override below.
  (provide (all-from-except (lib "swindle.ss" "swindle")
                            defclass box unbox define-syntax-parameter
                            ;; We replace these so ruby-objects.ss can
                            ;; implement a custom syntax for method
                            ;; dispatch.
                            #%app set!
                            ;; We get this from Swindle, but we want the
                            ;; version from ruby-objects.ss.
                            method object? class? instance-of? subclass?))
  
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

  ;; Set up Swindle's EQUALS? to work properly on lists. By default,
  ;; Swindle just uses EQUAL? to compare anything for which EQUALS? is
  ;; not defined on. This is fine for strings, symbols, numbers, and
  ;; so on, but causes problems when you have lists that contain
  ;; objects for which EQUALS? has been overridden.
  ;;
  ;; Taken from code kwasi added to swindle/extra.ss
  ;;
  ;; TODO - We should probably do something similar for vectors.
  (defmethod (equals? (x <list>) (y <list>))
    (cond ((and (null? x) (null? y))
           #t)
          ((or (null? x) (null? y))
           #f)
          (else
           (and (equals? (car x) (car y))
                (equals? (cdr x) (cdr y))))))
  
  
  ;;---------------------------------------------------------------------- 
  ;; Ruby/SmallTalk-style Features
  ;;---------------------------------------------------------------------- 

  (require "ruby-objects.ss")
  (provide (all-from-except "ruby-objects.ss"
                            method~ app~ set!~))

  (provide (rename method~ method)
           (rename app~ #%app)
           (rename set!~ set!)
           (rename object?~ object?)
           (rename class?~ class?)
           (rename instance-of?~ instance-of?)
           (rename subclass?~ subclass?))
  

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

  (provide define-symbol-macro let-symbol-macro)
  
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
  
  
  ;;---------------------------------------------------------------------- 
  ;; Generally useful utilities
  ;;---------------------------------------------------------------------- 
  
  (require "util.ss")
  (provide (all-from "util.ss"))
  )
