;; @BEGIN_LICENSE
;;
;; Halyard - Multimedia authoring and playback system
;; Copyright 1993-2008 Trustees of Dartmouth College
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

;;=========================================================================
;;  The Halyard Programming Language
;;  =========================================================================
;;  The actual Halyard programming language, including both the API
;;  and the special syntax.  Everything in this directory could
;;  probably stand a good refactoring; it's been accumulating cruft
;;  for a while.

(module halyard (lib "mizzen.ss" "mizzen")

  ;; We want to export most of lispish, but override a few definitions
  ;; locally to get some decidedly non-Scheme behavior.
  (provide (all-from-except (lib "mizzen.ss" "mizzen")
                            ;; We replace this.
                            #%top
                            ;; begin/var hacks (see below).
                            lambda define let unless when
                            ))
  
  ;; Load the formerly engine-independent portion of the API.
  (require (lib "api.ss" "halyard"))
  (provide (all-from (lib "api.ss" "halyard")))

  ;; Load the engine-dependent, subject-to-change API.
  (require (lib "elements.ss" "halyard"))
  (provide (all-from (lib "elements.ss" "halyard")))

  ;; These are not well-loved APIs, so we only include them fairly high up the
  ;; stack to try to avoid having any undocumented internal depedencies on
  ;; them.
  (require (lib "paths.ss" "halyard"))
  (provide (all-from (lib "paths.ss" "halyard")))
  (require (lib "card-sequence.ss" "halyard"))
  (provide (all-from (lib "card-sequence.ss" "halyard")))


  ;;=======================================================================
  ;;  begin/var Hacks
  ;;=======================================================================
  ;;  We want to redefine a number of the most common "body" macros to
  ;;  accept (var ...) declarations.

  (provide (rename lambda/var lambda)
           (rename define/var define)
           (rename let/var let)
           (rename unless/var unless)
           (rename when/var when))

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

  (define-syntax unless/var
    (syntax-rules ()
      [(unless/var cond body ...)
       (unless cond (begin/var body ...))]))

  (define-syntax when/var
    (syntax-rules ()
      [(when/var cond body ...)
       (when cond (begin/var body ...))]))
  
  
  ;;=======================================================================
  ;;  Relative Path (@foo) Syntax Support
  ;;=======================================================================

  (provide (rename hacked-#%top #%top))

  (define-syntax (hacked-#%top stx)
    ;; Handle relative path transformation.
    (syntax-case stx ()
      [(_ . varname)

       ;; Look for symbols matching the regex '^@[A-Za-z0-9_]'.
       (let [[v (syntax-object->datum #'varname)]]
         (and (symbol? v)
              (let [[vstr (symbol->string v)]]
                (and (>= (string-length vstr) 2)
                     (eq? #\@ (string-ref vstr 0))
                     (let [[c (string-ref vstr 1)]]
                       (or (eq? c #\_)
                           (char-alphabetic? c)
                           (char-numeric? c)))))))

       ;; Transform @foo -> (@ foo).
       (let* [[str (symbol->string (syntax-object->datum #'varname))]
              [name (substring str 1 (string-length str))]]
         (quasisyntax/loc stx (@ #,(string->symbol name))))]

      [(_ . varname)
       ;; Fallback case--use our existing version of #%top.
       (syntax/loc stx (#%top . varname))]))

  )
