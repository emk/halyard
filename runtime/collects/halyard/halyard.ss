;; @BEGIN_LICENSE
;;
;; Halyard - Multimedia authoring and playback system
;; Copyright 1993-2009 Trustees of Dartmouth College
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
                            lambda λ define let unless when
                            ))
  
  ;; Load the formerly engine-independent portion of the API.
  (require (lib "api.ss" "halyard/private"))
  (provide (all-from (lib "api.ss" "halyard/private")))

  ;; Load the engine-dependent, subject-to-change API.
  (require (lib "elements.ss" "halyard/private"))
  (provide (all-from (lib "elements.ss" "halyard/private")))

  ;; Make sure that this code has been loaded before we start defining any
  ;; user code that might need to set SKIP-WHEN-JUMPING-TO-EACH-CARD?.  We
  ;; don't need to provide any of the actual identifiers from this file,
  ;; however.
  (require (lib "jump-to-each-card.ss" "halyard/private"))

  ;; These are not well-loved APIs, so we only include them fairly high up the
  ;; stack to try to avoid having any undocumented internal depedencies on
  ;; them.
  (require (lib "paths.ss" "halyard/private"))
  (provide (all-from (lib "paths.ss" "halyard/private")))
  (require (lib "card-sequence.ss" "halyard/private"))
  (provide (all-from (lib "card-sequence.ss" "halyard/private")))


  ;;=======================================================================
  ;;  begin/var Hacks
  ;;=======================================================================
  ;;  We want to redefine a number of the most common "body" macros to
  ;;  accept (var ...) declarations.

  (provide (rename lambda/var lambda)
           (rename λ/var λ)
           (rename define/var define)
           (rename let/var let)
           (rename unless/var unless)
           (rename when/var when))

  (define-syntax lambda/var
    (syntax-rules ()
      [(lambda/var args body ...)
       (lambda args (begin/var body ...))]))

  (define-syntax λ/var
    (syntax-rules ()
      [(λ/var args body ...)
       (lambda/var args body ...)]))

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

  (define (find-static-node-or-error path)
    (or (find-node path #f)
        (error "Can't find static node" path)))

  (begin-for-syntax 
    (define path-re (regexp "^@[a-z/]"))
    (define static-node-re (regexp "^/[a-z]")))

  (define-syntax (hacked-#%top stx)
    
    ;; Turn a symbol syntax into a string, and match it against 
    ;; the given regular expression.
    (define (symbol-match syn regex)
      (let [[v (syntax-object->datum syn)]]
        (and (symbol? v)
             (regexp-match regex (symbol->string v)))))

    (syntax-case stx ()
      ;; Handle relative or absolute path transformation.
      [(_ . varname)
       ;; Look for symbols matching the regex '^@[a-z/]'.
       (symbol-match #'varname path-re)

       ;; Transform @foo -> (@ foo).
       (let* [[str (symbol->string (syntax-object->datum #'varname))]
              [name (substring str 1 (string-length str))]]
         (quasisyntax/loc stx (@ #,(string->symbol name))))]
      
      ;; Handle immediate static node resolution.
      [(_ . varname)
       ;; Look for symbols matching the regex '^/[a-z]'
       (symbol-match #'varname static-node-re)
       
       ;; transform paths like /foo/bar/baz to calls to 
       ;; find-static-node-or-error.
       (quasisyntax/loc stx (find-static-node-or-error
                             (quote #,(syntax-object->datum #'varname))))]
      
      [(_ . varname)
       ;; Fallback case--use our existing version of #%top.
       (syntax/loc stx (#%top . varname))]))

  )
