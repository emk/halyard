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

(module define-node-helper (lib "mizzen.ss" "mizzen")
  (require (lib "nodes.ss" "halyard/private"))
  (require-for-syntax (lib "util.ss" "mizzen"))
  (require (lib "tags.ss" "halyard/private"))
  (require (lib "indent.ss" "halyard/private"))

  (provide define-node-helper)

  ;;; If you write:
  ;;;
  ;;;   (define-node-helper menu-item (y text jump-to) %menu-item%)
  ;;;
  ;;; ..then the following:
  ;;;
  ;;;   (menu-item name (80 "Hello" @something :shown? #f)
  ;;;     (def (click) ...))
  ;;;
  ;;; ...will expand to:
  ;;;
  ;;;   (elem name (%menu-item% :y 80 :text "Hello" :jump-to @something
  ;;;                           :shown? #f)
  ;;;     (def (click) ...))
  ;;;
  ;;; (Well, it actually expands to DEFINE-NODE instead of ELEM, which
  ;;; means use can use it for cards and other types of nodes as well.)
  ;;;
  ;;; TODO - This would all be much nicer if we had .ELEM, which we should
  ;;; probably think about implementing someday (assuming we decide to
  ;;; ever allow anonymous class declarations).
  (define-syntax (define-node-helper stx)
    (define (names->keys+names names-stx)
      (datum->syntax-object stx
        (let recurse [[names (syntax->list names-stx)]]
          (if (null? names)
              '()
              (let [[symbol (syntax-object->datum (car names))]]
                (unless (symbol? symbol)
                  (raise-syntax-error 'define-node-helper
                                      "Malformed argument list" names-stx))
                (list* (symcat ":" symbol) (car names)
                       (recurse (cdr names))))))))
    
    (syntax-case stx []
      [(_ helper-name (args ...) class)
       (quasisyntax/loc stx
         (begin
           (define-syntax helper-name
             (syntax-rules ()
               [(_ name (args ... . keys) . body)
                (define-node name (class #,@(names->keys+names #'(args ...)) 
                                    . keys) 
                  . body)]))))]))
  
  (define-syntax-help define-node-helper 
    (define-node-helper helper-name (args ...) class))
  
  (define-syntax-tagger define-node-helper
    [(_ helper-name (args ...) class)
     'syntax helper-name 2 (helper-name name (args ... &rest keys) 
                                        body (... ...))])
  )