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

;;; This module is used to resolve paths files under the scripts/
;;; directory.
(module external-nodes (lib "mizzen.ss" "mizzen")

  (provide split-node-name node-name->module-name
           external-group external-card)

  ;;; Given a node name such a /foo/bar, return a list of the individual
  ;;; components in the node name.  For the root node |/|, return the empty
  ;;; list.
  (define (split-node-name name)
    (if (eq? name '|/|)
      '()
      (let [[components (split (regexp "/") (symbol->string name))]]
        (if (or (< (length components) 2)
                (not (equal? "" (car components)))
                (member "" (cdr components))
                (member "." (cdr components))
                (member ".." (cdr components)))
          (error (cat "Invalid node name: " name))
          (cdr components)))))
                

  ;;; Convert a node name of the form /foo/bar to the corresponding (file
  ;;; ...) form.
  (define (node-name->module-name name)
    `(file ,(string-append (join "/" (split-node-name name)) ".ss")))

  ;;; Declare that a group should be loaded from an external file.
  (define-syntax external-group
    (syntax-rules ()
      [(_ name)
       (namespace-require (node-name->module-name 'name))]))

  ;;; Declare that a card should be loaded from an external file.
  (define-syntax external-card
    (syntax-rules ()
      [(_ name)
       (external-group name)]))
  )
