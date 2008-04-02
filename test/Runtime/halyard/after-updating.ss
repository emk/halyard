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

(module after-updating (lib "language.ss" "mizzen")
  (require (lib "nodes.ss" "halyard"))
  (require (lib "util.ss" "mizzen"))
  (require-for-syntax (lib "capture.ss" "mizzen"))
  
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
  (define-syntax after-updating
    (syntax-rules ()
      [(_ [name ...] body ...)
       (.after-updating '(name ...)
                        (method () body ...))]
      [(_ name body ...)
       (after-updating [name] body ...)]))

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
