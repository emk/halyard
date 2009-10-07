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

(module elements (lib "halyard.ss" "halyard")
  (require (lib "kernel.ss" "halyard/private"))


  ;;;======================================================================
  ;;;  Pop up menu
  ;;;======================================================================

  (provide %pop-up-menu% pop-up-menu)

  ;;; EXPERIMENTAL: A pop-up menu from which the user may choose an item.
  ;;; The API to this class may change in a future release of Halyard.
  (define-class %pop-up-menu% (%widget%)
    (attr items :type <list> #| of strings |#)

    (def (create-engine-node)
      (call-prim 'PopUpMenu (.full-name)
                 (make-node-event-dispatcher self)
                 (parent->card self (.rect))
                 (.items)))

    ;;; Get the 0-based index of the currently selected item.
    (def (selection)
      (call-prim 'PopUpMenuGetSelection (.full-name)))
    )

  ;;; Declare a %pop-up-menu% object.
  (define-node-helper pop-up-menu (rect items) %pop-up-menu%)

  )

