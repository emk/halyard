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
;;  DrScheme Editor Integration
;;  =========================================================================
;;  This file takes care of integrating our Halyard language with the
;;  DrScheme IDE.  We do this by implementing a variety of interfaces
;;  defined in the "PLT Tools: DrScheme Extension Manual", available
;;  in the DrScheme help desk.

(module info (lib "infotab.ss" "setup")

  ;; Specify the name of this extension.
  (define name "halyard")

  ;; Define a tool which installs our language.
  (define tools (list "tool.ss"))
  (define tool-icons (list '("5L.gif" "halyard")))
  (define tool-names (list "Halyard Multimedia Programming Language"))

  ;; Register our language.
  ;(define drscheme-language-modules (list '("halyard.ss" "halyard")))
  ;(define drscheme-language-positions
  ;  (list '("Halyard Multimedia Programming Language")))
  ;(define drscheme-language-one-line-summaries
  ;  (list "For use with FiveL.exe from the DMS Interactive Media Lab"))
  )
