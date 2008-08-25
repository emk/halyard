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

;;; Extra names which need to be exported into the top-level scope.  There
;;; aren't many of these, because the top-level scope isn't used in
;;; ordinary code, which always resides in a module.  But any functions
;;; which need to be called on Halyard startup using '-e' must be
;;; explicitly exported.
;;;
;;; This module is used by halyard/loader/stage2.ss.
(module top-level (lib "halyard.ss" "halyard")

  ;; Make the command-line test driver available from the command line.
  (require (lib "halyard-unit.ss" "halyard"))
  (provide command-line-test-driver)

  )
