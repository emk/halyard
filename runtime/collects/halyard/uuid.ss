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

(module uuid (lib "halyard.ss" "halyard")
  (require (lib "kernel.ss" "halyard/private"))
  
  (provide uuid uuid?)

  ;;; Generate a universally unique identifier (UUID).
  ;;;
  ;;; SECURITY - This does not necessarily generate cryptographically
  ;;; secure UUIDs!  On Windows, we call the standard UuidCreate function.
  ;;; On other platforms, we rely on boost::uuid's built-in random
  ;;; generator, which has probably never been audited for security.  So
  ;;; (particularly on non-Windows platforms), it's probably possible for
  ;;; an attacker to guess these UUID values.
  (define (uuid)
    (call-prim 'GenerateUuid))

  (define $uuid-regexp
    (pregexp "^[0-9a-fA-F]{8}-([0-9a-fA-F]{4}-){3}[0-9a-fA-F]{12}$"))

  ;;; Return true iff value is a string in normal UUID format.
  (define (uuid? value)
    (and (string? value) (regexp-match $uuid-regexp value)))

  )
