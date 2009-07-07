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

;;; Various configurable defaults which can make Halyard more compatible
;;; with earlier versions.
(module compatibility-defaults (lib "mizzen.ss" "mizzen")
  (require (lib "util.ss" "halyard/private"))


  ;;=======================================================================
  ;;  API
  ;;=======================================================================

  (provide compatibility-default set-compatibility-default!)

  ;; A table of defaults.
  (define *defaults* (make-hash-table))

  ;; Used internally to detect non-existant features.
  (define $no-such-key '(no such key))

  ;;; Get a compatibility default.
  (define (compatibility-default key)
    (define result (hash-table-get *defaults* key $no-such-key))
    (if (eq? result $no-such-key)
      (error (cat "Unknown compatibility default: " key))
      result))

  ;;; Set a compatibility default.  Will disable a developer-only warning
  ;;; if a non-existant default is set.
  (define (set-compatibility-default! key value)
    (unless (hash-table-has-key? *defaults* key)
      (warn "Setting unknown compatibility default '" key "' to " value))
    (hash-table-put! *defaults* key value))


  ;;=======================================================================
  ;;  Defaults
  ;;=======================================================================

  ;; Z-order and visibility.
  (hash-table-put! *defaults* 'has-legacy-z-order-and-visibility? #t)

  )