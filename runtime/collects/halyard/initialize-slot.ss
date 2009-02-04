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

(module initialize-slot (lib "mizzen.ss" "mizzen")
  (require (lib "util.ss" "mizzen"))
        
  (with-instance %class%
    ;; TODO - Experimental code for .initialize-slot.  We need to think
    ;; about this, possibly redesign it, and--in any case--make it take a
    ;; method thunk as an initializer instead of a value.
    (def (initialize-slot name init-value)
      (.define-method (symcat "set-" name "!")
        (method (val)
          (if (.initialized?)
            (error "Tried to set slot " name " on " self " using the setter "
                   "after initialization.")
            (set! (slot name) val))))
      (.attr-initializer name (method () init-value) #f))))