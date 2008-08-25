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

(module interpolate (lib "mizzen.ss" "mizzen")
  (require (lib "util.ss" "mizzen"))
  (require (lib "types.ss" "halyard/private"))

  (provide number->integer interpolate-value)

  ;;; Convert any number to an integer.  Typically needed for use with
  ;;; INTERPOLATE-VALUE and ANIMATE.
  (define (number->integer n)
    (inexact->exact (round n)))

  (define (interpolate-float fraction from to)
    (+ from (* fraction (- to from))))

  ;;; Return a value FRACTION percent of the distance between FROM and
  ;;; TO.
  (defgeneric (interpolate-value (fraction <real>)
                                 (from <object>) (to <object>)))

  (defmethod (interpolate-value (fraction <real>)
                                (from <integer>) (to <integer>))
    (number->integer (interpolate-float fraction from to)))

  (defmethod (interpolate-value (fraction <real>) 
                                (from <real>) (to <real>))
    (interpolate-float fraction from to))

  (defmethod (interpolate-value (fraction <real>)
                                (from <point>) (to <point>))
    (elem-map-2 (curry interpolate-value fraction) from to))

  (defmethod (interpolate-value (fraction <real>)
                                (from <rect>) (to <rect>))
    (elem-map-2 (curry interpolate-value fraction) from to))

  (defmethod (interpolate-value (fraction <real>)
                                (from <color>) (to <color>))
    (elem-map-2 (curry interpolate-value fraction) from to))

  )
