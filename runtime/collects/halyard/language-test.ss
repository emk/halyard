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

(module lispish-test (lib "mizzen.ss" "mizzen")
  
  ;; Traditional functions.
  (define (t1) 'ok)
  (define (t2 . x) x)
  (define (t3 x . y) y)
  (define (t4 x y) y)
  
  ;; Optional keywords.
  (define (o1 &opt x) x)
  (define (o2 x &opt (y 1) (z (* y 2))) (list y z))
  
  ;; Rest arguments.
  (define (r1 &rest x) x)
  (define (r2 x &rest y) y)
  
  ;; Keyword arguments.
  (define (k1 &key x y) (list x y))
  (define (k2 &key (x 'foo) (y 'bar)) (list x y))
  (define (k3 &key (x1 :x 'foo) (y1 :y 'bar)) (list x1 y1))
  (define (k4 n &key (x n) (y (* 2 n))) (list x y))
  (define (k5 &key x &rest y) (list x y))
 
  ;; Generalized setters.
  (define test-list (list 1 2 3))
  (set! (car test-list) 0)
  
  ;; Test of identifer macros.
  (define (get-magic-function name func)
    (display "Getting magic function: ")
    (display name)
    (newline)
    func)
  (define-symbol-macro magic+ (get-magic-function "+" +))
  ;magic+
  ;(magic+ 1 2)
  ;(let-symbol-macro ((magic- (get-magic-function "-" -)))
  ;  magic-
  ;  (magic- 3 1))
  
  (define (magic-var str)
    (display "Getting value of ")
    (display str)
    (newline)
    'magic)
  
  (define (set-magic-var! value str)
    (display "Setting value of ")
    (display str)
    (display " to: ")
    (display value)
    (newline))
  
  (define-symbol-macro magic-x (magic-var "magic-x"))
  magic-x
  (set! magic-x 'more-magic)
  
  )