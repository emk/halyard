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

(module trace-test mzscheme
  (require trace)
  
  (define (fact x)
    (if (= x 0)
        1
        (* x (fact (- x 1)))))
  
  (+ 1 (* 3 4))
  (display "Hi!\n")
  (display (fact (* 3 4)))
  (newline)
  (if (= 3 4)
       (display "uh-oh")
       (display "the world is good"))
  
  (with-tracing
   (+ 1 (* 3 4))
   (display "Hi!\n")
   (display (fact (* 3 4)))
   (newline)
   (if (= 3 4)
       (display "uh-oh")
       (display "the world is good")))
  
  (+ 1 (* 3 4))
  (display "Hi!\n")
  (display (fact (* 3 4)))
  (newline)
  (if (= 3 4)
       (display "uh-oh")
       (display "the world is good"))
  
  (define trace-str "")
  (set-trace-fn! (lambda (x) (set! trace-str 
                                   (string-append trace-str 
                                                  (string-append x "\n")))))
  (with-tracing
   (+ 1 (* 3 4))
   (display "Hi!\n")
   (display (fact (* 3 4)))
   (newline)
   (if (= 3 4)
       (display "uh-oh")
       (display "the world is good")))
  
  (display "\n\nTrace output:\n\n")
  (display trace-str))
