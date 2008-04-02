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

;;=========================================================================
;;  Debugging Support for Local Variables
;;=========================================================================
;;  This a nasty hack for accessing the values of local variables for
;;  debugging purposes.  A real debugger would be *much* better, but PLT
;;  doesn't really have one yet.
;;
;;  Use:
;;    (define foo 1)
;;    (define foo 2)
;;    (debug-vars foo bar)
;;    (dv foo)
;;    (set! (dv foo) 2)

(module debug-var (lib "halyard.ss" "halyard")
  
  (provide *debug-vars* debug-vars dv set-dv!)

  (define *debug-vars* '())
  
  (defclass <debug-var> ()
    name
    getter
    setter)

  (define (register-debug-vars! vars)
    (set! *debug-vars* vars))

  (define (find-debug-var name vars)
    (cond
     [(null? vars)
      (error (cat name " is not registered as a debug-var"))]
     [(eq? name (debug-var-name (car vars)))
      (car vars)]
     [#t
      (find-debug-var name (cdr vars))]))

  (define (debug-var name)
    ((debug-var-getter (find-debug-var name *debug-vars*))))

  (define (set-debug-var! name value)
    ((debug-var-setter (find-debug-var name *debug-vars*)) value))

  (define-syntax debug-vars-helper
    (syntax-rules []
      [(debug-vars-helper)
       '()]
      [(debug-vars-helper var vars ...)
       (cons (make-debug-var 'var (fn () var) (fn (val) (set! var val)))
             (debug-vars-helper vars ...))]))

  ;;; Declare a set of debug variables.  Clears the previous set.
  (define-syntax debug-vars
    (syntax-rules []
      [(debug-vars vars ...)
       (register-debug-vars! (debug-vars-helper vars ...))]))

  ;;; Access the value of a local variable for debugging purposes.
  (define-syntax dv
    (syntax-rules []
      [(dv var)
       (debug-var 'var)]))

  ;;; Set the value of a local variable for debugging purposes.
  (define-syntax set-dv!
    (syntax-rules []
      [(set-dv! var value)
       (set-debug-var! 'var value)]))

  )