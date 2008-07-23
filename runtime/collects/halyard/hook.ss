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

(module hook mzscheme
  
  (provide (rename my-make-hook make-hook)
           hook? hook-name
           hook-add-function! hook-remove-function!
           hook-functions
           call-hook-functions
           )
  
  (define-struct hook (name function-table) (make-inspector))
  
  (define (my-make-hook name)
    (make-hook name (make-hash-table)))
  
  (define (hook-add-function! hk tag function)
    ;; The function should have the parameters specified by the hook
    ;; creator.
    (hash-table-put! (hook-function-table hk) tag function))
  
  (define (hook-remove-function! hk tag)
    (hash-table-remove! (hook-function-table hk) tag))
  
  (define (hook-functions hk)
    ;; Return the hook functions in any old order.
    (hash-table-map (hook-function-table hk) (lambda (key value) value)))
  
  (define (call-hook-functions hk . args)
    ;; This is just a handy wrapper around hook-functions for callers
    ;; who don't care about return values.
    (let loop [[functions (hook-functions hk)]]
      (unless (null? functions)
        (apply (car functions) args)
        (loop (cdr functions))))
    #f)
  )