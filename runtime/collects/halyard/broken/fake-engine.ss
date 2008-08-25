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
;;  Fake #%fivel-engine Module for DrScheme
;;=========================================================================
;;  When running under DrScheme, we need to provide a low-budget immitation
;;  of our standalone engine so users can test simple code.

(module #%engine-primitives mzscheme
  (provide %call-prim)
  
  (define (sym-or-str-arg string-or-symbol)
    (cond
      [(symbol? string-or-symbol) string-or-symbol]
      [(string? string-or-symbol) (string->symbol string-or-symbol)]
      [else
       (error "Expected string or symbol")]))
  
  (define (value->string value)
    (if (string? value)
	value
	(let ((str-port (open-output-string)))
	  (write value str-port)
	  (get-output-string str-port))))

  (define *variable-table* (make-hash-table))
  
  (define (prim-variableinitialized variable)
    (call-with-current-continuation
     (lambda (return)
       (let [[val (hash-table-get *variable-table* variable
				  (lambda () (return #f)))]]
	 #t))))

  (define (prim-get variable)
    (define (uninitialized-error)
      (error (string-append "Uninitialized variable: "
			    (symbol->string variable))))
    (hash-table-get *variable-table* variable uninitialized-error))

  (define (prim-settyped variable type value)
    ;; This behavior is silly, but it mimicks the engine.
    (hash-table-put! *variable-table* variable value)
    (void))
    
  (define (prim-log facility msg level)
    (display facility)
    (display ": ")
    (display msg)
    (newline)
    (when (eq? (sym-or-str-arg level) 'fatalerror)
      ;; Try to do something semi-useful with fatal errors.  These should
      ;; really abort all further execution, but this will do for now.
      (error msg))
    (void))
    
  (define (%call-prim name . args)
    (case name

      ;; See if a primitive is available.
      [[haveprimitive]
       (case (car args)
         [[haveprimitive variableinitialized get settyped log setwindowtitle
	   defstyle header keybind loadpal registereventdispatcher
	   setimagecachesize registercursor]
          #t]
         [else
          #f])]

      ;; Primitives with actual implementations.
      [[variableinitialized] (apply prim-variableinitialized args)]
      [[get] (apply prim-get args)]
      [[settyped] (apply prim-settyped args)]
      [[log] (apply prim-log args)]
      
      ;; The do-nothing primitives.
      [[setwindowtitle defstyle header keybind loadpal registereventdispatcher
	setimagecachesize registercursor]
       (void)]

      ;; 
      [else
       (error (string-append "The engine primitive "
                             (symbol->string name)
                             " is not emulated by DrScheme."))])))