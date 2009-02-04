;; @BEGIN_LICENSE
;;
;; Mizzen - Scheme object system
;; Copyright 2006-2009 Trustees of Dartmouth College
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

(module util (lib "swindle.ss" "swindle")
  
  (require "begin-var.ss")
  

  ;;=======================================================================
  ;;  Assertions
  ;;=======================================================================

  (provide %assert assert set-fatal-error-function!)

  (define fatal-error error)
  (define (set-fatal-error-function! fun)
    (set! fatal-error fun))
  
  (define (%kernel-assert fatal? label value)
    (when (not value)
      (let [[message (cat "Assertion failure: " label)]]
        (if fatal?
            (fatal-error message)
            (error message)))))
  
  ;;; This is an ASSERT for engine developers: It crashes the engine and
  ;;; probably e-mails a bug report.  Don't use this to check for
  ;;; regular user errors; use it to check for things which should
  ;;; never happen no matter how broken the user's script is.
  (define-syntax %assert
    (syntax-rules ()
      [(%assert cond)
       (%kernel-assert #t 'cond cond)]))

  ;;; This is an ASSERT for scriptors: It doesn't crash the engine, and
  ;;; it lets them fix their problem.
  (define-syntax assert
    (syntax-rules ()
      [(assert cond)
       (%kernel-assert #f 'cond cond)]))


  ;;=======================================================================
  ;;  Utility Functions
  ;;=======================================================================

  (provide foreach member? value->string cat symcat keyword-name
           symbol->keyword setter-name? setter-name->keyword
           hash-table-has-key? label with-values curry fn)

  ;;; Run a body once for each item in a list.
  ;;;
  ;;; @syntax (foreach [name list] body ...)
  ;;; @param NAME name The variable to use as the item name.
  ;;; @param LIST list The list from which to get the items.
  ;;; @param BODY body The code to run for each list item.
  (define-syntax foreach
    (syntax-rules (cons)
      [(foreach [(cons key value) alist] body ...)
       (foreach [pair alist]
         (let [[key (car pair)] [value (cdr pair)]]
           (begin/var body ...)))]
      [(foreach [[key value] hash] body ...)
       (hash-table-for-each hash (lambda (key value) body ...))]
      [(foreach [name lst] body ...)
       (let loop [[remaining lst]]
         (unless (null? remaining)
           (let [[name (car remaining)]]
             (begin/var body ...))
           (loop (cdr remaining))))]))

  ;;; Return #f if and only if ITEM appears in LIST.  Uses EQUAL? to
  ;;; perform the comparison.
  (define (member? item list)
    (if (null? list)
        #f
        (if (equal? item (car list))
            #t
            (member? item (cdr list)))))
  
  ;;; Convert any Scheme value to a string.
  (define (value->string value)
    (cond 
      ((string? value) value)
      ((object? value) (object->string value))
      (else
       (let ((str-port (open-output-string)))
         (write value str-port)
         (get-output-string str-port)))))
  
  ;;; Convert VALUES to strings and concatencate the result.
  (define (cat . values)
    (if (not (null? values))
        (string-append (value->string (car values)) (apply cat (cdr values)))
        ""))

  ;;; Convert VALUES to strings, concatenate the result, and convert it
  ;;; to a symbol.
  (define (symcat . values)
    (string->symbol (apply cat values)))

  ;;; Given a Swindle keyword of the form ":foo", strip the leading colon
  ;;; and return a symbol.
  (define (keyword-name value)
    (assert (keyword? value))
    (let [[str (symbol->string value)]]
      (string->symbol (substring str 1 (string-length str)))))

  ;;; Convert a regular symbol to a keyword object by preprending a colo
  (define (symbol->keyword symbol)
    (symcat ":" symbol))

  (define $setter-name-regexp (regexp "^set-([^!]+)!$"))

  ;;; Is SYMBOL a name of the form "set-...!"?
  (define (setter-name? symbol)
    (regexp-match? $setter-name-regexp (symbol->string symbol)))

  ;;; Convert a name of the form "set-...!" to the keyword ":...".
  (define (setter-name->keyword symbol)
    (define match (regexp-match $setter-name-regexp (symbol->string symbol)))
    (assert match)
    (symbol->keyword (string->symbol (cadr match))))

  ;;; Return #t if and only if KEY appears in TABLE.
  (define (hash-table-has-key? table key)
    (define result #t)
    (hash-table-get table key (lambda () (set! result #f)))
    result)

  ;;; Define a function NAME which can be called from any point within the
  ;;; LABEL construct to exit immediately from the LABEL.  The function
  ;;; will remain valid only until the LABEL is exited, but may be passed
  ;;; to subroutines, stored in global variables, etc., during that time.
  ;;;
  ;;; @syntax (label name body ...)
  ;;; @param NAME name The name of the function which can be called to
  ;;;   exit the body.
  ;;; @param BODY body The code within which the exit procedure may be called.
  (define-syntax label
    (syntax-rules ()
      [(label name body ...)
       (call-with-escape-continuation (lambda (name)
                                        (begin/var body ...)))]))

  ;;; Bind the multiple return values of EXPR to VALUES (a parameter list),
  ;;; and call BODY.
  (define-syntax with-values
    (syntax-rules ()
      [(with-values [values expr] body ...)
       (call-with-values (lambda () expr) (lambda values body ...))]))
  
  ;;; A Dylan-style "curry", not be confused with Haskell's version.
  ;;; Returns a new function, with the first arguments of F filled in with
  ;;; values from ARGS1.
  (define (curry f . args1)
    (lambda args2
      (apply f (append args1 args2))))

  ;;; Create an anonymous function object (which can be passed as a
  ;;; callback to many routines).  This is just an alias for Scheme's
  ;;; standard 'lambda' form.
  ;;;
  ;;; @syntax (fn arglist body ...)
  ;;; @param ARGLIST arglist A list of Scheme function parameters.
  ;;; @param BODY body The body of the function.
  (define-syntax fn
    (syntax-rules ()
      [(fn arglist code ...)
       (lambda arglist (begin/var code ...))]))


  ;;=======================================================================
  ;;  Split and Join
  ;;=======================================================================
  ;;  SPLIT and JOIN functions loosely inspired by Perl, but with slightly
  ;;  more regular semantics.
  ;;
  ;;  Performance: You should assume that these functions are slightly
  ;;  slower than regexp-match and string-append.

  (provide split join)

  ;;; Given a regular expression PATTERN and a string, split the string
  ;;; into pieces using PATTERN as a delimiter.  We differ from the Perl
  ;;; and Ruby version of this function in that we retain trailing empty
  ;;; fields.  Note that zero-width delimiters and string PATTERNs are
  ;;; currently undefined.
  (define (split pattern str)
    (assert (or (regexp? pattern) (byte-regexp? pattern)))
    (let [[matches (regexp-match-positions pattern str)]]
      (if (not matches)
        ;; No match, so just return our string.
        (list str)
        ;; Got a match, so split there and recurse on what follows.
        (let [[start-k (car (first matches))]
              [end-k (cdr (first matches))]]
          (cons (substring str 0 start-k)
                (split pattern (substring str end-k)))))))

  ;;; Join the elements of LST together using STR as a delimiter.
  (define (join str lst)
    ;; Build up a list of strings to join, and then attempt to do the join
    ;; all at once.  This should help reduce the amount of time spent
    ;; copying and recopying strings.
    (apply string-append
           (let recurse [[str str] [lst lst]]
             (cond
              [(null? lst)
               '()]
              [(null? (cdr lst))
               lst]
              [else
               (list* (car lst) str (recurse str (cdr lst)))]))))
  )
