;;  LISPish MzScheme Macros
;;  Copyright (C) 2002 Eric Kidd
;;
;;  This library is free software; you can redistribute it and/or
;;  modify it under the terms of the GNU Lesser General Public
;;  License as published by the Free Software Foundation; either
;;  version 2.1 of the License, or (at your option) any later version.
;;
;;  This library is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  Lesser General Public License for more details.
;;
;;  You should have received a copy of the GNU Lesser General Public
;;  License along with this library; if not, write to the Free Software
;;  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA


;;========================================================================
;;  Nice LISP Features Typically Missing From Scheme
;;========================================================================
;;  Scheme is a simplified, cleaned-up LISP dialect.  This is both good
;;  and bad--Common LISP contained a lot of unnecessary cruft, but it also
;;  contained some very useful features.  In this module, I attempt to
;;  re-introduce some of the most useful Common LISP features, but in a
;;  simplified form.  I borrow a bit from Dylan (an infix, softly-typed
;;  programming language inspired by LISP).  Special thanks go to Eli
;;  Barzilay, whose clever mzscheme extensions encouraged me to push the
;;  limits.
;;
;;  My goal here is not to create the ultimate "hacker's Scheme", but to
;;  include a well-chosen set of flexible, convenient extensions.  I've
;;  omitted lots of fun features in the name of simplicity.
;;
;;  A primary goal of this module is to remain interface compatible
;;  with Eli Barzilay's Swindle library, which he is currently upgrading
;;  to run under PLT 200.
;;
;;  Eric Kidd <eric.kidd@pobox.com>

(module lispish (lib "swindle.ss" "swindle")
  
  ;; Import DEFINE-SYNTAX-SET from mzlib, but nothing else.  (I can't
  ;; find any way to do this except by using RENAME.)
  ;;(require (rename (lib "etc.ss" "mzlib")
  ;;                 define-syntax-set
  ;;                 define-syntax-set))
  
  ;; Export the normal mzscheme language, minus a few specific features
  ;; we override below.
  ;;(provide (all-from-except mzscheme
  ;;                          #%module-begin #%top lambda define
  ;;                          set!))
  (provide (all-from-except (lib "swindle.ss" "swindle") while defclass))

  ;; For each mzscheme symbol we exclude above, we need to define and
  ;; export our own version.  But we can't define any of these symbols
  ;; under their normal names, because we import mzscheme above.
  ;; Instead, we define each symbol under a local name (begining with
  ;; "lispish-", and rename it on export.
  ;(provide (rename lispish-#%module-begin #%module-begin))
  ;(provide (rename lispish-#%top #%top))
  ;(provide (rename lispish-lambda lambda))
  ;(provide (rename lispish-define define))
  ;(provide (rename lispish-set! set!))
  
  ;; Here are some additional LISP-like features we provide.
  (provide define-symbol-macro let-symbol-macro)
  
  ;; Set up Swindle to have a reasonable behavior for defclass. We're
  ;; trying to be as much like Dylan as possible, except also defining
  ;; classname?  as a predicate to test for objects of type
  ;; <classname>. We tried doing this the way Eli suggests, but it
  ;; breaks in all sorts of exciting ways. 

  (provide (rename lispish-defclass defclass))
  (define-syntax lispish-defclass
    (syntax-rules ()
      ((_ args ...) (defclass args ... 
                      :auto #t
                      :printer #t))))

  ;;----------------------------------------------------------------------
  ;; Redefining the Language Used To Write Macro Transformers
  ;;----------------------------------------------------------------------
  ;; Most modules begin with an implicit (require-for-syntax mzscheme),
  ;; which allows macro transformers to access definitions in the
  ;; module mzscheme.  We want "lispish" modules to begin with an
  ;; implicit (require-for-syntax (lib "lispish.ss" "work")) instead.
  ;; We can do this by transforming #%module-begin into #%plain-module-begin
  ;; (which doesn't require mzscheme) and explicitly requiring our own
  ;; definitions instead of the defaults.
  
  (define-syntax lispish-#%module-begin
    (syntax-rules ()
      [(_ body ...)
       (#%plain-module-begin
        (require-for-syntax (lib "lispish.ss" "5L"))
        body ...)]))
  
  
  ;;----------------------------------------------------------------------
  ;; Keywords: Self-Evaluating Symbols
  ;;----------------------------------------------------------------------
  ;; LISP provides "keywords", which are self-evaluating symbols
  ;; begining with a colon.  (A regular symbol, such as "foo" is evaluated
  ;; by looking up the value of the variable named "foo".  A self-evaluating
  ;; symbol, such as ":foo" always returns itself, the literal symbol
  ;; ":foo".  This is used to implement keyword arguments to functions.)
  ;;
  ;; To implement keywords, we intercept variable evaluation, examine the
  ;; variable name, and turn certain variables into literal symbols.  This
  ;; code will make more sense if you understand how mzscheme represents
  ;; variable evaluation and literal symbols:
  ;;
  ;;   (%top . x)    ; The value of the variable 'x'.
  ;;   (%datum . x)  ; The literal symbol 'x'.
  ;;
  ;; Our job is to selectively transform the former into the later.
  ;;
  ;; This code is based on the keyword support included with Eli
  ;; Barzilay's "html.ss" module.
    
  (define-syntax lispish-#%top
    (lambda (stx)
      (syntax-case stx ()

        ;; ----- CASE 1: Symbols beginning with ":".
        [(_ . varname)
         
         ;; This is our "fender-expr" (a boolean condition which returns
         ;; true if we want to apply this transformation).  We convert
         ;; the syntax object "varname" (a data structure containing
         ;; line numbers, lexical scoping information, etc.) into a raw
         ;; value, check to see if it's a symbol, and look for the
         ;; leading colon.
         (let ((v (syntax-object->datum #'varname)))
           (and (symbol? v)
                (eq? #\: (string-ref (symbol->string v) 0))))

         ;; If the above condition applies, convert our reference into
         ;; a literal symbol.
         #'(#%datum . varname)]
        
        ;; ----- CASE 2: All other symbols.
        [(_ . varname)
         
         ;; Output a real variable reference, using the official version
         ;; of #%top from mzscheme (instead of our version).
         #'(#%top . varname)])))
  

  ;;----------------------------------------------------------------------
  ;; Optional & Keyword Arguments
  ;;----------------------------------------------------------------------
  ;; Normally, Scheme accepts a very limited set of function prototypes:
  ;;
  ;;   (lambda (x y) ...)   ; Fixed number of argument.
  ;;   (lambda (x . y) ...) ; One fixed argument, any number stored in y.
  ;;   (lambda y ...)       ; Any number of arguments stored in y.
  ;;
  ;; We also want to support LISP-style optional and keyword arguments:
  ;;
  ;;   (lambda (x &rest y) ...)     ; Another notation for "(x . y)".
  ;;   (lambda (x &opt y z) ...)    ; y and z are optional, default to #f.
  ;;   (lambda (x &opt (y 1) ...)   ; y is optional, defaults to 1.
  ;;   (lambda (&key x y) ...)      ; Keyword arguments, default to #f.
  ;;   (lambda (&key (x 1)) ...)    ; x defaults to 1.
  ;;   (lambda (&key (x1 :x 3) ...) ; Keyword :x stored in x1, default 3.
  ;;
  ;; You can also put a "&rest" option after the keywords.  This will
  ;; capture all the key/value pairs (even the ones you name explicitly)
  ;; in a list.  For more information, see the Common LISP HyperSpec.
  ;;
  ;; To implement optional and keyword arguments, we replace 'lambda'
  ;; and 'define' with our own (very hairy) expanders.  If you need to
  ;; understand how this code works, open up the MzScheme manual to
  ;; the chapter on macros, find a friend, and try to explain this
  ;; function to each other line by line.  If this fails, try pestering
  ;; your local Scheme guru.  At Dartmouth, this generally means
  ;; somebody who's taught or TA'd CS 18.
  ;;
  ;; I based these expanders on code from Eli Barzilay's "html.ss"
  ;; module.  My code borrow's heavily from Eli's architecture, and uses
  ;; quite a few of his tricks, but I stipped out lots of features,
  ;; retyped everything by hand, and refactored the code quite a bit.
  
  (define-syntax lispish-lambda
    (lambda (stx)
      (syntax-case stx ()
        [(_ formals body ...)
         
         ;; We use these variables to store partially-transformed
         ;; parameter information.
         (let ((vars '()) ; Regular position arguments.
               (opts '()) ; Optional arguments.
               (keys '()) ; Keyword arguments.
               (rest #f)) ; Our rest argument.
           
           ;; In just a momement, we're going to use a recursive
           ;; state machine to process each individual argument.
           ;; The legal states are:
           ;;
           ;;   #f   - Processing regular arguments.
           ;;   'o   - Processing optional arguments.
           ;;   'k   - Processing keyword arguments.
           ;;   'r   - Processing our &rest argument.
           ;;   'end - No more arguments are allowed.
           ;;
           ;; But first, we need to define *lots* of helper functions.
           
           (define (err msg loc)
             ;; Report an error at the specified source location.
             (raise-syntax-error #f msg stx loc))
           
           (define (rest-arg-ok? state)
             ;; Is a rest argument allowed in this state?
             (or (not state) (eq? state 'k)))

           (define (check-parameter state parameter)
             ;; Make sure this parameter is sane, given the current state.
             (cond
               [(identifier? parameter)
                ;; Check for a leading "&".
                (let ((datum (syntax-object->datum parameter)))
                  (when (eq? (string-ref (symbol->string datum) 0) #\&)
                    (err "Unknown parameter specifier" parameter)))]
               [(pair? (syntax-e parameter))
                ;; Make sure we're in a plausible state.
                (unless (memq state '(o k))
                  (err "Only &opt and &key allow defaults" parameter))]
               [else
                (err "Very suspicious parameter" parameter)]))
           
           (define (keyword? val)
             ;; Is "val" a keyword begining with a colon?
             (let ((v (syntax-object->datum val)))
               (and (symbol? v)
                    (eq? #\: (string-ref (symbol->string v) 0)))))
           
           (define (name->keyword name)
             ;; Given a variable name, compute the corresponding keyword.
             (let* ((str (symbol->string (syntax-object->datum name)))
                    (sym (string->symbol (string-append ":" str))))
               ;; Convert the name back into a syntax object.  The last
               ;; two parameters must be #f--if you make them "sym",
               ;; you'll break syntax highlighting of keyword arguments.
               (datum->syntax-object name sym #f #f)))
           
           (define (build-formals)
             ;; Combine our vals and our rest argument to produce our
             ;; final list of standard R5RS formal parameters.
             (cond
               [(and (null? vars) (not rest)) #'()]
               [(null? vars) rest]
               [(not rest) #`(#,@vars)]
               [else #`(#,@vars . #,rest)]))
           
           (define (process-opt parameter)
             ;; Process an optional argument of the form "name" or
             ;; "(name default)".
             (syntax-case parameter ()
               [(name default)
                (if (identifier? #'name)
                    (list #'name #'default)
                    (err "Expected &opt name" #'name))]
               [name
                (if (identifier? #'name)
                    (list #'name #'#f)
                    (err "Expected &opt name" #'name))]))
           
           (define (build-bind-opt info)
             ;; Create a variable binding and the initialization code for
             ;; an optional argument.
             (let ((name (car info))
                   (default (cadr info)))
               #`(#,name (if (null? #,rest)
                             #,default
                             (let ((v (car #,rest)))
                               (set! #,rest (cdr #,rest))
                               v)))))
           
           (define (process-key parameter)
             ;; Process a keyword argument of the form "name",
             ;; "(name default)" or "(name key default)".  (We use lots
             ;; of "fender expressions" below to control which cases
             ;; apply.)
             (syntax-case parameter ()
               [(name key default)
                (and (identifier? #'name)
                     (not (keyword? #'name))
                     (keyword? #'key))
                (list #'name #'key #'default)]
               [(name default)
                (and (identifier? #'name) (not (keyword? #'name)))
                (list #'name (name->keyword #'name) #'default)]
               [name
                (and (identifier? #'name) (not (keyword? #'name)))
                (list #'name (name->keyword #'name) #'#f)]
               [other
                (err "Malformed &key clause" parameter)]))
           
           (define (build-bind-key info)
             ;; Create a variable binding and the initialization code for
             ;; a keyword argument.
             (let ((name (car info))
                   (keyword (cadr info))
                   (default (caddr info)))
               #`(#,name (find-keyword #,rest '#,keyword #,default))))
           
           (define (process-argument state parameter remaining loop)
             ;; Process a single argument, then recursively call "loop"
             ;; to move onto the next state.
             (check-parameter state parameter)
             (case state
               [(#f)  (set! vars (cons parameter vars))]
               [(o)   (set! opts (cons (process-opt parameter) opts))]
               [(k)   (set! keys (cons (process-key parameter) keys))]
               [(r)   (set! rest parameter) (set! state 'end)]
               [(end) (err "expected end of parameter list" parameter)]
               [else  (err "bad state (this shouldn't happen)" parameter)])
             (loop state remaining))
           
           ;; Run our state machine.  We use a special version of "let"
           ;; which (basically) creates an inline recursive function.
           ;; The first time through, we bind some initial values to
           ;; "state" and "args".  At any point in the body of the loop,
           ;; we can call "(loop new-state new-args)" to recursively
           ;; re-enter this code.  If you're worrying about stack space,
           ;; stop right now and look up "tail recursion"--we're safe,
           ;; and the theory is quite beatiful.  This trick comes from
           ;; Eli's code, and it's quite excellent.
           (let loop ((state #f) (args #'formals))
             
             ;; We use this syntax case to yank off the first remaining
             ;; item in "args" and have a look at it.
             (syntax-case args (&opt &key &rest)
               
               ;; These three cases are simple--we recognize one of our
               ;; special tokens and shift states appropriately.
               [(&opt . remaining)
                (if (not state)
                    (loop 'o #'remaining)
                    (err "misplaced &opt" #'formals))]
               [(&key . remaining)
                (if (not state)
                    (loop 'k #'remaining)
                    (err "misplaced &keys" #'formals))]
               [(&rest . remaining)
                (if (rest-arg-ok? state)
                    (loop 'r #'remaining)
                    (err "misplaced &rest" #'formals))]
               
               ;; This case handles regular, non-special arguments.  There
               ;; are a lot of these, so we use a subroutine.
               [(parameter . remaining)
                (process-argument state #'parameter #'remaining loop)]
               
               ;; This case fires occurs when we reach the end of our
               ;; parameter list.  We don't call (loop ...), so we fall
               ;; out of the loop and our state machine stops.
               [()
                #f]
               
               ;; Our list of formals didn't end with '(), so it must be
               ;; a dotted list: (lambda (x . y) foo) or (lambda x foo).
               ;; Treat it like a traditional Scheme rest argument.
               [restarg
                (if (rest-arg-ok? state)
                    (begin
                      (check-parameter 'r #'restarg)
                      (set! rest #'restarg))
                    (err "misplaced &rest argument" #'formals))]))
           
           ;; Make sure our arguments are in the right order.
           (set! vars (reverse! vars))
           (set! opts (reverse! opts))
           (set! keys (reverse! keys))
           
           ;; Both &opt and &keys require a rest argument for their
           ;; own internal use.  We may have to create one now.
           (when (and (not rest)
                      (or (not (null? opts))
                          (not (null? keys))))
             (set! rest #'rest))
           
           ;; Build our result.  We rely on (build-formals) to assemble
           ;; a traditional argument list, including a rest argument
           ;; if we need one.  We use "quasisyntax/loc" to build a
           ;; replacement syntactic form without losing our line
           ;; numbers or other debugging information.
           (cond             
             ;; Handle the simple case.
             [(and (null? opts) (null? keys))
              (quasisyntax/loc stx (lambda #,(build-formals) body ...))]
             ;; Do the heavy lifting.
             [else
              (quasisyntax/loc
               stx
               (lambda #,(build-formals)
                 ;; We use "let*", so each parameter binding can see all
                 ;; of the ones which came before.  We use "#,@" to splice
                 ;; lists of bindings directly into the "let*".
                 (let* (#,@(map build-bind-opt opts)
                        #,@(map build-bind-key keys))
                   body ...)))]))])))
  
  ;; Runtime support code for looking up keywords in argument lists.
  ;; XXX - We don't check for odd-length keyword lists.
  ;; XXX - Check for too many optional arguments.
  (define (find-keyword keys key default)
    (let loop ((remaining-keys keys))
      (if (null? remaining-keys)
          default
          (if (eq? key (car remaining-keys))
              (cadr remaining-keys)
              (loop (cddr remaining-keys))))))
  
  ;; We also need to patch up "define" to use our fancy new lambda.
  (define-syntax lispish-define
    (syntax-rules ()
      
      ;; If we're defining a function, expand using lispish-lambda.
      [(_ (name . formals) body ...)
       (define name
         (lispish-lambda formals
           body ...))]
      
      ;; Otherwise, let mzscheme's "define" handle things.
      [(_ other ...)
       (define other ...)]))
  
  
  ;;----------------------------------------------------------------------
  ;; Symbol Macros and Generalized Setters
  ;;----------------------------------------------------------------------
  ;; A "symbol macro" is an identifier which expands into a more complex
  ;; expression:
  ;;
  ;;   (define-symbol-macro NAME EXPR)
  ;;   (let-symbol-macro ((NAME EXPR)...) BODY...)
  ;;   NAME => EXPR
  ;;
  ;; This allows library developers to create fancy kinds of variables--
  ;; in particular, "smart" variables that transparently update values
  ;; stored outside of Scheme.
  
  (define-syntax-set (define-symbol-macro let-symbol-macro)
    
     ;; define-symbol-macro and let-symbol-macro actually expand into
     ;; syntax definitions themselves.  So our macro will need to
     ;; contain syntax-case forms that generate *other*
     ;; syntax-case forms.  This function does the tricky bit. 
     (define (make-symbol-transformer id-and-expansion)
       (syntax-case id-and-expansion ()
         [(id expansion)
          ;; Return a two-item list containing the ID and the
          ;; syntax rules to bind it to.  (This two-item list can be
          ;; inserted directly into the expansion of both DEFINE- and
          ;; LET-SYMBOL-MACRO if we're clever.)
          #'(id
             (lambda (stx)
               (syntax-case stx ()
                 ;; Expand the symbol when it appears in the first position
                 ;; of a form.  This is standard Scheme.
                 [(_ . arg)
                  #'(expansion . arg)]
                 ;; Expand the symbol when it appears in any other position.
                 ;; This is an mzscheme extension to syntax-rules.
                 [_
                  #'expansion])))]))
     
     ;; Handy utility function: Unpack a syntax list, run map over it
     ;; and pack it back up again.
     (define (map-syntax func syntax-list)
       (datum->syntax-object syntax-list
                             (map func (syntax->list syntax-list))
                             syntax-list
                             syntax-list))
     
     (define (define-symbol-macro/proc stx)
       (syntax-case stx ()
         [(_ . binding)
          #`(define-syntax #,@(make-symbol-transformer #'binding))]))
      
     (define (let-symbol-macro/proc stx)
       (syntax-case stx ()
         [(_ bindings body ...)
          #`(let-syntax (#,@(map-syntax make-symbol-transformer #'bindings))
              body ...)])))
  

  ;;----------------------------------------------------------------------
  ;; Generalized Setters
  ;;----------------------------------------------------------------------
  ;; A "generalized setter" allows you to write complex "set!"
  ;; expressions:
  ;;
  ;;   (set! (NAME ARG...) VALUE)
  ;;     => (set-NAME! ARG... VALUE)
  ;;
  ;; This feature can be extended to support "destructive update"
  ;; functions, such as INC!, DEC!, PUSH! and POP!.  But we must be
  ;; very careful not to evaluate the ARG... values twice:
  ;;
  ;;   (inc! (NAME ARG...)
  ;;     => (let ((ARGVAL_n ARG_n)...)
  ;;          (set-NAME! ARGVAL... (+ 1 (NAME ARGVAL...))))
  ;;
  ;; We will define INC!, DEC!, PUSH! and POP! in another library, but
  ;; we'll provide all the machinery here.

  (define-syntax (lispish-set! stx)
    (syntax-case stx ()
      
      ;; Transform (set! (name args ...) value) into a call to
      ;; (set-nsme! args ... value), using the value of
      ;; "set-name!" in our caller's namespace (a nice trick!).
      ;; Eli Barzilay suggested that we use the Scheme conventions for
      ;; setters, rather than the more general Dylan ones, because they
      ;; work better with the existing language.
      [(lispish-set! (name args ...) value)
       (if (not (identifier? #'name))
           (raise-syntax-error #f "Expected identifier" stx #'name)
           (let* (;; We must do a fairly complicated song-and-dance to
                  ;; transform "name" into "set-name!" without losing
                  ;; the original lexical context.
                  (name-str (symbol->string (syntax-object->datum #'name)))
                  (setter-name-str (string-append "set-" name-str "!"))
                  (setter-name-sym (string->symbol setter-name-str))
                  (setter-name (datum->syntax-object #'name setter-name-sym
                                                     #'name #'name)))
             (quasisyntax/loc stx (#,setter-name args ... value))))]

      ;; This hack should allow us to SET! symbol macros (without breaking
      ;; other strange, built-in features of mzscheme's SET!).  The
      ;; theory: If we see a SET!, and...
      ;;   1) the first argument is an identifier, and
      ;;   2) that identifier has an expander value, and
      ;;   3) that expander is a procedure, and
      ;;   4) that procedure is not a built-in SET! transformer (which is
      ;;      mzscheme's half-hearted attempt to implement symbol macros),
      ;; ...*then* expand the symbol and run it back through LISPISH-SET!.
      ;; This allows us to SET! symbol macros without lots of messy
      ;; compile-time information, and shouldn't step on the toes of too
      ;; many other modules.
      ;;
      ;; TODO - Try to improve syntax highlighting of NAME.
      [(_ name value)
       (and (identifier? #'name)
            (let ((binding (syntax-local-value #'name (lambda () #f))))
              (and (procedure? binding)
                   (not (set!-transformer? binding)))))
       (quasisyntax/loc
        stx
        (lispish-set! #,((syntax-local-value #'name) #'name) value))]
      
      ;; Transform all other SET! statemets into the built-in SET!.
      [(_ other ...)
       (syntax/loc stx (set! other ...))]))

  )
