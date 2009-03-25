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

(module kernel (lib "mizzen.ss" "mizzen")

  ;; Import %call-prim from the engine.
  (require #%engine-primitives)

  (require (lib "begin-var.ss" "mizzen"))  
  (require (lib "mizzen-unit.ss" "mizzen"))  
  (require (lib "hook.ss" "halyard/private"))
  (require (lib "indent.ss" "halyard/private"))
  (require (lib "types.ss" "halyard/private"))
  (require (lib "util.ss" "mizzen"))
  (require (lib "util.ss" "halyard/private"))
  (require (lib "nodes.ss" "halyard/private"))
  (require (lib "events.ss" "halyard/private"))

  ;; Get format-result-values.
  (require (lib "trace.ss" "halyard/private"))

  ;; The C++ interface expects us to export these identifiers from
  ;; kernel.ss.
  (provide (all-from (lib "types.ss" "halyard/private")))


  ;;=======================================================================
  ;;  Standard Hooks
  ;;=======================================================================
  
  (provide *enter-card-hook* *exit-card-hook*
           *card-body-finished-hook* *before-draw-hook*
           *before-log-message-hook*
           *dangerous-exit-script-hook*)

  ;; Called before running each card.
  (define *enter-card-hook* (make-hook 'enter-card))

  ;; Called immediately before moving to a new card.
  (define *exit-card-hook* (make-hook 'exit-card))

  ;; Called after running the body of a each card, if the body exits
  ;; normally (not by jumping, etc.).
  (define *card-body-finished-hook* (make-hook 'card-body-finished))

  ;; Called before *most* screen redraws.
  (define *before-draw-hook* (make-hook 'before-draw))

  ;; Internal: Used to run code before certain classes of log messages
  ;; are displayed to the user.  This hook is subject to change.
  (define *before-log-message-hook* (make-hook 'before-log-message))

  ;; Called immediately before the engine exits or a script is reloaded.
  ;; This is marked as "dangerous" because the engine no longer has a stage
  ;; object, and many primitives will crash the engine when called.  The
  ;; node hierarchy should be relatively intact, and Scheme I/O should
  ;; still be available.
  ;;
  ;; This hook is meant to be used to save user state, etc., shortly
  ;; before full shutdown, and may be replaced with a more robust API later.
  (define *dangerous-exit-script-hook* (make-hook 'dangerous-exit-script))


  ;;=======================================================================
  ;;  Core engine API
  ;;=======================================================================
  ;;  We only declare a small number of primitives here, typically those
  ;;  which are needed by the kernel or which intimately depend on the
  ;;  kernel's inner workings.
  
  (provide call-prim have-prim? runtime-directory value->boolean idle
           blocking-idle exit-script custom-jump-handler-installed?
           call-with-jump-handler refresh adjust-delay)

  ;; C++ can't handle very large or small integers.  Here are the
  ;; typical limits on any modern platform.
  (define *32-bit-signed-min* -2147483648)
  (define *32-bit-signed-max* 2147483647)
  (define *32-bit-unsigned-min* 0)
  (define *32-bit-unsigned-max* 4294967295)

  (define (call-prim . args)
    ;; Our high-level wrapper for %call-prim (which is defined by
    ;; the engine).  You should almost always call this instead of
    ;; %call-prim, because this function calls %kernel-check-state
    ;; to figure out what happened while we were in C++.
    (let [[result (apply %call-prim args)]]
      (%kernel-check-state)
      result))
  
  (define (have-prim? name)
    (%call-prim 'HavePrimitive name))

  (define *runtime-directory* #f)

  ;;; The directory containing our engine runtime, including such things as
  ;;; fonts/ and collects/.
  (define (runtime-directory)
    (unless *runtime-directory*
      (set! *runtime-directory* (%get-runtime-directory)))
    *runtime-directory*)

  (define (value->boolean val)
    ;; XXX - Coerce a Scheme value to an explicit boolean value.  This
    ;; is currently needed in a few points in the Scheme/C++
    ;; interface, because C++ doesn't understand Scheme's rules for
    ;; which values are true and which are false.  We should probably
    ;; do something language-independent in C++, but the correct
    ;; design is unclear.
    (if val
        #t
        #f))
  
  (define (%idle blocking?)
    (%kernel-die-if-callback 'idle)
    ;; We call %call-prim directly to avoid using rest arguments or
    ;; 'apply', both of which cons (which we don't want to happen in the
    ;; idle loop.)
    (%call-prim 'Idle blocking?)
    (%kernel-check-state))

  (define (blocking-idle)
    (%idle #t))

  ;;; Hand over control to the operating system, and process any
  ;;; outstanding events.
  (define (idle)
    (%idle #f))
  
  (define (%kernel-idle-and-check-deferred)
    ;; We use either a regular or a blocking idle, depending on whether we
    ;; have something to do afterwards.
    (if (null? *%kernel-deferred-thunk-queue*)
      (blocking-idle)
      (idle))
    (%kernel-check-deferred))

  ;; TODO - replace most calls to this function with calls to ERROR.
  (define (error-with-extra-dialog msg)
    ;; TODO - More elaborate error support.
    (logger 'error 'halyard msg)
    (error msg))
  
  ;;; Try to exit the currently-running script.  If the script exits, this
  ;;; function will not return.
  ;;;
  ;;; However, if the engine is running in developer mode, the user may
  ;;; cancel the exit process when asked to save files.  In this case, the
  ;;; function will return #f.
  ;;;
  ;;; If success? is false, then the application will return an error to
  ;;; the operating system.
  (define (exit-script &opt [success? #t])
    (call-prim 'ShouldExitWithError (not success?))
    ;; If we have a GUI, it may want to issue "Save this file?" prompts and
    ;; clean up windows before actually starting a shutdown process.  But
    ;; if we don't have a GUI, we call the low-level exit routines instead.
    (if (have-prim? 'MaybeExitScriptGui)
      (call-prim 'MaybeExitScriptGui)
      (call-prim 'ExitScriptNonGui))
    ;; We didn't actually manage to exit, so reset ShouldExitWithError and
    ;; return #f.
    (call-prim 'ShouldExitWithError #f)
    #f)
  
  (define (check-whether-jump-allowed card)
    (cond
     [*running-on-exit-handler-for-node*
      (error (cat "Cannot JUMP in (on exit () ...) handler for node "
                  (*running-on-exit-handler-for-node* .full-name)))]
     [(or (eq? *%kernel-state* 'JUMPING)
          ;; XXX - An ugly special case: %kernel-run-as-callback may
          ;; temporarily change *%kernel-state* from 'JUMPING to 'NORMAL, but
          ;; not change *%kernel-jump-card*.  So we need to check for this.
          ;; We should probably clean it up, too.
          *%kernel-jump-card*)
      ;; TODO - Eventually, we may want to make this an error.  See F#10544.
      (warn 'halyard "Jump to " card " overriding jump to " *%kernel-jump-card*
            (if (eq? *%kernel-state* 'JUMPING) "" " in callback"))]
     [(eq? *%kernel-state* 'INTERPRETER-KILLED)
      (fatal 'halyard "Cannot jump to " card
             " after interpreter has been shut down")]))

  (define (set-jump-state! card)
    (check-whether-jump-allowed card)
    (set! *%kernel-jump-card* card)
    (%kernel-set-state 'JUMPING))

  (define (jump-to-card card)
    (set-jump-state! card)
    (%kernel-check-state))

  (define *custom-jump-handler-installed?* #f)

  ;; Returns #t if we are inside call-with-jump-handler.  You shouldn't
  ;; need this unless you're trying to do something really tricky with
  ;; jumps, like jump-to-each-card.ss does.
  (define (custom-jump-handler-installed?)
    *custom-jump-handler-installed?*)

  ;; This is an internal API for use by ASSERT-JUMPS.  It calls THUNK, and
  ;; if a jump occurs, the jump is intercepted and the destination card is
  ;; passed to HANDLER.
  (define (call-with-jump-handler handler thunk)
    (assert (eq? *%kernel-state* 'NORMAL))
    (assert (not *%kernel-jump-card*))
    (label exit-to-top
      (fluid-let [[*%kernel-exit-to-top-func* exit-to-top]
                  [*custom-jump-handler-installed?* #t]]
        (thunk)))
    (when (eq? *%kernel-state* 'JUMPING)
      (let [[jump-card *%kernel-jump-card*]]
        (set! *%kernel-state* 'NORMAL)
        (set! *%kernel-jump-card* #f)
        (handler jump-card)))
    (assert (eq? *%kernel-state* 'NORMAL))
    (assert (not *%kernel-jump-card*)))

  ;;; Show the results of all drawing calls made since the last screen
  ;;; update.  (The screen is only updated after calls to IDLE, REFRESH,
  ;;; WAIT, and similar functions.)
  (define (refresh &key (transition 'none) (ms 500))
    ;; Refresh the screen by blitting dirty areas of our offscreen buffer
    ;; to the display.
    (call-hook-functions *before-draw-hook*)
    (if (have-prim? 'Refresh)
        (call-prim 'Refresh transition ms)))

  ;; If Halyard is running in command-line mode, clamp all delays to a
  ;; maximum of 10 milliseconds.
  (define (adjust-delay milliseconds)
    (if (call-prim 'IsInCommandLineMode)
      (min milliseconds 10)
      milliseconds))


  ;;=======================================================================
  ;;  Engine Variables
  ;;=======================================================================
  ;;  These variables persist across reloads of the program, but may only
  ;;  store a limited set of Scheme types.
  ;;
  ;;  We can actually remove this code from the kernel and put it into a
  ;;  separate file if we modify event handling to not need the _pass and
  ;;  _veto engine variables.

  (provide engine-var set-engine-var! engine-var-exists?
           define-engine-variable define/p)

  (define (engine-var name)
    (call-prim 'Get (if (string? name) (string->symbol name) name)))
  
  (define (set-engine-var! name value)
    ;; Set an engine variable.  This is a pain, because we have to play
    ;; along with the engine's lame type system.
    (let [[namesym (if (string? name) (string->symbol name) name)]]
      (call-prim 'SetTyped namesym value)))

  (define (engine-var-exists? name)
    (call-prim 'VariableInitialized name))
  
  ;;; Bind a Scheme variable name to an engine variable.
  ;;;
  ;;; @syntax (define-engine-variable name engine-name &opt init-val)
  ;;; @param NAME name The Scheme name to use.
  ;;; @param NAME engine-name The corresponding name in the engine.
  ;;; @opt EXPRESSION init-val The initial value of the variable.
  ;;; @xref engine-var set-engine-var!
  (define-syntax define-engine-variable
    (syntax-rules ()
      [(define-engine-variable name engine-name init-val)
       (begin
         (define-symbol-macro name (engine-var 'engine-name))
         (maybe-initialize-engine-variable 'engine-name init-val))]
      [(define-engine-variable name engine-name)
       (define-symbol-macro name (engine-var 'engine-name))]))
  (define-syntax-indent define-engine-variable 2)

  (define (maybe-initialize-engine-variable engine-name init-val)
    ;; A private helper for define-engine-variable.  We only initialize
    ;; a variable if it doesn't already exist, so it can keep its value
    ;; across script reloads.
    (unless (engine-var-exists? engine-name)
      (set! (engine-var engine-name) init-val)))

  ;;; Define a persistent global variable which keeps its value across
  ;;; script reloads.  Note that two persistent variables with the same
  ;;; name, but in different modules, are essentially the same variable.
  ;;; Do not rely on this fact--it may change.
  ;;;
  ;;; @syntax (define/p name init-val)
  ;;; @param NAME name The name of the variable.
  ;;; @param EXPRESSION init-val The initial value of the variable.
  (define-syntax define/p
    (syntax-rules ()
      [(define/p name init-val)
       (define-engine-variable name name init-val)]))
  (define-syntax-indent define/p 1)


  ;;=======================================================================
  ;;  Main loop
  ;;=======================================================================

  (provide %main-kernel-loop)

  (define *initial-commands-have-been-run?* #f)

  ;; We call this from inside our main event loop.  The first time we call,
  ;; we run any initial commands, such as "(jump /start)", or a code
  ;; snippet passed in from the command-line.
  (define (run-initial-commands-the-first-time-through)
    (unless *initial-commands-have-been-run?*
      (set! *initial-commands-have-been-run?* #t)
      (call-prim 'RunInitialCommands)))

  (define (%main-kernel-loop)
    ;; The workhorse function.  We get called to manage the main event
    ;; loop, and we provide support code for handling jumps, STOPPING
    ;; the interpreter, idling after the end of each card, and quiting
    ;; the interpreter.  This code needs to be understood in
    ;; conjuction with *%kernel-state*, the functions that manipulate
    ;; it, and the callback system.  Yes, it's ugly--but it lets us
    ;; get the semantics we want without writing an entire interpreter
    ;; from scratch.
    (label exit-interpreter
      (fluid-let ((*%kernel-exit-interpreter-func* exit-interpreter))
        (let ((jump-card #f))
          (let loop []
            (label exit-to-top
              (with-exceptions-blocked (report-exception)
                (fluid-let ((*%kernel-exit-to-top-func* exit-to-top))
                  (run-initial-commands-the-first-time-through)
                  (cond
                   [jump-card
                    (run-card jump-card)]
                   [#t
                    ;; Highly optimized do-nothing loop. :-)  This
                    ;; is a GC optimization designed to prevent the
                    ;; interpreter from allocating memory like a crazed
                    ;; maniac while the user's doing nothing.  If we
                    ;; removed this block, we'd have to perform a lot
                    ;; of LABEL and FLUID-LET statements, which are
                    ;; extremely expensive in quantities of 1,000.
                    (let idle-loop []
                      (unless (eq? *%kernel-state* 'JUMPING)
                        (if (%kernel-stopped?)
                            (blocking-idle)
                            (%kernel-idle-and-check-deferred))
                        (idle-loop)))]))))
            (set! jump-card #f)
            (when (eq? *%kernel-state* 'JUMPING)
              ;; Handle a jump by setting jump-card for our next goaround.
              (set! jump-card *%kernel-jump-card*))
            (%kernel-maybe-clear-state)
            (loop)))))
    (%kernel-maybe-clear-state)
    (notify-exit-script))


  ;;=======================================================================
  ;;  Kernel Entry Points
  ;;=======================================================================
  ;;  The '%kernel-' methods are called directly by the engine.  They
  ;;  shouldn't raise errors, because they're called directly from C++
  ;;  code that doesn't want to catch them (and will, in fact, quit
  ;;  the program).
  ;;
  ;;  The theory behind these functions is documented in detail in
  ;;  TInterpreter.h.

  (define (notify-exit-script)
    (with-exceptions-blocked (report-exception)
      (define current-node-or-false (*engine* .current-group-member))
      (call-hook-functions *dangerous-exit-script-hook*
                           current-node-or-false)))

  (define (%kernel-kill-interpreter)
    ;; Note that this is the only function that should set
    ;; INTERPRETER-KILLED.  There's other stuff going on in
    ;; TInterpreterManager that needs to happen at the same time, in
    ;; particular setting mDone appropriately.
    (%kernel-set-state 'INTERPRETER-KILLED))

  (define (%kernel-stop)
    (%kernel-set-state 'STOPPING))

  (define (%kernel-go)
    (when (%kernel-stopped?)
      (%kernel-set-state 'NORMAL)))

  (define (%kernel-stopped?)
    (or (eq? *%kernel-state* 'STOPPING)
        (eq? *%kernel-state* 'STOPPED)))
  
  (define (%kernel-can-suspend?)
    (not *%kernel-running-callback?*))

  (define (%kernel-pause)
    (with-exceptions-blocked (report-exception)
      (%kernel-die-if-callback '%kernel-pause)
      (%kernel-set-state 'PAUSED)))

  (define (%kernel-wake-up)
    (with-exceptions-blocked (report-exception)
      (%kernel-die-if-callback '%kernel-wake-up)
      (when (%kernel-paused?)
        (%kernel-clear-state))))

  (define (%kernel-paused?)
    (eq? *%kernel-state* 'PAUSED))

  (define (%kernel-kill-current-card)
    (%kernel-set-state 'CARD-KILLED))

  (define (%kernel-jump-to-card-by-name card-name)
    (with-exceptions-blocked (report-exception)
      (set-jump-state! (find-card card-name))))

  (define (%kernel-load-group group-name)
    (with-code-loading-allowed [report-exception #f]
      ((find-static-node group-name) .ensure-loaded! 'c++)))

  (define (%kernel-current-card-name)
    (if (*engine* .current-card)
        (value->string ((*engine* .current-card) .full-name))
        ""))

  (define (%kernel-valid-card? card-name)
    (with-code-loading-allowed [report-exception #f]
      (card-exists? card-name)))

  (define (%kernel-eval expression)
    (let [[ok? #t] [result "#<did not return from jump>"]]

      ;; Jam everything inside a callback so JUMP, etc., work
      ;; as the user expects.  Do some fancy footwork to store the
      ;; return value(s) and return them correctly.  This code is ugly
      ;; because I'm too lazy to redesign the callback architecture
      ;; to make it pretty.
      (%kernel-run-as-callback
       ;; Our callback.
       (lambda ()
         (call-with-values
          (lambda () (eval (read (open-input-string expression))))
          (lambda r (set! result (apply format-result-values r)))))

       ;; Our error handler.
       (lambda (exn)
         (set! ok? #f)
         (set! result (exn-message exn))))
      
      ;; Return the result.
      (cons ok? result)))

  (define (%kernel-maybe-handle-log-message level category msg)
    ;; We have two different extension mechanisms here: A general hook,
    ;; which can't be used to control our return value, and a
    ;; special-purpose with mizzen's (current-warning-handler).  We could
    ;; probably do something more unified here, but it would have to happen
    ;; at the mizzen level.
    (call-hook-functions *before-log-message-hook* level category msg)
    (if (and (eq? level 'warn) (current-warning-handler))
      ;; The current-warning-handler is not supposed to raise any errors,
      ;; because we're already in the error-handling machinery.
      (with-exceptions-blocked [report-exception]
        ((current-warning-handler) msg)
        #t)
      ;; We don't have a current-warning-handler, so let somebody else
      ;; handle this for us.
      #f))

  (define built-in-identifiers-module
    '(lib "built-in-identifiers.ss" "halyard/loader"))

  ;;; Fetch all identifiers in the built-in namespace (currently mzscheme).
  ;;; We will probably want to add support for querying specific modules
  ;;; (MODULE->NAMESPACE will help).
  (define (%kernel-get-built-in-identifiers)
    (with-exceptions-blocked (report-fatal-exception)
      (define (sym->type sym)
        (with-handlers [[exn:fail:contract:variable? 
                         (lambda (exn) 'variable)] ;; unbound var
                        [exn:fail:syntax? (lambda (exn) 'syntax)]]    ;; a macro
          ;; We should only have to pass one argument to 
          ;; NAMESPACE-VARIABLE-VALUE, since the second argument is optional 
          ;; and defaults to #t. There's a bug in PLT's implementation, however,
          ;; that causes it to read junk data off the stack if you don't pass 
          ;; a second argument in, which is usually a true value (since most 
          ;; values are considered by scheme to be true), but can occasionally
          ;; become false, which leads to it always throwing exn:variable 
          ;; on imported names and syntax. In order to work around this, we 
          ;; can just pass #t in, and let the PLT maintainers sort out the bug. 
          (let [[val (namespace-variable-value sym #t)]]
            (cond
             [(function? val) 'function]
             [#t 'variable]))))
      (define (sym->record sym)
        (list sym (sym->type sym)))
      (namespace-require built-in-identifiers-module)
      (map sym->record
           (parameterize [[current-namespace
                           (module->namespace built-in-identifiers-module)]]
             (namespace-mapped-symbols)))))
  
  (define (%kernel-run-callback function args)
    (%kernel-run-as-callback (lambda () (apply function args))
                             report-exception))

  ;; We need to export this from the kernel so CallScheme can find it.
  (define %kernel-reverse! reverse!)
  (define %kernel-equals? equals?)
  

  ;;=======================================================================
  ;;  Internal State
  ;;=======================================================================
  ;;  When the interpreter returns from a primitive call (including a
  ;;  primitive call to run the idle loop), it can be in one of a number of
  ;;  states:
  ;;
  ;;    NORMAL:   The interpreter is running code normally, or has no code
  ;;              to run.  The default.
  ;;    STOPPING: The kernel is about to stop.  We use a two-phase stop
  ;;              system--STOPPING indicates we should escape to the
  ;;              top-level loop, and STOPPED indicates that we should
  ;;              efficiently busy-wait at the top-level.
  ;;    STOPPED:  The kernel has been stopped.
  ;;    PAUSED:   The interpreter should pause the current card until it is
  ;;              told to wake back up.  This is used for modal text
  ;;              entry fields, (wait ...), and similar things.
  ;;    JUMPING:  The interpreter should execute a jump.
  ;;    CARD-KILLED: The interpreter should stop executing the current
  ;;               card, and return to the top-level loop.
  ;;    INTERPRETER-KILLED: The interpreter should exit.
  ;;
  ;;  Callbacks are slightly special, however--see the code for details.
  
  (provide run-deferred defer executing-deferred-safe-time-callbacks?
           %kernel-check-deferred)

  ;; The most important global state variables.
  (define *%kernel-running-callback?* #f)
  (define *%kernel-state* 'NORMAL)

  ;; Some slightly less important global state variables.  See the
  ;; functions which define and use them for details.
  (define *%kernel-jump-card* #f)

  ;; Deferred thunks are used to implement (deferred-callback () ...).
  ;; See run-deferred for details.
  (defclass <deferred-action> ()
    parent
    thunk)

  (define *%kernel-running-deferred-thunks?* #f)
  (define *%kernel-deferred-thunk-queue* '())
  
  ;; These are bound at the top level in %kernel-run, and get
  ;; temporarily rebound during callbacks.  We use
  ;; *%kernel-exit-interpreter-func* to help shut down the
  ;; interpreter, and *%kernel-exit-to-top-func* to help implement
  ;; anything that needs to bail out to the top-level loop (usually
  ;; after setting up some complex state).  See the functions which define
  ;; and call these functions for more detail.
  (define *%kernel-exit-interpreter-func* 'no-exit-interpreter-func)
  (define *%kernel-exit-to-top-func* 'no-exit-to-top-func)
  
  (define (%kernel-die-if-callback name)
    (if *%kernel-running-callback?*
        (error-with-extra-dialog (cat "Cannot call " name 
                                      " from within callback."))))

  (define (%kernel-wake-up-if-necessary)
    ;; We may need to finish waking up from any WAIT calls which were
    ;; aborted in a callback (typically by the destruction of the
    ;; media object).
    ;;
    ;; Once upon a time, we did this during calls to Stage::OnIdle and
    ;; Stage::NotifyEnterCard. But that was much too late, and left us with
    ;; many opportunities (mostly involving JUMP and DEFERRED-CALLBACK) to
    ;; not wake up in time. I believe this is the correct place to wake up.
    ;;
    ;; We still need to do this even if (%KERNEL-PAUSED?) is false, because
    ;; the Stage maintains internal pause-related flags which need to be
    ;; cleared separately. In general, the pause system is pretty ugly, has
    ;; resulted in a number of subtle bugs, and should probably be
    ;; redesigned.
    (unless *%kernel-running-callback?*
      (when (have-prim? 'WakeUpIfNecessary)
        (%call-prim 'WakeUpIfNecessary))))

  (define (%kernel-assert-safe-to-run-deferred-thunks)
    ;; Here's a whole list of things that should be true if we're about
    ;; to run a deferred-callback.
    (%assert (not *%kernel-running-callback?*))
    (%assert (not *%kernel-running-deferred-thunks?*))
    (%assert (not *running-on-exit-handler-for-node*))
    (%assert (not (eq? *%kernel-state* 'PAUSED)))
    (%assert (eq? *%kernel-state* 'NORMAL)))

  ;;; This can be used to defer calls to WAIT, IDLE and other blocking
  ;;; functions that can't be called from a callback.  Note that because a
  ;;; thunk is not an element, we don't honor WITH-DEFAULT-ELEMENT-PARENT.
  (define (run-deferred thunk &key (parent (current-card)))
    (set! *%kernel-deferred-thunk-queue*
          (cons (make-deferred-action parent thunk)
                *%kernel-deferred-thunk-queue*))
    #f)

  ;;; Defer a body of code to run later using RUN-DEFERRED.
  (define-syntax defer
    (syntax-rules ()
      [(_ body ...)
       (run-deferred (fn () body ...))]))
  (define-syntax-indent defer 0)

  (define (executing-deferred-safe-time-callbacks?)
    *%kernel-running-deferred-thunks?*)
  
  (define (all-but-last lst)
    (reverse (cdr (reverse lst))))

  (define (%kernel-check-deferred)
    ;; If the interpreter has stopped, cancel any deferred thunks.  (See
    ;; run-deferred.)  This function won't call any deferred thunks
    ;; unless it is safe to do so.
    (when (%kernel-stopped?)
      (set! *%kernel-deferred-thunk-queue* '()))   

    ;; Is is even safe to be running deferred thunks right now?
    (%kernel-assert-safe-to-run-deferred-thunks)

    ;; Run any deferred thunks.
    (while (not (null? *%kernel-deferred-thunk-queue*))
      (let [[item (last *%kernel-deferred-thunk-queue*)]]
        (set! *%kernel-deferred-thunk-queue*
              (all-but-last *%kernel-deferred-thunk-queue*))
        
        (fluid-let [[*%kernel-running-deferred-thunks?* #t]]
          (debug 'halyard.deferred "Running deferred thunk.")
          ((deferred-action-thunk item))
          (debug 'halyard.deferred "Finished running deferred thunk.")))))

  (define (%kernel-cancel-deferred-thunks-for parent)
    ;; Only keep those thunks which aren't associated with PARENT.
    (set! *%kernel-deferred-thunk-queue*
          (filter (lambda (d)
                    (not (eq? parent (deferred-action-parent d))))
                  *%kernel-deferred-thunk-queue*)))

  (define (%kernel-clear-state)
    ;; This is the version that we want to call from most places to get the
    ;; current state set back to normal.
    (%assert (not (or (eq? *%kernel-state* 'STOPPING) 
                      (eq? *%kernel-state* 'STOPPED))))
    (set! *%kernel-state* 'NORMAL)
    (set! *%kernel-jump-card* #f))

  (define (%kernel-maybe-clear-state)
    ;; This should only ever be called from the main loop, because then
    ;; stopping has finished and the interpreter is in a stopped state. 
    ;; Everyone else should only ever call %kernel-clear-state
    (case *%kernel-state*
      [[STOPPING]
       (set! *%kernel-state* 'STOPPED)]
      [[STOPPED]
       #f]
      [else
       (set! *%kernel-state* 'NORMAL)])
    (set! *%kernel-jump-card* #f))
  
  (define (%kernel-run-as-callback thunk exception-handler)
    ;; This function is in charge of running callbacks from C++ into
    ;; Scheme.  These include simple callbacks and anything evaled from
    ;; C++.  When we're in a callback, we need to install special values
    ;; of *%kernel-exit-to-top-func* and *%kernel-exit-interpreter-func*,
    ;; because the values installed by %kernel-run can't be invoked without
    ;; "throwing" across C++ code, which isn't allowed (and which would
    ;; be disasterous).  Furthermore, we must trap all errors, because
    ;; our C++-based callers don't want to deal with Scheme exceptions.
    (if (eq? *%kernel-state* 'INTERPRETER-KILLED)
        (info 'halyard
              "Skipping callback because interpreter is being shut down")
        (let [[saved-kernel-state *%kernel-state*]]
          (set! *%kernel-state* 'NORMAL)
          (label exit-callback
            ;; TODO - Can we have better error handling?
            (with-exceptions-blocked (exception-handler)
              (fluid-let [[*%kernel-exit-to-top-func* exit-callback]
                          [*%kernel-exit-interpreter-func* exit-callback]
                          [*%kernel-running-callback?* #t]]
                (thunk))))
          (if (eq? *%kernel-state* 'NORMAL)
              (set! *%kernel-state* saved-kernel-state)))))

  (define (%kernel-set-state state)
    (set! *%kernel-state* state))

  (define (%kernel-check-state)
    ;; This function is called immediately after returning from any
    ;; primitive call (including idle).  It's job is to check flags set by
    ;; the engine, and generally bring the Scheme world back into sync with
    ;; everybody else.
    ;; 
    ;; Since this is called after idle, it needs to be *extremely* careful
    ;; about allocating memory.  See %kernel-run for more discussion about
    ;; consing in the idle loop (hint: it's bad).
    (%kernel-wake-up-if-necessary)  ; Should be the first thing we do.
    (case *%kernel-state*
      [[NORMAL STOPPED]
       #f]
      [[STOPPING]
       (when *%kernel-exit-to-top-func*
             (*%kernel-exit-to-top-func* #f))]
      [[PAUSED]
       (%call-prim 'Idle #t)          ; Similar to blocking-idle.
       (%kernel-check-state)]         ; Tail-call self without consing.
      [[JUMPING]
       (when *%kernel-exit-to-top-func*
             (*%kernel-exit-to-top-func* #f))]
      [[CARD-KILLED]
       (when *%kernel-exit-to-top-func*
             (*%kernel-exit-to-top-func* #f))]
      [[INTERPRETER-KILLED]
       (*%kernel-exit-interpreter-func* #f)]
      [else
       (fatal 'halyard "Unknown interpreter state")]))


  ;;=======================================================================
  ;;  Support for loading code
  ;;=======================================================================
  ;;  If a function might trigger lazy loading, we need to call it from
  ;;  within a callback context.  Consider the following scenario:
  ;;
  ;;   1) The script has called WAIT on some media.
  ;;   2) The engine is 'PAUSED until the WAIT is done.
  ;;   3) The user triggers a load from the GUI.
  ;;   4) Loading code tries to call a primitive.
  ;;
  ;; In a callback context, this should work, because we support call-prim
  ;; and all the usual side effects in a reasonably plausible way.  Without
  ;; the callback context, calling call-prim will typically cause an engine
  ;; freeze sooner or later, because call-prim hangs when 'PAUSED.

  (provide check-whether-safe-to-load-code script-load-error)

  (define (check-whether-safe-to-load-code node-name)
    (unless (eq? *%kernel-state* 'NORMAL)
      (fatal 'halyard "Cannot load " node-name " right now, probably "
             "because some code in kernel.ss needs to be "
             "wrapped with with-code-loading-allowed "
             "(try a C++ debugger to find it)")))

  (define (call-with-code-loading-allowed thunk exception-handler error-value)
    (let [[result #f]]
      (%kernel-run-as-callback
       (lambda ()
         (set! result (thunk)))
       (lambda (exn)
         (exception-handler exn)
         (set! result error-value)))
      result))

  (define-syntax with-code-loading-allowed
    (syntax-rules ()
      [(_ (exception-handler error-value) body ...)
       (call-with-code-loading-allowed (lambda () body ...)
                                       exception-handler error-value)]))

  ;;; Report an error while occurred while loading a script.  This function
  ;;; will not return.  Note that this function may only be called once we
  ;;; enter %main-kernel-loop.
  (define (script-load-error msg)
    ;; The LoadScriptFailed primitive will set *%kernel-state* to
    ;; 'INTERPRETER-KILLED, which will cause call-prim to perform a
    ;; non-local exit using *%kernel-exit-interpreter-func*.  This will
    ;; only work if we're inside %main-kernel-loop, and not during the
    ;; initial script load.
    (assert (function? *%kernel-exit-interpreter-func*))
    (logger 'error 'halyard msg)
    (call-prim 'LoadScriptFailed))


  ;;=======================================================================
  ;;  Node/Engine Interface
  ;;=======================================================================

  (define (enable-expensive-events enable?)
    (when (have-prim? 'EnableExpensiveEvents)
      (call-prim 'EnableExpensiveEvents enable?)))

  (define-class %real-engine% (%engine%)
    (def (set-event-handled?! handled?)
      (set! (engine-var '_pass) (not handled?)))

    (def (set-event-vetoed?! vetoed?)
      (set! (engine-var '_veto) vetoed?))

    (def (jump-to-card target)
      (jump-to-card target))

    (def (register-group-member node loaded?)
      (%kernel-register-group-member node loaded?))

    (def (enable-expensive-events enable?)
      (enable-expensive-events enable?))

    (def (notify-enter-card card)
      (when (have-prim? 'NotifyEnterCard)
        (call-prim 'NotifyEnterCard (card .full-name)))
      (call-hook-functions *enter-card-hook* card))

    (def (notify-exit-card card)
      (call-hook-functions *exit-card-hook* card)
      (when (have-prim? 'NotifyExitCard)
        (call-prim 'NotifyExitCard (card .full-name))))
    
    (def (notify-card-body-finished card)
      (call-hook-functions *card-body-finished-hook*)
      (refresh))
    
    (def (delete-element elem)
      ;; A little placeholder to make deletion work the same way in Halyard
      ;; and in Common test.
      ;; TODO - Remove when cleaning up element deletion.
      (when (have-prim? 'DeleteElements)
        (call-prim 'DeleteElements (elem .full-name))))

    (def (exit-node node)
      (call-prim 'StateDbUnregisterListeners (node .full-name))
      (%kernel-cancel-deferred-thunks-for node))
    )

  (set-engine! (%real-engine% .new))

  ;; Set up our event handling machinery.
  (enable-expensive-events #f)
  (when (have-prim? 'RegisterEventDispatcher)
    (call-prim 'RegisterEventDispatcher
                  dispatch-event-to-current-group-member))


  ;;=======================================================================
  ;;  Scanning Source Files
  ;;=======================================================================
  ;;  See tags.ss for more information.

  (provide set-extract-definitions-fn!)
  
  (define *extract-definitions-fn* #f)
  
  (define (set-extract-definitions-fn! f)
    (set! *extract-definitions-fn* f))
  
  (define (%kernel-extract-definitions file-path)
    (with-exceptions-blocked (report-fatal-exception)
      (define path (build-path file-path))
      (%assert *extract-definitions-fn*)
      ;; We want to ignore errors here (unless we're debugging tags.ss),
      ;; because *EXTRACT-DEFINITIONS-FN* regularly blows up when
      ;; encountering malformed source code, especially things which
      ;; READ-SYNTAX can't lex.
      (%kernel-run-as-callback
       (lambda ()
         (*extract-definitions-fn* path))
       (lambda (exn)
         (debug 'halyard "ScriptEditorDB: " (exn-message exn))))))
  

  ;;=======================================================================
  ;;  Cards
  ;;=======================================================================
  ;;  Older support code for cards.  Some of this should probably be
  ;;  refactored elsewhere; we'll see.

  (provide find-card card-exists? card-name)
  
  (define (%kernel-register-group-member node loaded?)
    (when (have-prim? 'RegisterGroupMember)
      (call-prim 'RegisterGroupMember (node .full-name)
                 (node .subclass-of? %card%) loaded?)))

  (define (find-card card-or-name
                     &opt (not-found
                           (lambda ()
                             (error-with-extra-dialog (cat "Unknown card: " 
                                                           card-or-name)))))
    (cond
     [(card? card-or-name)
      card-or-name]
     [(symbol? card-or-name)
      (let [[node (find-node card-or-name #f)]]
        (if (and node (card? node))
            node
            (not-found)))]
     [(string? card-or-name)
      (find-card (string->symbol card-or-name) not-found)]
     [#t
      (error-with-extra-dialog (cat "Not a card: " card-or-name))]))

  (define (card-exists? name)
    (define result #t)
    (find-card name (lambda () (set! result #f)))
    result)

  (define (card-name card-or-name)
    (cond
     [(card? card-or-name)
      (card-or-name .full-name)]
     [(symbol? card-or-name)
      (symbol->string card-or-name)]
     [(string? card-or-name)
      card-or-name]
     [#t
      (error-with-extra-dialog (cat "Not a card: " card-or-name))]))

  ) ; end module
