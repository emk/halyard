(module kernel (lib "language.ss" "5L")

  ;; Import %call-5l-prim from the engine.
  (require #%fivel-engine)

  ;; XXX - This gets automatically loaded sometimes, but not always.  In an
  ;; effort to keep our progress bar's "files to load" count correct, we
  ;; always load this whether or not it is needed.
  (require (lib "cm-ctime.ss" "mzlib" "private"))

  ;; Get begin/var, and re-export it.
  (require (lib "begin-var.ss" "5L"))
  (provide begin/var)
  (provide define/var)
  
  ;; Get hooks, and re-export them.
  (require (lib "hook.ss" "5L"))
  (provide (all-from (lib "hook.ss" "5L")))

  ;; Various support code and declarations refactored out of the kernel.
  (require (lib "types.ss" "5L"))
  (provide (all-from (lib "types.ss" "5L")))
  (require (lib "util.ss" "5L"))
  (provide (all-from (lib "util.ss" "5L")))
  (require (lib "nodes.ss" "5L"))
  (provide (all-from (lib "nodes.ss" "5L")))
  (require (lib "events.ss" "5L"))
  (provide (all-from (lib "events.ss" "5L")))
  (require (lib "indent.ss" "5L"))
  (provide (all-from (lib "indent.ss" "5L")))

  ;; Get format-result-values.
  (require (lib "trace.ss" "5L"))


  ;;=======================================================================
  ;;  Standard Hooks
  ;;=======================================================================
  
  (provide *enter-card-hook* *exit-card-hook*
           *card-body-finished-hook* *before-draw-hook*
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
  ;;  Core 5L API
  ;;=======================================================================
  ;;  We only declare a small number of primitives here, typically those
  ;;  which are needed by the kernel or which intimately depend on the
  ;;  kernel's inner workings.  The rest of these functions can be found
  ;;  in the 5L-API module.
  
  (provide call-5l-prim have-5l-prim? value->boolean idle blocking-idle
           engine-var set-engine-var! engine-var-exists?
           exit-script refresh)

  ;; C++ can't handle very large or small integers.  Here are the
  ;; typical limits on any modern platform.
  (define *32-bit-signed-min* -2147483648)
  (define *32-bit-signed-max* 2147483647)
  (define *32-bit-unsigned-min* 0)
  (define *32-bit-unsigned-max* 4294967295)

  (define (call-5l-prim . args)
    ;; Our high-level wrapper for %call-5l-prim (which is defined by
    ;; the engine).  You should almost always call this instead of
    ;; %call-5l-prim, because this function calls %kernel-check-state
    ;; to figure out what happened while we were in C++.
    (let [[result (apply %call-5l-prim args)]]
      (%kernel-check-state)
      result))
  
  (define (have-5l-prim? name)
    (%call-5l-prim 'HavePrimitive name))

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
    ;; We call %call-5l-prim directly to avoid using rest arguments or
    ;; 'apply', both of which cons (which we don't want to happen in the
    ;; idle loop.)
    (%call-5l-prim 'SchemeIdle blocking?)
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

  (define (engine-var name)
    (call-5l-prim 'Get (if (string? name) (string->symbol name) name)))
  
  (define (set-engine-var! name value)
    ;; Set an engine variable.  This is a pain, because we have to play
    ;; along with the engine's lame type system.
    (let [[namesym (if (string? name) (string->symbol name) name)]]
      (call-5l-prim 'SetTyped namesym value)))

  (define (engine-var-exists? name)
    (call-5l-prim 'VariableInitialized name))
  
  ;; TODO - replace most calls to this function with calls to ERROR.
  (define (error-with-extra-dialog msg)
    ;; TODO - More elaborate error support.
    (non-fatal-error msg)
    (error msg))
  
  ;;; Exit the currently-running script.
  (define (exit-script)
    ;; Call the appropriate exit primitive.
    (if (have-5l-prim? 'TamaleExit)
        (call-5l-prim 'TamaleExit)
        (call-5l-prim 'SchemeExit)))
  
  (define (check-whether-jump-allowed)
    (when *running-on-exit-handler-for-node*
      (error (cat "Cannot JUMP in (on exit () ...) handler for node "
                  (node-full-name *running-on-exit-handler-for-node*)))))

  (define (jump-to-card card)
    (check-whether-jump-allowed)
    (if (have-5l-prim? 'Jump)
        (call-5l-prim 'Jump (card-name card))
        (begin
          ;; If we don't have a JUMP primitive, fake it by hand.
          (set! *%kernel-jump-card* card)
          (%kernel-set-state 'JUMPING)
          (%kernel-check-state))))

  ;;; Show the results of all drawing calls made since the last screen
  ;;; update.  (The screen is only updated after calls to IDLE, REFRESH,
  ;;; WAIT, and similar functions.)
  (define (refresh &key (transition 'none) (ms 500))
    ;; Refresh the screen by blitting dirty areas of our offscreen buffer
    ;; to the display.
    (call-hook-functions *before-draw-hook*)
    (if (have-5l-prim? 'Refresh)
        (call-5l-prim 'Refresh transition ms)))


  ;;=======================================================================
  ;;  Kernel Entry Points
  ;;=======================================================================
  ;;  The '%kernel-' methods are called directly by the 5L engine.  They
  ;;  shouldn't raise errors, because they're called directly from C++ code
  ;;  that doesn't want to catch them (and will, in fact, quit the
  ;;  program).
  ;;
  ;;  The theory behind these functions is documented in detail in
  ;;  TInterpreter.h.

  (define (%kernel-run)
    ;; The workhorse function.  We get called to manage the main event
    ;; loop, and we provide support code for handling jumps, STOPPING
    ;; the interpreter, idling after the end of each card, and quiting
    ;; the interpreter.  This code needs to be understood in
    ;; conjuction with *%kernel-state*, the functions that manipulate
    ;; it, and the callback system.  Yes, it's ugly--but it lets us
    ;; get the semantics we want without writing an entire interpreter
    ;; from scratch.
    (with-errors-blocked (fatal-error)
      (label exit-interpreter
        (fluid-let ((*%kernel-exit-interpreter-func* exit-interpreter))
          (let ((jump-card #f))
            (let loop []
              (label exit-to-top
                (with-errors-blocked (non-fatal-error)
                  (fluid-let ((*%kernel-exit-to-top-func* exit-to-top))
                    (cond
                     [jump-card
                      (run-card (find-card jump-card))]
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
      (notify-exit-script)))

  (define (notify-exit-script)
    (with-errors-blocked (non-fatal-error)
      (define current-node-or-false (*engine* .current-group-member))
      (call-hook-functions *dangerous-exit-script-hook*
                           current-node-or-false)))

  (define (%kernel-kill-interpreter)
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
    (with-errors-blocked (non-fatal-error)
      (%kernel-die-if-callback '%kernel-pause)
      (%kernel-set-state 'PAUSED)))

  (define (%kernel-wake-up)
    (with-errors-blocked (non-fatal-error)
      (%kernel-die-if-callback '%kernel-wake-up)
      (when (%kernel-paused?)
        (%kernel-clear-state))))

  (define (%kernel-paused?)
    (eq? *%kernel-state* 'PAUSED))

  (define (%kernel-kill-current-card)
    (%kernel-set-state 'CARD-KILLED))

  (define (%kernel-jump-to-card-by-name card-name)
    (set! *%kernel-jump-card* card-name)
    (%kernel-set-state 'JUMPING))

  (define (%kernel-current-card-name)
    (if (*engine* .current-card)
        (value->string (node-full-name (*engine* .current-card)))
        ""))

  (define (%kernel-valid-card? card-name)
    (card-exists? card-name))

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
       (lambda (error-msg)
         (set! ok? #f)
         (set! result error-msg)))
      
      ;; Return the result.
      (cons ok? result)))

  ;;; Fetch all identifiers in the top-level namespace.  We will probably
  ;;; want to add support for querying specific modules (MODULE->NAMESPACE
  ;;; will help).
  (define (%kernel-get-identifiers)
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
           [(function? val)
            (if (treat-as-syntax? sym)
              'syntax
              'function)]
           [#t 'variable]))))
    (define (sym->record sym)
      (list sym (sym->type sym) (syntax-indent sym)))
    (map sym->record (namespace-mapped-symbols)))
  
  (define (%kernel-run-callback function args)
    (%kernel-run-as-callback (lambda () (apply function args))
                             non-fatal-error))

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
  
  (provide run-deferred executing-deferred-safe-time-callbacks?)

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
  (define *%kernel-exit-interpreter-func* #f)
  (define *%kernel-exit-to-top-func* #f)
  
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
      (when (have-5l-prim? 'WakeUpIfNecessary)
        (%call-5l-prim 'WakeUpIfNecessary))))

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
          (debug-log "Running deferred thunk.")
          ((deferred-action-thunk item))
          (debug-log "Finished running deferred thunk.")))))

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
  
  (define (%kernel-run-as-callback thunk error-handler)
    ;; This function is in charge of running callbacks from C++ into
    ;; Scheme.  These include simple callbacks and anything evaled from
    ;; C++.  When we're in a callback, we need to install special values
    ;; of *%kernel-exit-to-top-func* and *%kernel-exit-interpreter-func*,
    ;; because the values installed by %kernel-run can't be invoked without
    ;; "throwing" across C++ code, which isn't allowed (and which would
    ;; be disasterous).  Furthermore, we must trap all errors, because
    ;; our C++-based callers don't want to deal with Scheme exceptions.
    (if (eq? *%kernel-state* 'INTERPRETER-KILLED)
        (5l-log "Skipping callback because interpreter is being shut down")
        (let [[saved-kernel-state *%kernel-state*]]
          (set! *%kernel-state* 'NORMAL)
          (label exit-callback
            ;; TODO - Can we have better error handling?
            (with-errors-blocked (error-handler)
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
       (%call-5l-prim 'SchemeIdle #t) ; Similar to blocking-idle.
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
       (fatal-error "Unknown interpreter state")]))


  ;;=======================================================================
  ;;  Node/Engine Interface
  ;;=======================================================================

  (define (enable-expensive-events enable?)
    (when (have-5l-prim? 'EnableExpensiveEvents)
      (call-5l-prim 'EnableExpensiveEvents enable?)))

  (define-class %real-engine% (%engine%)
    (def (set-event-handled?! handled?)
      (set! (engine-var '_pass) (not handled?)))

    (def (set-event-vetoed?! vetoed?)
      (set! (engine-var '_veto) vetoed?))

    (def (jump-to-card target)
      (jump-to-card target))

    (def (register-card card)
      (%kernel-register-card card))

    (def (enable-expensive-events enable?)
      (enable-expensive-events enable?))

    (def (notify-enter-card card)
      (when (have-5l-prim? 'NotifyEnterCard)
        (call-5l-prim 'NotifyEnterCard (node-full-name card)))
      (call-hook-functions *enter-card-hook* card))

    (def (notify-exit-card card)
      (call-hook-functions *exit-card-hook* card)
      (when (have-5l-prim? 'NotifyExitCard)
        (call-5l-prim 'NotifyExitCard (node-full-name card))))
    
    (def (notify-card-body-finished card)
      (call-hook-functions *card-body-finished-hook*)
      (refresh))
    
    (def (delete-element elem)
      ;; A little placeholder to make deletion work the same way in Tamale
      ;; and in Common test.
      ;; TODO - Remove when cleaning up element deletion.
      (when (have-5l-prim? 'DeleteElements)
        (call-5l-prim 'DeleteElements (node-full-name elem))))

    (def (exit-node node)
      (call-5l-prim 'StateDbUnregisterListeners (node-full-name node))
      (%kernel-cancel-deferred-thunks-for node))
    )

  (set-engine! (%real-engine% .new))

  ;; Set up our event handling machinery.
  (enable-expensive-events #f)
  (when (have-5l-prim? 'RegisterEventDispatcher)
    (call-5l-prim 'RegisterEventDispatcher
                  dispatch-event-to-current-group-member))


  ;;=======================================================================
  ;;  Scanning Source Files
  ;;=======================================================================
  ;;  See tags.ss for more information.

  (provide set-extract-definitions-fn!)
  
  (define *extract-definitions-fn* #f)
  
  (define (set-extract-definitions-fn! f)
    (set! *extract-definitions-fn* f))
  
  (define (%kernel-extract-definitions relative-file-path)
    (with-errors-blocked (fatal-error)
      (define path (build-path (current-directory) relative-file-path))
      (%assert *extract-definitions-fn*)
      ;; We want to ignore errors here (unless we're debugging tags.ss),
      ;; because *EXTRACT-DEFINITIONS-FN* regularly blows up when
      ;; encountering malformed source code, especially things which
      ;; READ-SYNTAX can't lex.
      (%kernel-run-as-callback
       (lambda ()
         (*extract-definitions-fn* path))
       (lambda (msg)
         (debug-caution (cat "ScriptEditorDB: " msg))))))
  

  ;;=======================================================================
  ;;  Cards
  ;;=======================================================================
  ;;  Older support code for cards.  Some of this should probably be
  ;;  refactored elsewhere; we'll see.

  (provide find-card card-exists? card-name)
  
  (define (%kernel-register-card card)
    (when (have-5l-prim? 'RegisterCard)
      (call-5l-prim 'RegisterCard (node-full-name card))))

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
      (node-full-name card-or-name)]
     [(symbol? card-or-name)
      (symbol->string card-or-name)]
     [(string? card-or-name)
      card-or-name]
     [#t
      (error-with-extra-dialog (cat "Not a card: " card-or-name))]))

  ) ; end module
