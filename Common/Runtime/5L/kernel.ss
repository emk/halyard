(module kernel (lib "lispish.ss" "5L")

  ;; Import %call-5l-prim from the engine.
  (require #%fivel-engine)

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
  (require (lib "indent.ss" "5L"))
  (provide (all-from (lib "indent.ss" "5L")))

  ;; Get format-result-values.
  (require (lib "trace.ss" "5L"))


  ;;=======================================================================
  ;;  Standard Hooks
  ;;=======================================================================
  
  (provide *enter-card-hook* *exit-card-hook*
           *card-body-finished-hook* *before-draw-hook*)

  ;; Called before running each card.
  (define *enter-card-hook* (make-hook 'enter-card))

  ;; Called immediately before moving to a new card.
  (define *exit-card-hook* (make-hook 'exit-card))

  ;; Called after running the body of a each card, if the body exits
  ;; normally (not by jumping, etc.).
  (define *card-body-finished-hook* (make-hook 'card-body-finished))

  ;; Called before *most* screen redraws.
  (define *before-draw-hook* (make-hook 'before-draw))


  ;;=======================================================================
  ;;  Core 5L API
  ;;=======================================================================
  ;;  We only declare a small number of primitives here, typically those
  ;;  which are needed by the kernel or which intimately depend on the
  ;;  kernel's inner workings.  The rest of these functions can be found
  ;;  in the 5L-API module.
  
  (provide call-5l-prim have-5l-prim? value->boolean idle blocking-idle
           engine-var set-engine-var! engine-var-exists?  throw 
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
    (%call-5l-prim 'haveprimitive name))

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
    (%call-5l-prim 'schemeidle blocking?)
    (%kernel-check-state))

  (define (blocking-idle)
    (%idle #t))

  (define (idle)
    (%idle #f))
  
  (define (engine-var name)
    (call-5l-prim 'get (if (string? name) (string->symbol name) name)))
  
  (define (set-engine-var! name value)
    ;; Set an engine variable.  This is a pain, because we have to play
    ;; along with the engine's lame type system.
    (let [[namesym (if (string? name) (string->symbol name) name)]]
      (call-5l-prim 'settyped namesym value)))

  (define (engine-var-exists? name)
    (call-5l-prim 'VariableInitialized name))
  
  (define (throw msg)
    ;; TODO - More elaborate error support.
    (non-fatal-error msg)
    (error msg))
  
  (define (exit-script)
    (call-5l-prim 'schemeexit))
  
  (define (check-whether-jump-allowed)
    (when *running-on-exit-handler-for-node*
      (error (cat "Cannot JUMP in (on exit () ...) handler for node "
                  (node-full-name *running-on-exit-handler-for-node*)))))

  (define (jump-to-card card)
    (check-whether-jump-allowed)
    (if (have-5l-prim? 'jump)
        (call-5l-prim 'jump (card-name card))
        (begin
          ;; If we don't have a JUMP primitive, fake it by hand.
          (set! *%kernel-jump-card* card)
          (%kernel-set-state 'JUMPING)
          (%kernel-check-state))))

  (define (refresh &key (transition 'none) (ms 500))
    ;; Refresh the screen by blitting dirty areas of our offscreen buffer
    ;; to the display.
    (call-hook-functions *before-draw-hook*)
    (if (have-5l-prim? 'refresh)
        (call-5l-prim 'refresh transition ms)))


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
                    (idle)
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
                              (idle))
                          (idle-loop)))]))))
              (set! jump-card #f)
              (when (eq? *%kernel-state* 'JUMPING)
                ;; Handle a jump by setting jump-card for our next goaround.
                (set! jump-card *%kernel-jump-card*))
              (%kernel-maybe-clear-state)
              (loop)))))
      (%kernel-maybe-clear-state)
      (%kernel-clear-timeout)))

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

  (define (%kernel-timeout card-name seconds)
    (with-errors-blocked (non-fatal-error)
      (%kernel-die-if-callback '%kernel-timeout)
      (%kernel-set-timeout (+ (current-milliseconds) (* seconds 1000))
                           (lambda () (jump (find-card card-name))))))
    
  (define (%kernel-nap tenths-of-seconds)
    (with-errors-blocked (non-fatal-error)
      (%kernel-die-if-callback '%kernel-nap)
      (set! *%kernel-nap-time* (+ (current-milliseconds)
                                  (* tenths-of-seconds 100)))
      (%kernel-set-state 'NAPPING)))

  (define (%kernel-napping?)
    (eq? *%kernel-state* 'NAPPING))

  (define (%kernel-kill-nap)
    (with-errors-blocked (non-fatal-error)
      (%kernel-die-if-callback '%kernel-kill-nap)
      (when (%kernel-napping?)
        (%kernel-clear-state))))

  (define (%kernel-kill-current-card)
    (%kernel-set-state 'CARD-KILLED))

  (define (%kernel-jump-to-card-by-name card-name)
    (set! *%kernel-jump-card* card-name)
    (%kernel-set-state 'JUMPING))

  (define (%kernel-current-card-name)
    (if (engine-current-card *engine*)
        (value->string (node-full-name (engine-current-card *engine*)))
        ""))

  (define (%kernel-previous-card-name)
    (if (engine-last-card *engine*)
        (value->string (node-full-name (engine-last-card *engine*)))
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
      (with-handlers [[exn:variable? (lambda (exn) 'variable)] ;; unbound var
                      [exn:syntax? (lambda (exn) 'syntax)]]    ;; a macro
        (let [[val (namespace-variable-value sym)]]
          (cond
           [(function? val) 'function]
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
  ;;    NAPPING:  The interpreter should pause the current card for the
  ;;              specified number of milliseconds.
  ;;    CARD-KILLED: The interpreter should stop executing the current
  ;;               card, and return to the top-level loop.
  ;;    INTERPRETER-KILLED: The interpreter should exit.
  ;;
  ;;  Callbacks are slightly special, however--see the code for details.
  
  (provide call-at-safe-time)

  ;; The most important global state variables.
  (define *%kernel-running-callback?* #f)
  (define *%kernel-state* 'NORMAL)

  ;; Some slightly less important global state variables.  See the
  ;; functions which define and use them for details.
  (define *%kernel-jump-card* #f)
  (define *%kernel-timeout* #f)
  (define *%kernel-timeout-thunk* #f)
  (define *%kernel-nap-time* #f)

  ;; Deferred thunks are used to implement (deferred-callback () ...).
  ;; See call-at-safe-time for details.
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
        (throw (cat "Cannot call " name " from within callback."))))

  (define (%kernel-clear-timeout)
    (set! *%kernel-timeout* #f)
    (set! *%kernel-timeout-thunk* #f))

  (define (%kernel-set-timeout time thunk)
    (when *%kernel-timeout*
      (debug-caution "Installing new timeout over previously active one."))
    (set! *%kernel-timeout* time)
    (set! *%kernel-timeout-thunk* thunk))
  
  (define (%kernel-check-timeout)
    ;; If we have a timeout to run, and it's a safe time to run it, do so.
    (if (%kernel-stopped?)
        (%kernel-clear-timeout)
        (unless *%kernel-running-callback?*
          (when (and *%kernel-timeout*
                     (>= (current-milliseconds) *%kernel-timeout*))
            (let ((thunk *%kernel-timeout-thunk*))
              (%kernel-clear-timeout)
              (thunk))))))

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

  (define (%kernel-safe-to-run-deferred-thunks?)
    ;; Would now be a good time to run deferred thunks?  Wait until
    ;; nothing exciting is happening.  See call-at-safe-time.
    (and (not *%kernel-running-callback?*)
         (not *%kernel-running-deferred-thunks?*)
         (not *running-on-exit-handler-for-node*)
         (member? *%kernel-state* '(NORMAL PAUSED NAPPING))))

  (define (call-at-safe-time thunk)
    ;; Make sure we run 'thunk' at the earliest safe time, but not
    ;; in a callback.  This can be used to defer calls to video, input,
    ;; and other functions which can't be called from a callback.
    (if (%kernel-safe-to-run-deferred-thunks?)
        (thunk)
        (set! *%kernel-deferred-thunk-queue*
              (cons thunk *%kernel-deferred-thunk-queue*)))
    #f)
    
  (define (%kernel-check-deferred)
    ;; If the interpreter has stopped, cancel any deferred thunks.  (See
    ;; call-at-safe-time.)  This function won't call any deferred thunks
    ;; unless it is safe to do so.
    (when (%kernel-stopped?)
      (set! *%kernel-deferred-thunk-queue* '()))   

    ;; Run any deferred thunks.
    (unless (or (null? *%kernel-deferred-thunk-queue*)
                (not (%kernel-safe-to-run-deferred-thunks?)))

      ;; Make a copy of the old queue and clear the global variable
      ;; (which may be updated behind our backs).
      (let [[items (reverse *%kernel-deferred-thunk-queue*)]]
        (set! *%kernel-deferred-thunk-queue* '())

        ;; Run every thunk in the queue, in order.
        (fluid-let [[*%kernel-running-deferred-thunks?* #t]]
          (foreach [item items]
            (item))))

      ;; Check to see if any new items appeared in the queue while we
      ;; were running the first batch.
      (%kernel-check-deferred)))

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
    ;; primitive call (including idle).  It's job is to check flags
    ;; set by the engine, handle any deferred callbacks, check
    ;; timeouts, and generally bring the Scheme world back into sync
    ;; with everybody else.
    ;; 
    ;; Since this is called after idle, it needs to be *extremely*
    ;; careful about allocating memory.  See %kernel-run for more
    ;; discussion about consing in the idle loop (hint: it's bad).
    (%kernel-wake-up-if-necessary)  ; Should be the first thing we do.
    (%kernel-check-deferred)        ; Should be the second thing we do.
    ;; XXX - I'm no longer convinced that we should call
    ;; %KERNEL-CHECK-DEFERRED before dealing with the value of
    ;; *%KERNEL-STATE* (below), because it sometimes allows jumps to get
    ;; lost, and other bits of weirdness.  But I'm not willing to redesign
    ;; this very tricky and important routine without a lot of thought,
    ;; particularly during a relatively stable period of engine
    ;; development.  Feel free to revisit this later.
    (unless *%kernel-running-callback?*
      (%kernel-check-timeout))
    (case *%kernel-state*
      [[NORMAL STOPPED]
       #f]
      [[STOPPING]
       (when *%kernel-exit-to-top-func*
             (*%kernel-exit-to-top-func* #f))]
      [[PAUSED]
       (%call-5l-prim 'schemeidle #f)
       (%kernel-check-state)]       ; Tail-call self without consing.
      [[NAPPING]
       (if (< (current-milliseconds) *%kernel-nap-time*)
           (begin
             (%call-5l-prim 'schemeidle #f)
             (%kernel-check-state)) ; Tail call self without consing.
           (%kernel-clear-state))]
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

  (defclass <real-engine> (<engine>))

  (defmethod (set-engine-event-handled?! (eng <engine>) (handled? <boolean>))
    (set! (engine-engine-var *engine* '_pass) (not handled?)))

  (defmethod (set-engine-event-vetoed?! (eng <engine>) (vetoed? <boolean>))
    (set! (engine-engine-var *engine* '_veto) vetoed?))

  (defmethod (set-engine-engine-var! (eng <real-engine>) (name <symbol>) value)
    (set-engine-var! name value))

  (defmethod (engine-jump-to-card (eng <real-engine>) (target <card>))
    (jump-to-card target))

  (defmethod (engine-register-card (eng <real-engine>) (card <card>))
    (%kernel-register-card card))

  (defmethod (engine-enable-expensive-events (engine <real-engine>)
                                             (enable? <boolean>))
    (enable-expensive-events enable?))

  (define (enable-expensive-events enable?)
    (when (have-5l-prim? 'EnableExpensiveEvents)
      (call-5l-prim 'EnableExpensiveEvents enable?)))

  (defmethod (engine-notify-enter-card (engine <real-engine>) (card <card>))
    (%kernel-clear-timeout)
    (call-5l-prim 'resetorigin)
    (when (have-5l-prim? 'notifyentercard)
      (call-5l-prim 'notifyentercard (node-full-name card)))
    (call-hook-functions *enter-card-hook* card))

  (defmethod (engine-notify-exit-card (engine <real-engine>) (card <card>))
    (call-hook-functions *exit-card-hook* card)
    (when (have-5l-prim? 'notifyexitcard)
      (call-5l-prim 'notifyexitcard (node-full-name card))))

  (defmethod (engine-notify-card-body-finished (eng <real-engine>)
                                               (card <card>))
    (call-hook-functions *card-body-finished-hook*)
    (refresh))

  (defmethod (engine-delete-element (engine <real-engine>)
                                    (elem <element>))
    ;; A little placeholder to make deletion work the same way in Tamale
    ;; and in Common test.
    ;; TODO - Remove when cleaning up element deletion.
    (when (have-5l-prim? 'deleteelements)
      (call-5l-prim 'deleteelements (node-full-name elem))))

  (defmethod (engine-state-db-unregister-listeners (engine <real-engine>)
                                                   (node <node>))
    (call-5l-prim 'StateDbUnregisterListeners (node-full-name node)))

  (set-engine! (make <real-engine>))

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
      (define path (apply build-path (current-directory)
                          (regexp-split "/" relative-file-path)))
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
                             (throw (cat "Unknown card: " card-or-name)))))
    (cond
     [(card? card-or-name)
      card-or-name]
     [(symbol? card-or-name)
      (let [[node (find-node card-or-name)]]
        (if (and node (instance-of? node <card>))
            node
            (not-found)))]
     [(string? card-or-name)
      (find-card (string->symbol card-or-name) not-found)]
     [#t
      (throw (cat "Not a card: " card-or-name))]))

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
      (throw (cat "Not a card: " card-or-name))]))

  ) ; end module
