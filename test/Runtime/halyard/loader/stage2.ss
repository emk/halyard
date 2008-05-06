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

(module stage2 mzscheme
  (require #%engine-primitives)

  ;;===== Primitive functions =====
  
  ;; Call the specified engine primitive if it exists.
  (define (maybe-call-prim name . args)    
    (when (%call-prim 'HavePrimitive name)
      (apply %call-prim name args)))

  ;; Notify the operating system that we're still alive, and haven't hung.
  (define (heartbeat)
    (maybe-call-prim 'Heartbeat))

  ;; Load a splash screen at the standard location, assuming it exists.
  (define (maybe-load-splash path)
    (maybe-call-prim 'MaybeLoadSplash path))

  ;; Force a screen update.
  (define (refresh-splash-screen)
    (maybe-call-prim 'RefreshSplashScreen))

  ;; Let the engine know we've loaded a file.
  (define (notify-file-loaded)
    (maybe-call-prim 'NotifyFileLoaded))

  ;; Let the engine know we've loaded all the files we plan to load.
  (define (notify-script-loaded)
    (maybe-call-prim 'NotifyScriptLoaded))

  ;; Draw the progress bar for the current status of our load.
  (define (draw-load-progress)
    (maybe-call-prim 'DrawLoadProgress))

  ;; Get the official directory we should be loading scripts from.
  (define (scripts-directory-name)
    (%call-prim 'ScriptsDirectoryName))

  ;; Should we be compiling our files using the errortrace instrumentation?
  (define (errortrace-compile-enabled?)
    (if (%call-prim 'HavePrimitive 'ErrortraceCompileEnabled)
      (%call-prim 'ErrortraceCompileEnabled)
      #f))
  
  ;; Throw an error that displays a message in a dialog box and then takes
  ;; down the engine without submitting a crash report.
  (define (environment-error message)
    (%call-prim 'Log 'halyard message 'environmenterror))

  ;; Write a line to the debug log.
  (define (debug-log msg)
    (%call-prim 'Log 'Debug msg 'log))

  ;;===== Splash screen management =====

  ;; The time we started loading the script.
  (define *load-start-time* #f)

  ;; Do we need to display a second splash screen at the appropriate
  ;; moment?
  (define *showing-first-splash-screen?* #t)

  ;; Keep track of when we started to load the program, and which
  ;; splash-screen we're showing.
  (define (initialize-splash-screen!)
    (set! *load-start-time* (current-milliseconds))
    (set! *showing-first-splash-screen?* #t))
  
  ;; Make any necessary updates to the splash screen.
  (define (update-splash-screen!)
    (notify-file-loaded)
    (when (and *showing-first-splash-screen?*
               (> (current-milliseconds) (+ *load-start-time* 2000)))
      (set! *showing-first-splash-screen?* #f)
      (maybe-load-splash "splash2.png"))
    (unless *showing-first-splash-screen?*
      (draw-load-progress))
    (refresh-splash-screen))

  ;;===== Compilation support =====

  ;; Trace our compilation status.
  (define (trace msg)
    ;;(printf "~s~n" msg)
    (debug-log msg))

  ;; This parameter is only available in our custom-patched version of PLT
  ;; Scheme.  If it's available, we want to use it, but if not, we need to
  ;; disable our support for TRUST-PRECOMPILED.
  ;;
  ;; Note that if we don't have always-treat-zo-and-so-as-newer, we define
  ;; a dummy parameter to take its place.  This makes it easier for us to
  ;; use PARAMETERIZE, below, without having to worry about whether we
  ;; found this parameter.
  (define always-treat-zo-and-so-as-newer-available? #t)
  (define %always-treat-zo-and-so-as-newer
    (with-handlers [[exn:fail:contract?
                     (lambda (exn)
                       (set! always-treat-zo-and-so-as-newer-available? #f)
                       (make-parameter #f))]]
      (dynamic-require 'mzscheme 'always-treat-zo-and-so-as-newer)))
  
  ;; When loading Scheme files, we need to do two things: (1) Call the
  ;; HEARTBEAT function, so that the operating systems knows we're still
  ;; alive, and (2) call UPDATE-SPLASH-SCREEN!, so that the user knows that
  ;; we're still alive.
  (define (notify-loading-file path)
    (heartbeat)
    (update-splash-screen!))

  ;; Take a load/use-compiled handler (specified by the parameter
  ;; LOAD/USE-COMPILED), and return a new function which wraps it in the
  ;; appropriate magic.
  (define (wrap-load/use-compiled-with-heartbeat load/use-compiled)
    ;; Return a LOAD/USE-COMPILED handler.
    (lambda (file-path expected-module-name)
      ;;(debug-log (string-append "Loading: " (path->string file-path)))
      (let [[result (load/use-compiled file-path expected-module-name)]]
        (notify-loading-file file-path)
        result)))

  ;; We use this loader function when we don't want to even *think* about
  ;; recompiling *.zo files.
  (define (make-load-with-heartbeat)
    (wrap-load/use-compiled-with-heartbeat (current-load/use-compiled)))

  ;;===== Stage 2 of loading =====

  (provide %stage2)

  (define (%stage2 make-compilation-manager-load/use-compiled-handler
                   manager-compile-notify-handler
                   manager-trace-handler)

    ;; Decide whether or not we should always trust (and use) compiled *.zo
    ;; files.  This will generally only be true if our code was installed
    ;; by a prepackaged installer.
    (let [[always-trust-precompiled?
           (and always-treat-zo-and-so-as-newer-available?
                (file-exists? (build-path (current-directory)
                                          "TRUST-PRECOMPILED")))]
          [filename "none"]]

      (initialize-splash-screen!)

      ;; If we're planning to enable errortrace later, then we need to set
      ;; up use-compiled-file-paths _before_ we call
      ;; make-compilation-manager-load/use-compiled-handler.  This is
      ;; necessary because the compilation manager caches the value of
      ;; use-compiled-file-paths, and will refuse to run if it has changed.
      (when (errortrace-compile-enabled?)
        (use-compiled-file-paths (list (build-path "compiled" "errortrace"))))

      ;; If we're running in regular development mode, we want MzScheme
      ;; to transparently compile modules to *.zo files, and recompile
      ;; them whenever necessary.  But if we're running as part of an
      ;; installed program, we have known-good *.zo files (and we may not
      ;; have the privileges needed to overwrite them anyway).  This is
      ;; particularly a problem under Vista, with the new UAC restrictions
      ;; on the "Program Files" directory.
      ;;
      ;; So we check for the presence of TRUST-PRECOMPILED.  If this file
      ;; is absent, we install our custom compilation manager and
      ;; recompile *.zo files as needed.  If this file is absent, we just
      ;; use the default compilation manager, and instruct it to always
      ;; load *.zo files blindly whenever they're present.
      ;;
      ;; It's very important that we install CURRENT-LOAD/USE-COMPILED
      ;; using a dynamically-scoped PARAMETERIZE, and not as a global
      ;; value, because future calls to
      ;; MAKE-COMPILATION-MANAGER-LOAD/USE-COMPILED-HANDLER will wrap
      ;; whatever they find in this parameter, and we don't want to wind
      ;; up with nested compilation managers (a subtle performance
      ;; killer!).
      (%always-treat-zo-and-so-as-newer always-trust-precompiled?)
      (current-load/use-compiled
       (if always-trust-precompiled?
           ;; Load *.zo files blindly if they exist.  We also need to make
           ;; sure that notify-loading-file gets called here, so we use
           ;; make-load-with-heartbeat.
           (make-load-with-heartbeat)
           ;; Recompile *.zo files on demand.  We set up
           ;; notify-loading-file below.
           (make-compilation-manager-load/use-compiled-handler)))

      ;; Install a compilation manager notify-handler.  This will be called
      ;; every time a file is compiled, allowing us to update the splash
      ;; screen, etc.  It will never be called if always-trust-precompiled?
      ;; is #t, but in that case, we'll be using make-load-with-heartbeat.
      (manager-compile-notify-handler notify-loading-file)

      ;; Print out trace information from the compilation manager.
      (manager-trace-handler trace)

      ;; Support for decent backtraces upon errors.  We pull in the support
      ;; from errortrace-lib.ss, and then manually enable errortrace if
      ;; requested.  (This code is a customized version of what happens in
      ;; errortrace.ss.)  Note that we always require errortrace-lib.ss so
      ;; we will have stable file counts in application.halyard.
      ;;
      ;; We have to do this _after_ we install the compilation manager,
      ;; because:
      ;;   1) errortrace must be loaded in the same namespace where we want
      ;;      to use it, or else it will refuse to work.
      ;;   2) errortrace depends on a variety of other modules which might
      ;;      be used by scripts.  And it isn't safe to load any of those
      ;;      modules into this namespace until after the compilation
      ;;      manager is set up, because compiled modules are not allowed
      ;;      to depend on non-compiled modules.
      ;; Of course, this means we don't actually have error-tracing on any
      ;; module which is itself required by errortrace-lib.ss.  Oh, well.
      (let [[errortrace-compile-handler
             (dynamic-require '(lib "errortrace-lib.ss" "errortrace")
                              'errortrace-compile-handler)]]
        (when (errortrace-compile-enabled?)
          (current-compile errortrace-compile-handler)))

      ;; Load the kernel.
      (set! filename "kernel.ss")
      (namespace-require '(lib "kernel.ss" "halyard"))
          
      ;; Provide a reasonable default language for writing scripts.  We
      ;; need to set up both the transformer environment (which is used
      ;; only by code in macro expanders) and the regular environment
      ;; (which is used by normal program code).
      (set! filename "language.ss")
      (namespace-transformer-require '(lib "mizzen.ss" "mizzen"))
      (set! filename "halyard.ss")
      (namespace-require '(lib "halyard.ss" "halyard"))
          
      ;; Load the user's actual script into our new namespace.
      (set! filename "start.ss")
      (load/use-compiled (build-path (current-directory)
                                     (scripts-directory-name) "start.ss"))
      ;; (XXX - Disabled until we can determine why the number of files
      ;; loaded goes up after a "reload script".)
      ;; XXX- Re-enabled because we currently suppress all splash-screen
      ;; code after a "reload script".
      (notify-script-loaded)
      #f))
  )
