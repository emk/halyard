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
  (require #%engine-primitives
           (lib "cm.ss" "mzlib")
           (lib "errortrace-lib.ss" "errortrace"))

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
  ;; alive, and (2) call UPDATE-SPLASH-SCREEN!, so that user knows that
  ;; we're still alive.  So we take our regular loader (specified by the
  ;; parameter LOAD/USE-COMPILED), and return a new function which wraps it
  ;; in the appropriate magic.
  (define (wrap-load/use-compiled-with-heartbeat load/use-compiled)
    ;; Return a LOAD/USE-COMPILED handler.
    (lambda (file-path expected-module-name)
      ;; We call HEARTBEAT twice, before and after, just to be on the
      ;; safe side.  We may have had a really specific reason for this,
      ;; but if so, I don't remember it.
      (heartbeat)
      ;;(debug-log (string-append "Loading: " (path->string file-path)))
      (let [[result (load/use-compiled file-path expected-module-name)]]
        (heartbeat)
        (update-splash-screen!)
        result)))

  ;; There's some sort of context dependence that makes it so we can't just 
  ;; define COMPILE-ZO-WITH-HEARTBEAT out here, probably due to some paths 
  ;; not being set up that MAKE-COMPILATION-MANAGER-LOAD/USE-COMPILED-HANDLER
  ;; uses, so instead we just wrap this all in a function that will return our
  ;; COMPILE-ZO-WITH-HEARTBEAT function, and call it at the appropriate time.
  (define (make-compile-zo-with-heartbeat)
    ;; A suitable function to use with CURRENT-LOAD/USE-COMPILED.  This
    ;; handles automatic compilation of *.zo files for us.
    (define compile-zo (make-compilation-manager-load/use-compiled-handler))
    (wrap-load/use-compiled-with-heartbeat compile-zo))

  ;; We use this loader function when we don't want to even *think* about
  ;; recompiling *.zo files.
  (define (make-load-with-heartbeat)
    (wrap-load/use-compiled-with-heartbeat (current-load/use-compiled)))

  (define (stage2)
    ;; Decide whether or not we should always trust (and use) compiled *.zo
    ;; files.  This will generally only be true if our code was installed
    ;; by a prepackaged installer.
    (let [[always-trust-precompiled?
           (and always-treat-zo-and-so-as-newer-available?
                (file-exists? (build-path (current-directory)
                                          "TRUST-PRECOMPILED")))]
          [filename "none"]]

      (initialize-splash-screen!)
        
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
           ;; Load *.zo files blindly if they exist.
           (make-load-with-heartbeat)
           ;; Recompile *.zo files on demand.
           (make-compile-zo-with-heartbeat)))
        
      ;; Support for decent backtraces upon errors.  We pull in 
      ;; the support from errortrace-lib.ss, and then manually enable
      ;; errortrace if requested.  Note that we always require 
      ;; errortrace-lib.ss so we will have stable file counts in 
      ;; application.halyard.
      (set! filename "errortrace-lib.ss")
      (when (errortrace-compile-enabled?)
        ;; Re-implement the logic from errortrace.ss.
        (current-compile errortrace-compile-handler)
        (use-compiled-file-paths (list (build-path "compiled" "errortrace")
                                       (build-path "compiled"))))

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

  (stage2))
