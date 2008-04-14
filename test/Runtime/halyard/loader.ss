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
;;  Kernel and Script Loader
;;=========================================================================
;;  This code loads our runtime kernel and our user scripts.  Both the
;;  kernel and the user scripts are loaded into an isolated namespace,
;;  which we can throw away and recreate in a pristine condition whenever
;;  a developer asks us to reload the currently running script.
;;
;;  You can find similar code for the DrScheme IDE in tool.ss.

(module loader mzscheme

  (require #%engine-primitives)

  ;;===== Library collection paths =====
  ;; Before we can use any of load and require machinery, we need to
  ;; have a sane set of collection paths set up.  We want to include
  ;; the following in our collection paths:
  ;;   * Scripts
  ;;   * Runtime
  ;;   * The standard PLT collections
  ;; The standard PLT collections may be in any of a variety of
  ;; locations depending on how we're being run, whether it be on
  ;; Windows or a *nix, whether we're running development or deployed
  ;; code, etc.

  ;; Find the default collects-dir built into our executable.  This
  ;; path may be relative to exec-file's parent directory.  This will
  ;; let us know where PLT thinks it should be finding its
  ;; collections.
  (define (default-collects-dir)
    (call-with-values (lambda () (split-path (find-system-path 'exec-file)))
      (lambda (base name must-be-dir?)
        (let [[collects (find-system-path 'collects-dir)]]
          (if (eq? base 'relative)
            (path->complete-path collects)
            (path->complete-path collects base))))))

  ;; List of places to search for mzlib and other standard PLT
  ;; collections.
  (define $collects-dir-candidates
    (list
     ;; If we're using a released version of the engine, then our
     ;; standard collections will live in Runtime.
     (build-path (current-directory) "Runtime")
     ;; If we're using the system's copy of mzscheme, it will supply
     ;; its own copies of the standard collections.
     (default-collects-dir)
     ;; If all else fails, then maybe we're being run from inside the
     ;; Halyard source tree.  See if we have a checked out copy of
     ;; PLT.
     (build-path (current-directory) 'up "libs" "plt" "collects")))
  
  ;; Search our possible collection directories for a copy of
  ;; "mzlib/lists.ss".  Use the first directory that has it as our
  ;; collects-dir.  We need to do this before we call
  ;; find-library-collection-paths, below, which uses this information
  ;; to come up with a list of paths it wants us to include.  We need
  ;; to store this as our collects-dir, because otherwise our
  ;; compilation-manager gets grumpy and won't compile our code.
  (let loop [[candidates $collects-dir-candidates]]
    (unless (null? candidates)
      (let [[candidate (car candidates)]]
        (if (file-exists? (build-path candidate "mzlib" "list.ss"))
          (%call-prim 'SchemeSetCollectsDir candidate)
          (loop (cdr candidates))))))
    
  ;; Make sure the "Runtime" and "Scripts" directories get searched
  ;; for collections of support modules.  Note that if
  ;; SCRIPTS-DIRECTORY-NAME is not equal to "Scripts", we don't
  ;; attempt to honor that when searching for libraries, because we
  ;; don't have enough engine state set up at the top level of this
  ;; file to run %call-prim.
  (current-library-collection-paths
   (list* (build-path (current-directory) "Scripts")
          (build-path (current-directory) "Runtime")
          ;; Generates a list of paths that should be searched for
          ;; collections, including the value of (find-system-path
          ;; 'collects-dir).  find-library-collection-paths is what
          ;; MzScheme uses to initialize current-library-collection-paths.
          (find-library-collection-paths)))

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

  ;;===== Loader =====

  ;; Import a function the hard way.  We can't just (require ...) this
  ;; module because we don't set up the collection paths until its
  ;; too late to help.
  (define make-compilation-manager-load/use-compiled-handler
    (dynamic-require '(lib "cm.ss" "mzlib")
                     'make-compilation-manager-load/use-compiled-handler))
  
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

  ;;; The default namespace into which this script was loaded.  We don't
  ;;; use it to run much except this code.
  (define *original-namespace* #f)

  ;;; The namespace in which we're running the currently active script.
  ;;; This will be overwritten if we reload a script.
  (define *script-namespace* #f)

  ;;; Create a new, pristine namespace in which to run the user's
  ;;; script, and install it as the current-namespace parameter for
  ;;; this thread.
  (define (new-script-environment)
    
    ;; Store our original namespace for safe keeping.
    (unless *original-namespace*
      (set! *original-namespace* (current-namespace)))
    
    ;; Create a new, independent namespace and make it the default for all
    ;; code loaded into this thread.
    (set! *script-namespace* (make-namespace 'initial))
    ;;(set! *script-namespace* (make-namespace 'empty))
    (current-namespace *script-namespace*)
    
    ;; Alias some basic runtime support modules into our new namespace.  This
    ;; tehcnically means that these modules are shared between the original
    ;; namespace and the script namespace, which is fairly weird.
    ;;(namespace-attach-module *original-namespace* 'mzscheme)
    (namespace-attach-module *original-namespace* '#%engine-primitives)
    #f)

  ;;; Call this function to load the kernel and begin running a user
  ;;; script.  Call this immediately *after* calling
  ;;; new-script-environment.
  ;;;
  ;;; @return #f if the load succeeds, or an error string.
  (define (load-script)

    ;; Decide whether or not we should always trust (and use) compiled *.zo
    ;; files.  This will generally only be true if our code was installed
    ;; by a prepackaged installer.
    (let [[always-trust-precompiled?
           (file-exists? (build-path (current-directory) "TRUST-PRECOMPILED"))]
          [filename "none"]]

      ;; Set up an error-handling context so we can report load-time errors
      ;; meaningfully.
      (with-handlers [[void (lambda (exn)
                              (string-append "Error while loading <" 
                                             filename
                                             ">: " (exn-message exn)))]]
        (initialize-splash-screen!)
        
        ;; First, we install enough of a namespace to parse 'module'.
        ;; We'll need this in just a second when we call load/use-compiled.
        (set! filename "bootstrap-env.ss")
        (namespace-require '(lib "bootstrap-env.ss" "halyard"))

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
        ;;
        ;; TODO - I'm disabling this code, because it relies on a
        ;; custom-patched version of mzscheme, and we're currently
        ;; upgrading mzscheme.  Feel free to uncomment this code if you
        ;; need it.
        (parameterize [;; Temporary replacement version.
                       [current-load/use-compiled
                        (make-compile-zo-with-heartbeat)]
                       ;;[always-treat-zo-and-so-as-newer
                       ;; always-trust-precompiled?]
                       ;;[current-load/use-compiled
                       ;; (if always-trust-precompiled?
                       ;;     ;; Load *.zo files blindly if they exist.
                       ;;     (make-load-with-heartbeat)
                       ;;     ;; Recompile *.zo files on demand.
                       ;;     (make-compile-zo-with-heartbeat))]]
                       ]
        
          ;; Support for decent backtraces upon errors.  We pull in 
          ;; the support from errortrace-lib.ss, and then manually enable
          ;; errortrace if requested.  Note that we always require 
          ;; errortrace-lib.ss so we will have stable file counts in 
          ;; application.halyard.
          (set! filename "errortrace-lib.ss")
          (namespace-require '(lib "errortrace-lib.ss" "errortrace"))
          (when (errortrace-compile-enabled?)
            ;; Re-implement the logic from errortrace.ss.
            (current-compile (namespace-variable-value 
                              'errortrace-compile-handler))
            (use-compiled-file-paths (list (build-path "compiled" 
                                                       "errortrace")
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
          #f))))

  ) ; end module
