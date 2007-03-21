;;=========================================================================
;;  Kernel and Script Loader
;;=========================================================================
;;  This code loads our runtime kernel and our user scripts.  Both the
;;  kernel and the user scripts are loaded into an isolated namespace,
;;  which we can throw away and recreate in a pristine condition whenever
;;  a developer asks us to reload the currently running script.
;;
;;  You can find similar code for the DrScheme IDE in tool.ss.

(module 5L-Loader mzscheme

  (require #%fivel-engine)

  ;; Make sure the "Runtime" and "Script" directories get searched for
  ;; collections of support modules.
  (current-library-collection-paths
   (list (build-path (current-directory) "Runtime")
         (build-path (current-directory) "Scripts")))

  ;;===== Primitive functions =====
  
  ;; Call the specified engine primitive if it exists.
  (define (maybe-call-5l-prim name . args)    
    (when (%call-5l-prim 'HavePrimitive name)
      (apply %call-5l-prim name args)))

  ;; Notify the operating system that we're still alive, and haven't hung.
  (define (heartbeat)
    (maybe-call-5l-prim 'Heartbeat))

  ;; Load a splash screen at the standard location, assuming it exists.
  (define (maybe-load-splash path)
    (maybe-call-5l-prim 'MaybeLoadSplash path))

  ;; Force a screen update.
  (define (refresh-splash-screen)
    (maybe-call-5l-prim 'RefreshSplashScreen))

  ;; Let the engine know we've loaded a file.
  (define (notify-file-loaded)
    (maybe-call-5l-prim 'NotifyFileLoaded))

  ;; Let the engine know we've loaded all the files we plan to load.
  (define (notify-script-loaded)
    (maybe-call-5l-prim 'NotifyScriptLoaded))

  ;; Draw the progress bar for the current status of our load.
  (define (draw-load-progress)
    (maybe-call-5l-prim 'DrawLoadProgress))

  ;; Throw an error that displays a message in a dialog box and then takes
  ;; down the engine without submitting a crash report.
  (define (environment-error message)
    (%call-5l-prim 'Log '5L message 'environmenterror))

  ;; Write a line to the debug log.
  (define (debug-log msg)
    (%call-5l-prim 'Log 'Debug msg 'log))

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
  
  ;; There's some sort of context dependence that makes it so we can't just 
  ;; define COMPILE-ZO-WITH-HEARTBEAT out here, probably due to some paths 
  ;; not being set up that MAKE-COMPILATION-MANAGER-LOAD/USE-COMPILED-HANDLER
  ;; uses, so instead we just wrap this all in a function that will return our
  ;; COMPILE-ZO-WITH-HEARTBEAT function, and call it at the appropriate time.
  (define (make-compile-zo-with-heartbeat)
    ;; A suitable function to use with CURRENT-LOAD/USE-COMPILED.  This
    ;; handles automatic compilation of *.zo files for us.
    (define compile-zo (make-compilation-manager-load/use-compiled-handler))
    
    (define error-string 
      (string-append "This program must be run once from an administrative\n"
                     "account before you can use it. Please ask your\n"
                     "system administrator for assistance."))

    ;; Wrap COMPILE-ZO with two calls to HEARTBEAT, just to let the operating
    ;; system know we're still alive during really long loads.
    (define (compile-zo-with-heartbeat file-path expected-module-name)
      (with-handlers 
         [[exn:fail:filesystem? (lambda (x)
                                  (debug-log (exn-message x))
                                  (environment-error error-string))]]
        (heartbeat)
        (let [[result (compile-zo file-path expected-module-name)]]
          (heartbeat)
          (update-splash-screen!)
          result)))
    
    compile-zo-with-heartbeat)

  ;;; The default namespace into which this script was loaded.  We don't
  ;;; use it to run much except this code.
  (define *original-namespace* #f)

  ;;; The namespace in which we're running the currently active script.
  ;;; This will be overwritten if we reload a script.
  (define *script-namespace* #f)

  ;;; Create a new, pristine namespace in which to run the user's 5L
  ;;; script, and install it as the current-namespace parameter for this
  ;;; thread.
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
    (namespace-attach-module *original-namespace* '#%fivel-engine)
    #f)

  ;;; Call this function to load the 5L kernel and begin running a user
  ;;; script.  Call this immediately *after* calling
  ;;; new-script-environment.
  ;;;
  ;;; @return #f if the load succeeds, or an error string.
  (define (load-script)
    
    ;; Set up an error-handling context so we can report load-time errors
    ;; meaningfully.
    (let ((filename "none"))
      (with-handlers [[void (lambda (exn)
                              (string-append "Error while loading <" 
                                             filename
                                             ">: " (exn-message exn)))]]
        (initialize-splash-screen!)
        
        ;; First, we install enough of a namespace to parse 'module'.
        ;; We'll need this in just a second when we call load/use-compiled.
        (set! filename "bootstrap-env.ss")
        (namespace-require '(lib "bootstrap-env.ss" "5L"))

        ;; Ask MzScheme to transparently compile modules to *.zo files.
        ;; It's very important that we install this using PARAMETERIZE, and
        ;; not as a global value, because future calls to
        ;; MAKE-COMPILATION-MANAGER-LOAD/USE-COMPILED-HANDLER will wrap
        ;; whatever they find in this parameter, and we don't want to wind
        ;; up with nested compilation managers (a subtle performance
        ;; killer!).
        (parameterize [[current-load/use-compiled
                        (make-compile-zo-with-heartbeat)]]
        
          ;; Manually load the kernel into our new namespace.  We need to
          ;; call (load/use-compiled ...) instead of (require ...), because
          ;; we want the kernel registered under its official module name
          ;; (so the engine can easily grovel around inside it) but not
          ;; imported into our namespace (which is the job of the 5L
          ;; language module).
          (set! filename "kernel.ss")
          (namespace-require '(lib "kernel.ss" "5L"))
          ;;(load/use-compiled (build-path (current-directory)
          ;;                               "Runtime" "5L"
          ;;                               "kernel.ss"))
          
          ;; Provide a reasonable default language for writing scripts.  We
          ;; need to set up both the transformer environment (which is used
          ;; only by code in macro expanders) and the regular environment
          ;; (which is used by normal program code).
          (set! filename "lispish.ss")
          (namespace-transformer-require '(lib "lispish.ss" "5L"))
          (set! filename "5l.ss")
          (namespace-require '(lib "5l.ss" "5L"))
          
          ;; Load the user's actual script into our new namespace.
          (set! filename "start.ss")
          (load/use-compiled (build-path (current-directory)
                                         "Scripts" "start.ss"))
          ;; (XXX - Disabled until we can determine why the number of files
          ;; loaded goes up after a "reload script".)
          ;; XXX- Re-enabled because we currently suppress all splash-screen
          ;; code after a "reload script".
          (notify-script-loaded)
          #f))))

  ) ; end module
