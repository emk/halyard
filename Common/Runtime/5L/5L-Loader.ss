;;=========================================================================
;;  Kernel and Script Loader
;;=========================================================================
;;  This code loads our runtime kernel and our user scripts.  Both the
;;  kernel and the user scripts are loaded into an isolated namespace,
;;  which we can throw away and recreate in a pristine condition whenever
;;  a developer asks us to reload the currently running script.

(module 5L-Loader mzscheme

  (require #%fivel-engine)

  ;; Make sure the "Runtime" and "Script" directories get searched for
  ;; collections of support modules.
  (current-library-collection-paths
   (list (build-path (current-directory) "Runtime")
	 (build-path (current-directory) "Scripts")))
  
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
    (set! *script-namespace* (make-namespace 'empty))
    (current-namespace *script-namespace*)
    
    ;; Alias some basic runtime support modules into our new namespace.  This
    ;; tehcnically means that these modules are shared between the original
    ;; namespace and the script namespace, which is fairly weird.
    (namespace-attach-module *original-namespace* 'mzscheme)
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
			      (string-append "Error while loading <" filename
					     ">: " (exn-message exn)))]]

        ;; Provide a reasonable default language for writing scripts.  We
        ;; need to set up both the transformer environment (which is used
        ;; only by code in macro expanders) and the regular environment
        ;; (which is used by normal program code).
        (set! filename "lispish.ss")
	(namespace-transformer-require '(lib "lispish.ss" "5L"))
	(namespace-require '(lib "lispish.ss" "5L"))
      
	;; Manually load the kernel into our new namespace.
	(set! filename "5L-Kernel.ss")
	(load/use-compiled (build-path (current-directory)
				       "Runtime" "5L"
				       "5L-Kernel.ss"))
      
	;; Install the 5L API into our new namespace.
	(set! filename "5L-API.ss")
	(namespace-require '(lib "5L-API.ss" "5L"))

	;; Load the user's actual script into our new namespace.
	(set! filename (build-path (current-directory) "Scripts" "start.ss"))
	(load/use-compiled filename)
	#f)))

  ) ; end module
