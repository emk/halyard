;;=========================================================================
;;  DrScheme IDE Tool
;;=========================================================================
;;  This used to be a dummy tool which did nothing other than satisfy
;;  DrScheme's ravenous appetite for initialization functions.  It now
;;  takes care of correctly registering our language, and loading a few
;;  tricky modules during 'on-execute'.  We need to do things that
;;  hard way because we don't like the defaults.
;;
;;  You can find similar code for the standalone engine in 5L-Loader.ss.

(module 5L-tool mzscheme
  (require (lib "unitsig.ss")
	   (lib "class.ss")
	   (lib "framework.ss" "framework")
	   (lib "tool.ss" "drscheme"))
  
  (provide tool@)

  ;; Construct a reasonable languages dialog "number" for a language.
  ;; This has a tree structure--the first number specifies how to sort our
  ;; group, and the second number specifies how to sort languages within
  ;; that group.
  (define (get-numbers language-number)
    (list 100 language-number))

  ;; Construct a reasonable name to go with GET-NUMBER.  The first component
  ;; is our group name.
  (define (get-position language-name)
    (list "Dartmouth Interactive Media Lab" language-name))

  ;; Define our editor extension.
  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)
      
      ;; Phase 1 initialization is only required for support tools such as
      ;; syntax checkers, steppers, etc.  We don't need to do it.
      (define (phase1) (void))

      ;;===================================================================
      ;;  Heavy Lifting
      ;;===================================================================
      ;;  Provide implementations of the interface
      ;;  drscheme:language:language<%>, with some slightly non-standard
      ;;  behavior.
      ;;
      ;;  This uses the distinctly funky (lib "class.ss" "mzlib") object
      ;;  system, whereas the enclosing tool uses the gratuitously
      ;;  different (lib "unitsig.ss" "mzlib") unit system.  This is
      ;;  presumably because everybody involved in a big Scheme system
      ;;  traditionally has a different opinion on what objects are, and
      ;;  how they should work.  Fie!  Fie!  And let's not mention the code
      ;;  in htdp-langs.ss, which does such evil things as construct
      ;;  complex class hiearchies at runtime.

      ;; We'll need something to subclass when creating our custom
      ;; languages.  We use this magic grik to generate a parent class.
      ;; Basically, this takes a built-in class implementing a the
      ;; interface simple-module-based-language<%>, and uses a helper
      ;; function to generate a class implementing the interface
      ;; module-based-language<%>.
      (define module-based-language%
	(drscheme:language:simple-module-based-language->module-based-language-mixin
	 drscheme:language:simple-module-based-language%))

      ;; LISPish is a pretty simple language, but we want to change
      ;; the transformer module from the default.
      (define lispish-language%
	(class module-based-language%
	  (define/override (get-transformer-module)
	    '(lib "lispish.ss" "5L"))
	  (super-instantiate ())))

      ;; The 5L language requires some tricky, pre-execution setup to match
      ;; what the engine does.  This is adapted from htdp-langs.ss.
      (define 5L-language%
	(class lispish-language%
	  (rename [super-on-execute on-execute])
	  (inherit get-module get-transformer-module)
	  (define/override (on-execute settings run-in-user-thread)
	    (run-in-user-thread
	     (lambda ()
	       ;; Now we're running in the user's namespace, and we can
	       ;; munge stuff appropriately.  First, we install enough of a
	       ;; namespace to parse 'module'.  We'll need this in just a
	       ;; second when we call load/use-compiled.
	       (namespace-require '(lib "bootstrap-env.ss" "5L"))

	       ;; We need these two modules to be loaded directly into the
	       ;; namespace, where they'll be known as #%fivel-engine and
	       ;; 5L-Kernel, respectively.  This is for compatibility
	       ;; with the standalone engine.
	       (load/use-compiled (build-path (collection-path "5L")
					      "5L-Fake-Engine.ss"))
	       (load/use-compiled (build-path (collection-path "5L")
					      "5L-Kernel.ss"))))
	    (super-on-execute settings run-in-user-thread))
	  (super-instantiate ())))

      ;; Given a class implementing drscheme:language:module-based-language<%>,
      ;; create a subclass implementing drscheme:language:language<%>.  Yes,
      ;; this is *really* weird, but I got the code from htdp-langs.ss.
      ;; This is just the way we have to do things.  Note that we must
      ;; run this DURING phase 2 initialization, not before, because
      ;; DRSCHEME:LANGUAGE:GET-DEFAULT-MIXIN won't be set up until phase 1
      ;; is complete.  See the manual.
      (define (wrap-language module-based-language%)
	((drscheme:language:get-default-mixin)
	 (drscheme:language:module-based-language->language-mixin
	  module-based-language%)))

      ;; This function takes a class implementing the interface
      ;; drscheme:language:language<%> and registers it with the IDE.
      (define (register-language language)
	(drscheme:language-configuration:add-language language))
      
      ;; Phase 2 initialization.  We need to install our language now.
      (define (phase2)

	;; Install our lispish language.
	(register-language
	 (instantiate (wrap-language lispish-language%) ()
           (module '(lib "lispish.ss" "5L"))
	   (language-numbers (get-numbers 2))
	   (language-position
	    (get-position "LISPish (MzScheme plus LISP-like extensions)"))
	   (one-line-summary
	    "MzScheme plus symbol macros, keywords and generalized setters")))

	;; Install our 5L language.
	(register-language
	 (instantiate (wrap-language 5L-language%) ()
           (module '(lib "5L.ss" "5L"))
	   (language-numbers (get-numbers 1))
	   (language-position
	    (get-position "5L Multimedia Programming Language"))
	   (one-line-summary
	    "For use with the standalone 5L application")))

	))))
