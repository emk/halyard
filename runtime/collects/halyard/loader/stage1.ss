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

;; This needs to actually be loaded before we declare our module below,
;; because we want to be able to use (require ...) from inside the loader
;; module, and we can't do that until we know where our collections are.
(require #%engine-primitives)
(load (build-path (%get-runtime-directory)
                  "collects" "halyard" "loader" "collection-paths.ss"))


;;=========================================================================
;;  Kernel and Script Loader
;;=========================================================================
;;  This code loads our runtime kernel and our user scripts.  Both the
;;  kernel and the user scripts are loaded into an isolated sandbox, which
;;  we can throw away and recreate in a pristine condition whenever a
;;  developer asks us to reload the currently running script.
;;
;;  THREAD WARNING - See lang/scheme/MZSCHEME-THREADS.txt for details.
;;
;;  Whenever we call *sandboxed-evaluator*, it actually switches us into a
;;  new thread.  This is dangerous, because it involves doing non-portable
;;  things behind the back of our C++ compiler.  Specifically, everything
;;  between our call to scheme_set_stack_base and the current stack frame
;;  is actually _moved_ temporarily to another address in memory!
;;
;;  Whenever we call from C++ to Scheme, it's vitally important that we do
;;  so from the correct thread.  Specifically, if we call a function in
;;  this file, we need to do it from our original C++ thread (the one
;;  running TInterpreterManager).  If we call a function in kernel.ss, we
;;  must do so from *sandboxed-evaluator*'s thread.
;;
;;  The two important transfers of control happen in load-script and
;;  run-script, below.  When these functions call *sandboxed-evaluator*,
;;  they switch into *sandboxed-evaluator*'s thread, run some code, and
;;  switch back into the original thread once that code is finished.  Once
;;  in *sandboxed-evaluator*'s thread, Scheme will call back out into C++,
;;  which will in turn call back into Scheme, all from the correct thread.

(module stage1 mzscheme
  (require (lib "sandbox.ss" "mzlib"))

  ;; We load the compilation manager _outside_ the sandbox, and pass it
  ;; through to stage2, inside the sandbox.  This is necessary, because the
  ;; compilation manager can't be loaded in the same namespace that we want
  ;; to run it in.
  ;;
  ;; Q: Why must we load the compilation manager in a separate namespace?
  ;;
  ;; A: Loading the compilation manager also loads quite a few modules from
  ;; mzlib.  But many of these modules will also be loaded again later, by
  ;; the script we're compiling.  This means that we would have compiled
  ;; modules depending on non-compiled modules.  And this is specicially
  ;; forbidden in the compilation manager's documentation.  By using two
  ;; separate namespaces, we hide the non-compiled mzlib modules from the
  ;; program we're trying to compile.
  (require (lib "cm.ss" "mzlib"))

  ;;===== Sandbox options =====

  ;; Specify how to initialize the sandbox's namespace.  The first item in
  ;; this list is our namespace creation function, and the following items
  ;; are a list of modules to copy from our global namespace into our
  ;; sandbox.
  (sandbox-namespace-specs
   `(;; Our namespace creation function.
     ,make-namespace
     ;; We can't just pass #%engine-primitives directly to our child
     ;; module, because sandbox.ss will pass these paths to
     ;; dynamic-require, which wants 'lib' or 'file' forms.  But once the
     ;; helper is pulled into our sandbox, #%engine-primitives will come
     ;; with it.
     (lib "engine-primitives-helper.ss" "halyard" "loader")))

  ;; Allow full access to the file system ("execute" includes "write",
  ;; "read", etc.).  We may tighten this up later.
  (sandbox-path-permissions
   `((execute ,(byte-regexp #".*"))))

  ;; Place no limits on running time or memory used.
  (sandbox-eval-limits '(#f #f))

  ;; Allow the sandbox to print output to our regular output port.  This
  ;; generally is only visible from CommonTest, but that's better than
  ;; throwing it away.
  (sandbox-output (current-output-port))

  ;; Allocate a reasonably large thread stack, because we're running a lot
  ;; of C++ code on the sandbox's thread stack, and C++ doesn't know how to
  ;; increase the stack size at runtime.  I've observed up to 4000 in use,
  ;; so this is a delibrately over-large number.
  (current-thread-initial-stack-size 64000)

  ;;===== Memory leak workaround =====

  ;; Save the display and write handlers for our standard output ports.
  ;; See below for why we need to do this.
  (define $original-output-port-display-handler
    (port-display-handler (current-output-port)))
  (define $original-error-port-display-handler
    (port-display-handler (current-error-port)))
  (define $original-output-port-write-handler
    (port-write-handler (current-output-port)))
  (define $original-error-port-write-handler
    (port-write-handler (current-error-port)))

  ;;; Reset our display and write handlers to their original values.  These
  ;;; handlers get changed by Swindle, and if we don't reset them properly
  ;;; between script loads, the Swindle handlers will cause us to leak
  ;;; memory.  This memory leak was tracked down by Matthew Flatt.
  (define (reset-port-handlers!)
    (port-display-handler (current-output-port)
                          $original-output-port-display-handler)
    (port-display-handler (current-error-port)
                          $original-error-port-display-handler)
    (port-write-handler   (current-output-port)
                          $original-output-port-write-handler)
    (port-write-handler   (current-error-port)
                          $original-error-port-write-handler))

  ;;===== Loader =====

  (define *sandboxed-evaluator* #f)

  ;;; Create a new sandbox in which we can run the user's script.
  (define (new-script-environment)
    ;; Shut down our previous sandbox, if any.
    (when *sandboxed-evaluator*
      ;; Reset our port handlers, removing the ones installed by
      ;; swindle/extra.ss.  This prevents a memory leak in v372.
      (reset-port-handlers!)
      ;; Shut down the evaluator and try to GC the memory it was using.
      (kill-evaluator *sandboxed-evaluator*)
      (set! *sandboxed-evaluator* #f)
      (collect-garbage))
      
    ;; Make a new evaluator.
    (set! *sandboxed-evaluator* (make-evaluator '(begin) '()))

    ;; Return the namespace for our evaluator.
    (*sandboxed-evaluator* '(current-namespace)))

  ;;; Call this function to load the kernel and begin running a user
  ;;; script.  Call this immediately *after* calling
  ;;; new-script-environment.
  ;;;
  ;;; See the top-of-file comment for a discussion of *sandboxed-evaluator*
  ;;; and how we deal with its threads.
  ;;;
  ;;; @return #f if the load succeeds, or an error string.
  (define (load-script)
    (with-handlers [[void (lambda (exn) (exn-message exn))]]
      (*sandboxed-evaluator*
       `(begin
          (require (lib "stage2.ss" "halyard" "loader"))
          ;; Pass through the compilation manager APIs we'll need--see the
          ;; comment at the top of this file.
          (%stage2 ,make-compilation-manager-load/use-compiled-handler
                   ,manager-compile-notify-handler
                   ,manager-trace-handler)))
      #f))

  ;;; Transfer control into *sandboxed-evaluator*.  
  ;;;
  ;;; See the top-of-file comment for a discussion of *sandboxed-evaluator*
  ;;; and how we deal with its threads.
  (define (run-script)
    (*sandboxed-evaluator* '(%main-kernel-loop)))

  ) ; end module
