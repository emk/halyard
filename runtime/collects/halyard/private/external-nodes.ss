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

;;; This module is used to resolve paths files under the scripts/
;;; directory.
(module external-nodes (lib "mizzen.ss" "mizzen")
  (require (lib "util.ss" "halyard/private"))
  (require (lib "nodes.ss" "halyard/private"))
  (require (lib "kernel.ss" "halyard/private"))


  ;;=======================================================================
  ;;  Purely-Functional Helpers
  ;;=======================================================================
  ;;  Purely-functional support code which is easy to unit test.

  (provide split-node-name node-name->module-name)

  ;;; Given a node name such a /foo/bar, return a list of the individual
  ;;; components in the node name.  For the root node |/|, return the empty
  ;;; list.
  (define (split-node-name name)
    (if (eq? name '|/|)
      '()
      (let [[components (split (regexp "/") (symbol->string name))]]
        (if (or (< (length components) 2)
                (not (equal? "" (car components)))
                (member "" (cdr components))
                (member "." (cdr components))
                (member ".." (cdr components)))
          (error (cat "Invalid node name: " name))
          (cdr components)))))

  ;;; Convert a node name of the form /foo/bar to the corresponding (file
  ;;; ...) form.
  (define (node-name->module-name name)
    `(file ,(string-append (join "/" (split-node-name name)) ".ss")))


  ;;=======================================================================
  ;;  Other Support Code
  ;;=======================================================================

  (provide *enable-demand-loading?*)

  ;;; Do we want to turn on demand loading?  This will be honored after the
  ;;; next reload.
  (define/p *enable-demand-loading?* #f)


  ;;=======================================================================
  ;;  Node Trampolines
  ;;=======================================================================
  ;;  A "trampoline" is traditionally a tricky little piece of machine code
  ;;  that is used to redirect function calls.  Trampolines are usually
  ;;  generated at runtime.  See the Jargon File[1] for details.
  ;;
  ;;  In a linker, trampolines may be "snapped"--the first time you call a
  ;;  function, you actually call a trampoline.  The trampoline calls the
  ;;  linker, which finds the correct function to call.  The linker then
  ;;  overwrites the trampoline with a pointer to the actual function.  So
  ;;  the next time you call the function, you bypass the linker.
  ;;
  ;;  A %static-node-trampoline% is a proxy for an unloaded node.  It can
  ;;  respond to a few simply methods directly.  But as soon as you try to
  ;;  do anything complicated with it, it will actually load the real node
  ;;  in from disk and then "snap" itself out existence.
  ;;
  ;;  Just to make like exciting, a %static-node-trampoline% is actually
  ;;  emulating a _class_, not an instance.  So that affects things like
  ;;  .instance-of? and .subclass-of?.
  ;;
  ;;  [1] http://www.catb.org/jargon/html/T/trampoline.html

  (define-class %static-node-trampoline% ()

    ;;---------------------------------------------------------------------
    ;; Trampoline machinery

    (attr %snap-state 'not-ready :writable? #t)

    ;; Load the real object from disk, and replace several references to
    ;; the trampoline with references to the actual node.  Anybody else who
    ;; has a copy of the trampoline can continue to use it as a proxy.
    (def (%snap-trampoline caller args)
      (non-fatal-error (cat "Loading " (.full-name)
                            " from disk because " (.%superclass-of-real-node)
                            " trampoline can't handle ." caller
                            " with arguments " args))
      (unless (eq? (.%snap-state) 'ready)
        (fatal-error (cat "Tried to snap trampoline " (.full-name)
                          " while in state " (.%snap-state)
                          " because of ." caller " with arguments " args)))
      (set! (.%snap-state) 'snapping)
      (unregister-trampoline! self)
      (load-external-node (.full-name))
      (set! (slot '%node) (find-static-node (symbol->string (.full-name))))
      (set! (.%snap-state) 'snapped))

    ;;; Delegate methods to our underlying node, loading it if necessary.
    (def (method-missing name . args)
      ;; If we haven't loaded our real node yet, do it.
      (unless (has-slot? '%node)
        (.%snap-trampoline name args))
      ;; Pass this message to the real node.
      (apply send (slot '%node) name args))

    ;;; Allow people to tell whether we're a trampoline.
    (def (trampoline?)
      #t)

    ;; Make sure that NODE can be used to replace this trampoline, and
    ;; raise an error if it can't.
    (def (trampoline-check-replacement-node node)
      (unless (node .subclass-of? (.%superclass-of-real-node))
        (error (cat "External node " self " expected to be of type "
                    (.%superclass-of-real-node) ", but " node " is not"))))

    ;;---------------------------------------------------------------------
    ;; Emulating other classes: Typing and duck typing (remember, we're
    ;; emulating a static node, which is actually a class, so that affects
    ;; some details here)

    (attr %superclass-of-real-node)

    (def (instance-of? klass)
      (or (super)
          (.method-missing 'instance-of? klass)))

    (def (responds-to? klass)
      (or (super)
          (.method-missing 'responds-to? klass)))

    (def (subclass-of? klass)
      (or ;; Hey, we can answer this without loading anything!
          (.%superclass-of-real-node.subclass-of? klass)
          ;; OK, this is going to be harder.
          (if (klass .subclass-of? (.%superclass-of-real-node))
            ;; We might be a subclass of klass, so we need to load and
            ;; check.
            (.method-missing 'subclass-of? klass)
            ;; OK, klass isn't in the right part of the class hierarchy, so
            ;; unless somebody is being sneaky and faking multiple
            ;; inheritence (or fooling around with proxy objects--like us),
            ;; we can assume that we're not a subclass.
            #f)))

    ;;---------------------------------------------------------------------
    ;; Emulating a static node

    (attr parent)
    (attr name)
    (attr full-name)

    (def (to-string)
      (cat "#<unloaded " (.full-name) ">"))

    (def (register)
      (register-trampoline! self)
      (set! (.%snap-state) 'ready))

    )


  ;;=======================================================================
  ;;  External Cards and Groups
  ;;=======================================================================

  (provide external-group external-card)

  ;; Load an external node from the corresponding *.ss file.
  (define (load-external-node name)
    (let [[module-name (node-name->module-name name)]]
      (with-restriction-on-loadable-nodes [name module-name]
        (namespace-require module-name))))

  ;; Install a trampoline for node NAME with known SUPERCLASS.
  (define (install-trampoline name superclass)
    (with-values [[parent local-name] (analyze-node-name name #t)]
      (let [[trampoline
             (%static-node-trampoline% .new
                                       :%superclass-of-real-node superclass
                                       :parent parent :name local-name
                                       :full-name name)]]
        (trampoline .register))))

  ;; Either install a placeholder for an external node, or load it
  ;; immediately.
  (define (declare-external-node name superclass)
    (if *enable-demand-loading?*
      (install-trampoline name superclass)
      (load-external-node name)))

  ;;; Declare that a group should be loaded from an external file.
  (define-syntax external-group
    (syntax-rules ()
      [(_ name)
       (declare-external-node 'name %card-group%)]))

  ;;; Declare that a card should be loaded from an external file.
  (define-syntax external-card
    (syntax-rules ()
      [(_ name)
       (declare-external-node 'name %card%)]))


  ;;=======================================================================
  ;;  Test Harness
  ;;=======================================================================

  (provide test-demand-loader)

  ;;; A quick-and-dirty test harness for the demand-loader.
  (define (test-demand-loader node-name)
    (define (node)
      (find-static-node (symbol->string node-name)))
    (define (loaded?)
      (not ((node) .trampoline?)))

    (unless (and *enable-demand-loading?* (not (loaded?)))
      (error (cat "To run test-demand-loader, turn on the demand-loader "
                  "and make sure " node-name " isn't loaded.")))

    (assert (not (loaded?)))
    (assert (eq? ((node) .full-name) node-name))
    (assert (not (loaded?)))
    (assert (list? ((node) .members)))
    (assert (loaded?))
    (assert (memq (node) ((node) .parent.members)))
    "OK")

  )
