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
;;  Initializing Library Collection Paths
;;=========================================================================
;;  Before we can use any of load and require machinery, we need to have a
;;  sane set of collection paths set up.  We want to include the following
;;  in our collection paths:
;;    * Scripts
;;    * Runtime
;;    * The standard PLT collections
;;  The standard PLT collections may be in any of a variety of locations
;;  depending on how we're being run, whether it be on Windows or a *nix,
;;  whether we're running development or deployed code, etc.

;; Note that this file can't (require ...) anything other than
;; #%engine-primitives, because we have no collection paths set up when
;; we load it.  That's also why this file is separate from loader.ss.
(require #%engine-primitives)

;; Top-level code, not a module, because we want to execute immediately,
;; and not get caught by PLT's weird delayed initialization of modules.  We
;; wrap everything in a (let [] ...) to avoid polluting the top-level
;; namespace.
(let []

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
     ;; Under MacPorts (and possibly other Unix-style package managers),
     ;; the value of (find-system-path 'collects-dir) doesn't get set up
     ;; correctly.  But on those systems, we can generally find our
     ;; collections under 'addon-dir.  This path would, in any case, get
     ;; added to current-library-collection-paths by
     ;; find-library-collection-paths, so everything will _appear_ to work
     ;; until we invoke the compilation manager.
     ;;
     ;; But we _really_ need to set up 'collects-dir correctly, or else
     ;; setup/main-collects.ss will be unable to compute
     ;; main-collects-relative->path correctly for the compilation manager.
     (build-path (find-system-path 'addon-dir) (version) "collects")
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
          (%set-collects-path candidate)
          (loop (cdr candidates))))))
    
  ;; Make sure the "Runtime" and "Scripts" directories get searched
  ;; for collections of support modules.  Note that if
  ;; SCRIPTS-DIRECTORY-NAME is not equal to "Scripts", we don't
  ;; attempt to honor that when searching for libraries, because we
  ;; don't have enough engine state set up at the top level of this
  ;; file to run %call-prim.
  (current-library-collection-paths
   (list* (build-path (current-directory) "Scripts")
          (build-path (current-directory) "collects")
          (build-path (current-directory) "Runtime")
          ;; Generates a list of paths that should be searched for
          ;; collections, including the value of (find-system-path
          ;; 'collects-dir).  find-library-collection-paths is what
          ;; MzScheme uses to initialize current-library-collection-paths.
          (find-library-collection-paths)))
  )
