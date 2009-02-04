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

;;; This module is used to resolve paths to content files, such as video,
;;; graphics, HTML, etc.
(module content-paths (lib "mizzen.ss" "mizzen")


  ;;=======================================================================
  ;; Path decomposition
  ;;=======================================================================

  (provide collection-directory collection-halyard-directory
           decompose-abstract-path)

  (define (return-directory-if-exists path)
    (if (directory-exists? path)
      path
      #f))

  ;;; If 'collection-name' belongs to a real collection, return a path to
  ;;; its on-disk directory.
  (define (collection-directory collection-name)
    (return-directory-if-exists
     (build-path (current-directory) "collects" collection-name)))

  ;;; Return a path to the _halyard directory inside a specified
  ;;; collection, or #f if it doesn't exist.
  (define (collection-halyard-directory collection-name)
    (return-directory-if-exists
     (build-path (current-directory) "collects" collection-name "_halyard")))

  ;;; Turn an abstract path (represented by a Unix-style relative path
  ;;; string) into two parts: (1) a "root path", which points to either
  ;;; (current-directory) or an appropriate _halyard directory, and (2) a
  ;;; "remaining path", which contains any path elements not used in part
  ;;; (1).
  ;;;
  ;;; For example, assuming we have a directory collects/foo/_halyard,
  ;;; but *no* directory collects/other/_halyard, then:
  ;;; 
  ;;;   foo        $HALYARD_SCRIPT/collects/foo/_halyard  .
  ;;;   foo/bar    $HALYARD_SCRIPT/collects/foo/_halyard  bar
  ;;;   other/bar  $HALYARD_SCRIPT                        other/bar
  ;;;
  ;;; Note that our return values are both PLT path objects, not strings.
  ;;; Also note that we make no attempt to deal with nested collections,
  ;;; though we might in the future.
  (define (decompose-abstract-path path)
    (let* [[match (regexp-match (regexp "^([^/]+)(/([^/].*)?)?$") path)]]
      (cond
       ;; We're not even sure what this is.
       [(not match)
        (error (cat "Malformed abstract path: " path))]
       ;; The first component of a path matches a collection with a
       ;; _halyard directory.
       [(collection-halyard-directory (second match)) =>
        (lambda (dir)
          (values dir (if (fourth match)
                        (build-path (fourth match))
                        (build-path 'same))))]
       ;; Nothing interesting is going on here.
       [else
        (values (current-directory) (build-path path))])))


  ;;=======================================================================
  ;; Locating Content Files
  ;;=======================================================================

  (provide url? exn:fail:content-not-found? content-not-found-error
           resolve-content-path)

  ;;; Return #t if path appears to be a URL string.  We only check for
  ;;; those schema we're likely to care about.
  (define (url? path)
    (and (string? path)
         (regexp-match "^(http|ftp|rtsp|file|gopher|about):" path)))

  (define-struct (exn:fail:content-not-found exn:fail)
    (content-type abstract-path))

  ;;; Raise an error specifying that we couldn't find a content file.
  (define (content-not-found-error content-type abstract-path)
    (raise (make-exn:fail:content-not-found
            (string->immutable-string
             (cat "Can't find " content-type " content: " abstract-path))
            (current-continuation-marks)
            content-type abstract-path)))

  ;;; Given an abstract path and a content type, look for a matching file
  ;;; of that type.  This function knows about decompose-abstract-path,
  ;;; and will check for both "streaming" and "local" versions of content.
  (define (resolve-content-path content-type abstract-path)
    (cond
     ;; Leave PLT paths alone, but make sure they exist.
     [(path? abstract-path)
      (if (or (file-exists? abstract-path)
              (directory-exists? abstract-path))
        abstract-path
        (content-not-found-error content-type abstract-path))]
     ;; Leave URLs alone completely.
     [(url? abstract-path)
      abstract-path]
     ;; Decompose all other paths, and look for a matching file.
     [else
      (with-values [[root remaining] (decompose-abstract-path abstract-path)]
        (let recurse [[candidate-dirs '("local" "streaming")]]
          (if (null? candidate-dirs)
            (content-not-found-error content-type abstract-path)
            (let [[candidate 
                   (build-path root (car candidate-dirs)
                               content-type remaining)]]
              (if (file-exists? candidate)
                candidate
                (recurse (cdr candidate-dirs)))))))]))

  )