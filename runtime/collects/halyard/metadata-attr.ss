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

;; Metadata support for static nodes.
(module metadata-attr (lib "mizzen.ss" "mizzen")
  (require (lib "nodes.ss" "halyard/private"))

  (provide metadata-attr)

  ;;; Define a new metadata attribute on a subclass of %node%.  Metadata
  ;;; attributes are weird consequence of our static/running node system,
  ;;; and they should be used sparingly.  An example:
  ;;;
  ;;;   (define-class %group% (%card%)
  ;;;     (metadata-attr ordered? #t)
  ;;;     ...)
  ;;;
  ;;;   (group /levels (%group% :ordered? #f)
  ;;;     ...)
  ;;;
  ;;;   (/levels .ordered?) ; returns #f
  ;;;
  ;;; Here, /levels is actually a class.  When we enter the group /levels,
  ;;; we instantiate it and assign values to ordinary ATTRs.  But every
  ;;; once in a while, we need to access an attribute on the static node.
  ;;; METADATA-ATTR is a slighly hackish solution to this problem: It
  ;;; defines a read-only attribute on a static node, but allows the value
  ;;; of the attribute to be specified as though it were an ordinary ATTR.
  ;;;
  ;;; For now, METADATA-ATTR is very limited, and should be used with
  ;;; caution.  Specifically:
  ;;;
  ;;;  1) Always supply a default value.
  ;;;
  ;;;  2) Both the default value and the value passed in a static node's
  ;;;     argument list should generally be constants, not expressions.  In
  ;;;     any case, we evaluate the initializer immediately, so stay far
  ;;;     away from things like MAKE-HASH-TABLE.
  (define-syntax (metadata-attr stx)
    (syntax-case stx ()
      [(_ name default . args)
       (not (keyword? (syntax-object->datum #'default)))
       (syntax/loc stx
         (metadata-attr name :default (method () default) . args))]
      [(_ name . args)
       (syntax/loc stx
         (self .metadata-attr 'name . args))]))

  (with-instance (%node% .class)
    ;;; The names of the METADATA-ATTRs defined on this class.  Does not
    ;;; include those defined on our superclasses.
    (attr metadata-attr-names (make-hash-table))

    ;;; Does NAME correspond to a METADATA-ATTR?
    (def (metadata-attr-name? name)
      ;; TODO - Does this slow down load time too much for big scripts?
      (let recurse [[klass self]]
        (or (hash-table-get (klass .metadata-attr-names) name #f)
            (if (eq? klass %node%)
              #f
              (recurse (klass .superclass))))))

    ;;; Define a new metadata attribute.
    (def (metadata-attr name &key [default #f])
      (unless default
        (error (cat "Must supply default value for metadata-attr " name " on " 
                    self)))
      (hash-table-put! (.metadata-attr-names) name #t)
      (.attr-initializer name default #t))

    ;; Add the methods needed to implement a metadata attribute.  Metadata
    ;; attributes need to be special in two ways.
    ;;
    ;; 1) We need to be able to override metadata in subclasses and when
    ;;    defining static nodes.  Class-level attributes aren't normally
    ;;    overridable though, since they don't follow the initialization
    ;;    protocol.  Since metadata attributes are always read-only,
    ;;    instead of actually declaring them as attributes, we just define
    ;;    overridable getters, and override them on any level of the
    ;;    hierarchy that we need to.
    ;;
    ;; 2) We would like to be able to override the value of a metadata
    ;;    attribute when declaring a node using the keyword arguments on a
    ;;    static node.  Normally, this sets up attr-initializers on the
    ;;    class, which handle how instances of the class are set up, but we
    ;;    override .ATTR-INITIALIZER to instead define a getter method on
    ;;    the class (aka static node) with the appropriate value.
    ;;
    ;;    This also leads to the somewhat strange consequence that the way
    ;;    you override the value of a metadata attribute in a subclass is
    ;;    by using (default ordered? ...) in the class body itself, not a
    ;;    (with-instance (.class) ...), even though this is effectively a
    ;;    class-level attribute.
    ;;
    ;; Note that using VALUE for a metadata attribute will keep subclasses
    ;; from overriding it by sealing the getter method.  This basically
    ;; corresponds to the way VALUE works for regular attributes, to reduce
    ;; surprise.
    (def (attr-initializer name init-method ignorable? 
                           &key (skip-if-missing-values? #f))
      (if (.metadata-attr-name? name)
        (.define-metadata-attr-getter name init-method ignorable?)
        (super)))

    ;; Define a getter for our a metadata attribute.  See above for an 
    ;; explanation of why this works the way it does.
    (def (define-metadata-attr-getter name init-method overridable?)
      (define init-value (instance-exec self init-method))
      ((.class) .define-method name (method () init-value))
      (unless overridable?
        ((.class) .seal-method! name)))
      
    )
  )
