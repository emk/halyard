(module ruby-nodes (lib "language.ss" "5L")
  (require (lib "util.ss" "5L"))
  (require (lib "tamale-unit.ss" "5L"))

  (require-for-syntax (only (lib "util.ss" "5L") with-values))

  ;; TODO in ruby-objects.ss:
  ;; / .send
  ;; / ruby-object?
  ;;   Unbreak the special form for "()" (the empty list)
  ;;   Instance classes
  ;;   Attrs which are write-only after initialization
  ;;   .DEFINE-METHOD should fail if method exists


  ;;=======================================================================
  ;;  The Node Class
  ;;=======================================================================
  ;;  The methods in this class are used by many different sections
  ;;  throughout this file.

  ;; <node>
  (define-class %node% ()
    (with-instance (.class)
      ;; TODO - Find a good name.
      ;; Returns true if this is a named node class 
      (def (in-hierarchy?)
        (slot 'in-hierarchy?))

      ;;; If this class represents a node in our program, the name
      ;;; of that node.
      (def (name)
        (assert (.in-hierarchy?))
        (slot 'name))

      ;;; If this class represents a node in our program, the parent
      ;;; of that node.
      (def (parent)
        (assert (.in-hierarchy?))
        (slot 'parent))

      ;;; Get the path from the root to this node class.
      (def (path)
        (define (node-and-parents node)
          (if (node .parent)
            (cons node (node-and-parents (node .parent)))
            '()))
        (%node-path% .new
          :components (reverse (map (lambda (node) (node .name))
                                    (node-and-parents self)))))

      ;;; Get the root node associated with this node class.
      (def (root)
        (define parent (slot 'parent))
        (if parent (parent .root) self))

      ;; Internal: Initialize this node class to represent the root node.
      (def (act-as-root-node-class)
        (set! (slot 'in-hierarchy?) #t)
        (set! (slot 'parent) #f)
        (set! (slot 'name) '||))

      ;; Internal: Attach this node class to its parent node class and give
      ;; it a name.  This is used to set up the static, uninstantiated node
      ;; hierarchy.
      (def (act-as-child-node-class parent name)
        (set! (slot 'in-hierarchy?) #t)
        (set! (slot 'parent) parent)
        (set! (slot 'name) name)
        (parent .append-member self))
      )

    (attr name)  ; :writable? #f)

    (def (propagate name . args)
      ;; TODO - Pass method up containment hierarchy.
      )

    (def (define-method name meth)
      ;; TODO - Install method into instance class.
      )
    )


  ;;=======================================================================
  ;;  Basic Node Support
  ;;=======================================================================

  ;; Helper function.
  (define (instance-or-subclass-of? klass obj)
    (and (ruby-object? obj)
         (or (obj .instance-of? klass)
             (obj .subclass-of? klass))))

  ;; create DEPRECATE
  (define (create klass . args)
    (klass .send 'new args))

  ;; node? DEPRECATE
  (define (node? obj)
    (instance-or-subclass-of? %node% obj))

  ;; node-name DEPRECATE
  (define (node-name node)
    (node .name))
  
  ;; extends-template? DEPRECATE (Semantically, this also used to work for
  ;; "phantom" card nodes.  Now, you need to use .subclass-of? for that.)
  (define (extends-template? node template)
    (node .instance-of? template))

  ;; on RENAME? DEPRECATE?
  (define-syntax on
    (syntax-rules ()
      [(on name args . body)
       (def (name . args) . body)]))

  ;; send* REPLACE with .send or .propagate on a case-by-case basis.

  ;; send DEPRECATE (Note that this is no longer recursive by default,
  ;; unless NAME has been declared to be an event.)
  (define-syntax send 
    (syntax-rules ()
      [(send node name . args)
       (node .send 'name (list . args))]))

  #| PROP is temporarily RUBY-PROP
  ;; prop* DEPRECATE
  (define (prop* node name)
    (node .send name '()))

  ;; set-prop*! DEPRECATE
  (define (set-prop*! node name value)
    (node .send (symcat "set-" name "!") (list value)))

  ;; prop DEPRECATE
  (define-syntax prop
    (syntax-rules ()
      [(prop node name)
       (prop* node 'name)]))
  |#


  ;;=======================================================================
  ;;  Basic Node Support Tests
  ;;=======================================================================

  (provide <node-test> <node-deprecated-test>)

  (define-class %titled-node% (%node%)
    (attr title "My Foo" :writable? #t))

  (define-test-case <node-test> () []
    (test "Nodes should have names"
      (define foo (%node% .new :name "foo"))
      (assert-equals "foo" (foo .name)))
    )

  (define-test-case <node-deprecated-test> () []
    (test "CREATE should make an instance of a class"
      (define foo (create %node% :name "foo"))
      (assert (node? foo)))
    (test "Nodes should have names"
      (define foo (create %node% :name "foo"))
      (assert-equals "foo" (node-name foo)))
    (test "EXTENDS-TEMPLATE? should return true for instances of a class"
      (define foo (create %node% :name "foo"))
      (assert (extends-template? foo %node%)))
    ;; TODO - Test ON once we have instance classes
    (test "SEND should send a message to a node"
      (define foo (create %node% :name "foo"))
      (assert-equals "foo" (send foo name)))
    #| PROP is temporarily RUBY-PROP
    (test "PROP should send a message to a node"
      (define foo (create %node% :name "foo"))
      (assert-equal "foo" (prop foo name)))
    (test "SET! should work on PROP"
      (define titled (create %titled-node% :name "titled"))
      (set! (prop titled title) "Your Foo")
      (assert-equal "Your Foo" (prop titled title)))
    |#
    )


  ;;=======================================================================
  ;;  Node Paths
  ;;=======================================================================
  ;;  TODO
  ;;    Act as proxy object for active nodes
  ;;    Merge with version that was copied into Runtime, once ready

  ;; Get some string processing stuff from the SRFI libraries.
  (require (only (lib "13.ss" "srfi") string-tokenize string-join))
  (require-for-syntax (only (lib "13.ss" "srfi") string-tokenize string-join))
  (require (only (lib "14.ss" "srfi") char-set char-set-complement))
  (require-for-syntax (only (lib "14.ss" "srfi") char-set char-set-complement))

  ;;; A path in our node hierarchy.  Can represent either nested node
  ;;; classes, or nested nodes themselves.
  (define-class %node-path% ()
    (attr components)

    ;;; Print a path in a readable format, but without the leading @ sign.
    ;;; The opposite of @*.
    (def (to-path-string)
      (components->path-string (.components)))

    ;;; Print a path in a readable format.
    (def (to-string)
      (string-append "@" (.to-path-string)))

    ;;; Build a symbol representing this path.  For legacy use.
    (def (to-symbol)
      (string->symbol (.to-path-string)))

    ;;; Resolve this path relative to BASE.
    (def (resolve base)
      (define (find-node-internal base components)
        (cond
         [(empty? components) base]
         [(eq? '|.| (car components))
          (find-node-internal base (cdr components))]
         [(eq? '|..| (car components))
          (find-node-internal (base .parent) (cdr components))]
         [else (find-node-internal (base .child (car components))
                                   (cdr components))]))
      (define components (slot 'components))
      (if (and (not (null? components))
               (eq? '|.| (car components)))
          (find-node-internal base (cdr components))
          (find-node-internal (base .root) components)))
    )

  ;;; Run code at both syntax expansion time and runtime.
  (define-syntax begin-for-syntax-and-runtime
    (syntax-rules ()
      [(_ code ...)
       (begin
         (begin-for-syntax code ...)
         code ...)]))

  (begin-for-syntax-and-runtime
    ;;; Break a path string appart into a list of components.
    (define (path-string->components path-string)
      (let* [[absolute? (and (> (string-length path-string) 0)
                             (eq? #\/ (string-ref path-string 0)))]
             [components
              (string-tokenize path-string
                               (char-set-complement (char-set #\/)))]]
        (map string->symbol
             (if absolute? components (cons "." components)))))

    ;;; Combine components back into a path string.
    (define (components->path-string components)
      (define (join lst)
        (string-join (map symbol->string lst) "/"))
      (if (and (not (null? components))
               (eq? '|.| (car components)))
          (join (cdr components))
          (string-append "/" (join components))))
    )

  ;; @* CHANGED (Now takes a string as an argument.  This used to return a
  ;; node; now it just returns a path in the node tree.  It now also
  ;; distinguishes between relative and absolute paths, and no longer
  ;; searches up the hierarchy.)
  (define (@* path-string)
    (%node-path% .new :components (path-string->components path-string)))

  ;; @ DELETED (No longer necessary, since @* takes a string.  Will need to
  ;; update "@" read syntax, though.)


  ;;=======================================================================
  ;;  Node Path Tests
  ;;=======================================================================

  (provide <node-path-test>)

  (define-test-case <node-path-test> () []
    (test "@* should construct absolute and relative paths"
      (assert-equals (list 'a 'b) ((@* "/a/b") .components))
      (assert-equals (list '|.| 'a 'b) ((@* "a/b") .components)))
    (test "Paths should display using their (non-portable) read syntax"
      (assert-equals "@/a/b" (cat (@* "/a/b")))
      (assert-equals "@a/b" (cat (@* "a/b")))))


  ;;=======================================================================
  ;;  Root Node Class
  ;;=======================================================================
  ;;  The root node class in our hierarchy.

  ;; The root node class in our main hierarchy.
  (define *root-node-class* #f)

  ;; Temporarily change our root node class, and put it back when we're
  ;; done.  Used for testing.
  (define-syntax with-root-node-class
    (syntax-rules ()
      [(_ node-class body ...)
       (let [[previous #f]]
         (dynamic-wind
           (lambda ()
             (set! previous *root-node-class*)
             (set! *root-node-class* node-class))
           (lambda () body ...)
           (lambda ()
             (set! *root-node-class* previous))))]))

  ;; The same as WITH-ROOT-NODE-CLASS, but it works at the top-level and
  ;; handles errors in a less-graceful fashion.
  (define-syntax with-root-node-class/top-level
    (syntax-rules ()
      [(_ node-class body ...)
       (begin
         (define previous #f)
         (set! previous *root-node-class*)
         (set! *root-node-class* node-class)
         body ...
         (set! *root-node-class* previous))]))


  ;;=======================================================================
  ;;  Groups
  ;;=======================================================================

  ;; <card-group>
  (define-class %group% (%node%)
    (with-instance (.class)
      ;;; The children of this group.
      (attr members '() :writable? #t)

      ;; Internal: Add a member to this group.
      (def (append-member node)
        ;; O(N^2) WARNING
        (assert (.in-hierarchy?))
        (set! (.members) (append (.members) (list node))))

      ;;; Find the child with the specified name.
      (def (child name)
        ;; O(N^2) WARNING
        (let recurse [[children (.members)]]
          (cond
           [(null? children)
            (error (cat self " has no child " name))]
           [(eq? name ((car children) .name))
            (car children)]
           [else
            (recurse (cdr children))])))
      ))

  ;; group-members DEPRECATE
  (define (group-members group)
    (group .members))

  ;; card-group? DEPRECATE
  (define (card-group? obj)
    (instance-or-subclass-of? %group% obj))

  ;; node-full-name DEPRECATE
  (define (node-full-name node)
    ((node .path) .to-symbol))

  ;; node-parent DEPRECATE
  (define (node-parent node)
    (node .parent))

  ;; This was present before, but not exported.
  (define (find-node-relative base path)
    (path .resolve base))

  ;; find-node DEPRECATE
  ;; TODO - Implement as soon as we have a global, default root node.

  ;; define-group-template REMOVE

  ;; group DEPRECATE
  (define-syntax (group stx)
    ;; Given a path of the '/foo' or '/foo/bar', return the node's parent
    ;; and the node's name.
    (define (analyze-node-path name-stx)
      (define path-string (symbol->string (syntax-object->datum name-stx)))
      (define rev-components (reverse (path-string->components path-string)))
      (define parent-components (reverse (cdr rev-components)))
      (values (datum->syntax-object name-stx
                (if (null? parent-components)
                    '*root-node-class*
                    (string->symbol (components->path-string
                                     parent-components))))
              (datum->syntax-object name-stx (car rev-components))))
    (syntax-case stx ()
      [(_ path (super))
       (with-values [[parent name] (analyze-node-path #'path)]
         (quasisyntax/loc stx
           (define-class path (super)
             (.act-as-child-node-class #,parent '#,name)
             )))]))


  ;;=======================================================================
  ;;  Group Tests
  ;;=======================================================================

  (provide <node-group-test> <node-group-deprecated-test>)

  (define-class $fake-root (%group%)
    (.act-as-root-node-class))
  (define-class /foo (%group%)
    (.act-as-child-node-class $fake-root 'foo))
  (define-class /foo/bar (%group%)
    (.act-as-child-node-class /foo 'bar))


  (with-root-node-class/top-level $fake-root
    (group /foo/baz (%group%))
    (group /quux (%group%))
    )

  (define-test-case <node-group-test> () []
    (test "Root node should have an empty name and no parent"
      (assert-equals '|| ($fake-root .name))
      (assert-equals #f ($fake-root .parent)))
    (test "Child nodes should have a name and parent"
      (assert-equals 'foo (/foo .name))
      (assert-equals $fake-root (/foo .parent))
      (assert-equals 'bar (/foo/bar .name))
      (assert-equals /foo (/foo/bar .parent)))
    (test "Parent nodes should have children"
      (assert (memq /foo ($fake-root .members)))
      (assert (memq /foo/bar (/foo .members))))
    (test "Nodes should have paths"
      (assert-equals "@/" (cat ($fake-root .path)))
      (assert-equals "@/foo" (cat (/foo .path)))
      (assert-equals "@/foo/bar" (cat (/foo/bar .path))))
    (test "Absolute and relative paths should be resolvable"
      (assert-equals /foo ((@* "/foo") .resolve $fake-root))
      (assert-equals /foo/bar ((@* "/foo/bar") .resolve $fake-root))
      (assert-equals $fake-root ((@* "/") .resolve /foo))
      (assert-equals /foo ((@* "/foo") .resolve /foo))
      (assert-equals /foo/bar ((@* "bar") .resolve /foo)))
    (test ". and .. should have the expected meanings in paths"
      (assert-equals /foo ((@* ".") .resolve /foo))
      (assert-equals $fake-root ((@* "..") .resolve /foo))
      ;;((@* "..") .resolve $fake-root) ; No behavior defined yet.
      )
    (test "GROUP should allow the definition of a group"
      (assert-equals 'baz (/foo/baz .name))
      (assert-equals /foo (/foo/baz .parent)))
    (test "Top-level GROUPs should be attached to the root node"
      (assert-equals 'quux (/quux .name))
      (assert-equals $fake-root (/quux .parent)))
    )

  (define-test-case <node-group-deprecated-test> () []
    (test "CARD-GROUP? should return true for classes and instances"
      (assert (card-group? /foo))
      (assert (card-group? (%group% .new :name "foo"))))
    (test "Root node should have an empty name and no parent"
      (assert-equals '|| (node-name $fake-root))
      (assert-equals #f (node-parent $fake-root)))
    (test "Child nodes should have a name and parent"
      (assert-equals 'foo (node-name /foo))
      (assert-equals $fake-root (node-parent /foo))
      (assert-equals 'bar (node-name /foo/bar))
      (assert-equals /foo (node-parent /foo/bar)))
    (test "Parent nodes should have members"
      (assert (memq /foo (group-members $fake-root)))
      (assert (memq /foo/bar (group-members /foo))))
    (test "Nodes should have paths"
      (assert-equals '|/| (node-full-name $fake-root))
      (assert-equals '/foo (node-full-name /foo))
      (assert-equals '/foo/bar (node-full-name /foo/bar)))
    (test "Absolute and relative paths should be resolvable"
      (assert-equals /foo (find-node-relative $fake-root (@* "/foo")))
      (assert-equals /foo/bar (find-node-relative $fake-root (@* "/foo/bar")))
      (assert-equals $fake-root (find-node-relative /foo (@* "/")))
      (assert-equals /foo (find-node-relative /foo (@* "/foo")))
      (assert-equals /foo/bar (find-node-relative /foo (@* "bar"))))
    )


  ;;=======================================================================
  ;;  Everything Else
  ;;=======================================================================



  ;; run-card
  ;; jump
  ;; delete-element-internal
  ;; dispatch-event-to-current-group-member
  ;; current-group-member
  ;; current-card
  ;; *running-on-exit-handler-for-node*
  ;; <jumpable>
  ;; jumpable?

  ;; sequence
  ;; <card-sequence>
  ;; card-sequence?

  ;; <card>
  ;; card?
  ;; card-next
  ;; card-prev
  ;; jump-next
  ;; jump-prev
  ;; define-card-template
  ;; card


  ;;=======================================================================
  ;;  Elements
  ;;=======================================================================
  ;;  We'll bolt these on once everything else works

  ;; node-elements

  ;; <element>
  ;; element?
  ;; define-element-template
  ;; element
  ;; default-element-parent
  ;; call-with-default-element-parent
  ;; with-default-element-parent


  

  )