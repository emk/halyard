;; An implementation of card-next, card-prev, etc.  This has known bugs
;; and design flaws, and will be redesigned soon.  See case 2411.
(module card-sequence (lib "language.ss" "halyard")
  (require (lib "nodes.ss" "halyard"))
  

  ;;=======================================================================
  ;;  %node%
  ;;=======================================================================

  (with-instance (%node% .class)
    ;; Corresponds to the old %jumpable% class.
    ;; TODO - Decide if we want a general .implements-interface? method.
    (def (jumpable?)
      (error (cat "%node%:jumpable? - please override in child class.")))
    
    ;; Must be overriden by all jumpable nodes.
    (def (find-first-card)
      #f)
    
    ;; Must be overriden by all jumpable nodes.
    (def (find-last-card)
      #f))
  
  
  ;;=======================================================================
  ;;  %card-group%
  ;;=======================================================================
  
  (with-instance (%card-group% .class)
    ;; ORDERED? is a property of a static node, thus it needs to be 
    ;; implemented as a class-level attribute.
    ;; 
    ;; The ORDERED? attribute needs to be special in two ways.
    ;;
    ;; 1) We need to be able to override ORDERED? in subclasses and
    ;;    when defining the node using GROUP.  Class-level attributes
    ;;    aren't normally overridable though, since they don't follow
    ;;    the initialization protocol.  Since it also doesn't need to
    ;;    ever be written, instead of actually declaring it as an
    ;;    attribute, we just define an overridable getter, and
    ;;    override it on any level of the hierarchy that we need to.
    ;;
    ;; 2) We would like to be able to override the value when
    ;;    declaring a node using the keyword arguments on GROUP.
    ;;    Normally, this sets up attr-initializers on the class, which
    ;;    handle how instances of the class are set up, but we
    ;;    override .ATTR-INITIALIZER to instead define a getter method
    ;;    on the class (aka static node) with the appropriate value.
    ;;
    ;;    This also leads to the somewhat strange consequence that the
    ;;    way you override the value of ORDERED? in a subclass is by
    ;;    using (default ordered? ...) in the class body itself, not
    ;;    a (with-instance (.class) ...), even though this is effectively
    ;;    a class-level attribute.
    ;;
    ;; Note that using VALUE for ORDERED? will keep subclasses from 
    ;; overriding it by sealing the getter method.  This basically 
    ;; corresponds to the way VALUE works for regular attributes, 
    ;; to reduce surprise.
    (def (attr-initializer name init-method ignorable? 
                           &key (skip-if-missing-values? #f))
      (if (eq? name 'ordered?)
        (.define-ordered?-getter init-method ignorable?)  
        (super)))

    ;; Define a getter for our ORDERED? attribute.  See above for an 
    ;; explanation of why this works the way it does.
    (def (define-ordered?-getter init-method overridable?)
      ((.class) .define-method 'ordered? init-method)
      (unless overridable?
        ((.class) .seal-method! 'ordered?))))
  
  (with-instance %card-group%
    ;; Define the base case getter for ORDERED?. See above for an 
    ;; explanation of how this is basically equivalent to defining a 
    ;; class-level (or static node) attribute.
    (default ordered? #t)
    
    ;; Proxy from the running node to the static node.
    (def (ordered?)
      ((.class) .ordered?)))
  
  (with-instance (%card-group% .class)
    (def (jumpable?)
      (.ordered?))
    
    (def (jump)
      (assert (.jumpable?))
      (if (null? (.members))
        (error (cat "Can't jump to group " (.full-name)
                    " because it contains no cards."))
        (jump (car (.members)))))

    (def (find-next member)
      (assert (member .jumpable?))
      (if (not (.ordered?))
        #f
        ;; Find the node *after* member.
        (let [[remainder (memq member (.members))]]
          (%assert (not (null? remainder)))
          (if (null? (cdr remainder))
            ((.parent) .find-next self)
            ;; Walk recursively through any sequences to find the first card.
            ((cadr remainder) .find-first-card)))))
    
    (def (find-prev member)
      (assert (member .jumpable?))
      (if (not (.ordered?))
        #f
        ;; Find the node *before* member.  Notice the two (2!) lambdas in
        ;; this function, which are used to implement a form of lazy
        ;; evaluation: They keep track of how to find the node we want,
        ;; assuming the current current node is MEMBER (which is one past the
        ;; node we want).
        (let search [[members (.members)]
                     [candidate-func 
                      (lambda ()
                        ((.parent) .find-prev self))]]
          (%assert (not (null? members)))
          (if (eq? (car members) member)
            (candidate-func)
            (search (cdr members)
                    (lambda ()
                      ;; This is our actual base case: It's called when
                      ;; we've located MEMBER, and it recursively looks
                      ;; for the last card in the node *before* MEMBER.
                      ;; Got it?
                      ((car members) .find-last-card)))))))
    
    (def (find-first-card)
      (assert (.ordered?))
      ((first (.members)) .find-first-card))
    
    (def (find-last-card)
      (assert (.ordered?))
      ((last (.members)) .find-last-card))
    )
  
  
  ;;=======================================================================
  ;;  %root-node%
  ;;=======================================================================
  
  (with-instance %root-node%
    (value ordered? #f))
  
  
  ;;=======================================================================
  ;;  %card%
  ;;=======================================================================
  
  (provide card-next card-prev jump-next jump-prev)
  
  (with-instance (%card% .class)
    (def (jumpable?) #t)
    
    (def (find-first-card) self)
    (def (find-last-card) self)
    
    (def (card-next)
      ((.parent) .find-next self))
    
    (def (card-prev)
      ((.parent) .find-prev self))
    )

  (with-instance %card%
    (def (card-next)
      ((.class) .card-next))

    (def (card-prev)
      ((.class) .card-prev))
    )

  (define (card-next)
    ((current-card) .card-next))
  
  (define (card-prev)
    ((current-card) .card-prev))
  
  (define (jump-helper find-card str)
    (let [[c (find-card)]]
      (if c
          (jump c)
          (error (cat "No card " str " " ((current-card) .full-name)
                      " in sequence.")))))
      
  (define (jump-next) (jump-helper card-next "after"))
  (define (jump-prev) (jump-helper card-prev "before"))
  
  )
