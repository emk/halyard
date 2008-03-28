;; An implementation of card-next, card-prev, etc.  This has known bugs
;; and design flaws, and will be redesigned soon.  See case 2411.
(module card-sequence (lib "language.ss" "mizzen")
  (require (lib "nodes.ss" "halyard"))
  (require (lib "util.ss" "mizzen"))

  
  ;;=======================================================================
  ;;  Helper Functions
  ;;=======================================================================
  
  ;; Given an item V and a list LST, return a values list consisting of the
  ;; previous item to V, the item V, and the next item after V.
  ;; If V is not in LST, return all values as #f.
  ;; If there is not a previous or next item, return #f for those values.
  (define (find-with-neighbors v lst)
    (let recurse [[v v] [prev #f] [lst lst]]
      (cond
       [(null? lst)
        (values #f #f #f)]
       [(eq? (car lst) v)
        (values prev (car lst)
                (if (null? (cdr lst))
                    #f
                    (cadr lst)))]
       [else
        (recurse v (car lst) (cdr lst))])))
  
  
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

  (with-instance (%group-member% .class)
    (def (flatten-group-member)
      (error "flatten-group-member must be overridden."))
    
    (def (largest-containing-ordered-group)
      (error "largest-containing-ordered-group must be overridden.")))
  
  (with-instance (%card-group% .class)    
    ;; By default, card groups ignore themselves. Ordered groups return their
    ;; child nodes; Unordered groups do not participate and return the empty
    ;; list.
    (def (flatten-group-member)
      (if (.ordered?)
        (.flattened-group-members)
        '()))
    
    ;; Return a flattened list of all child group members.
    ;; (Child group members are included only if they return themselves or
    ;;  their children in .flatten-group-member)
    (def (flattened-group-members)
      (define (flatten-group-member node)
        (node .flatten-group-member))
      (apply append (map flatten-group-member (.members))))
    
    ;; Return the largest containing ordered group, starting with self.
    ;; If there is a direct parent ordered group, or #f if there is no
    ;; direct containing group that is ordered.
    (def (largest-containing-ordered-group)
      (if (not (.ordered?))
        #f
        (or ((.parent) .largest-containing-ordered-group)
            self)))
    
    (def (jumpable?)
      (.ordered?))
    
    (def (jump)
      (assert (.jumpable?))
      (if (null? (.members))
        (error (cat "Can't jump to group " (.full-name)
                    " because it contains no cards."))
        (jump (car (.members)))))
    
    ;; Given a group-member child, searches for the member's neighbors
    ;; if this group has a largest-containing-ordered-group, otherwise
    ;; there aren't adjacent neighbors but we have succeeded, so we should
    ;; return (values #f #t #f).
    (def (%find-adjacent-if-available member)
      (assert (member .jumpable?))
      (define lcog (.largest-containing-ordered-group))
      (if lcog
        (find-with-neighbors member (lcog .flattened-group-members))
        (values #f #t #f)))
    
    ;; Given a group-member child and a direction ('next or 'prev),
    ;; return the static card for that direction (or #f, if there is no card).
    (def (find-adjacent member direction)
      (with-values 
        [[prev-node-or-false succeeded? next-node-or-false]
         (.%find-adjacent-if-available member)]
        (cond
         [(not succeeded?)
          (error (cat "find-adjacent: " member " is not a member of " self))]
         [(eq? 'next direction)
          next-node-or-false]
         [(eq? 'prev direction)
          prev-node-or-false]
         [else
          (error (cat "find-adjacent: invalid direction " direction
                      " (member = " member ", card-group = " self ")."))])))
    
    (def (find-next member)
      (.find-adjacent member 'next))
    
    (def (find-prev member)
      (.find-adjacent member 'prev))
    
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
    
    ;; By default, cards are included.
    (def (flatten-group-member)
      (list self))
    
    ;; Return the largest containing ordered group, or #f if there is no
    ;; direct containing group that is ordered.
    (def (largest-containing-ordered-group)
      ((.parent) .largest-containing-ordered-group))
    
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
