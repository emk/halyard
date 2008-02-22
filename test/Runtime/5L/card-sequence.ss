;; An implementation of card-next, card-prev, etc.  This has known bugs
;; and design flaws, and will be redesigned soon.  See case 2411.
(module card-sequence (lib "language.ss" "5L")
  (require (lib "nodes.ss" "5L"))
  

  ;;=======================================================================
  ;;  %node%
  ;;=======================================================================

  (with-instance (%node% .class)
    ;; Corresponds to the old %jumpable% class.
    ;; TODO - Decide if we want a general .implements-interface? method.
    (def (jumpable?)
      #f)
    
    ;; Must be overriden by all jumpable nodes.
    (def (find-first-card)
      #f)
    
    ;; Must be overriden by all jumpable nodes.
    (def (find-last-card)
      #f))

  (define (find-first-card node) (node .find-first-card))
  (define (find-last-card node) (node .find-last-card))
  
  
  ;;=======================================================================
  ;;  %card-group%
  ;;=======================================================================

  (with-instance (%card-group% .class)
    (def (find-next member)
      (assert (member .jumpable?))
      #f)
    
    (def (find-prev member)
      (assert (member .jumpable?))
      #f))
  
  (define (card-group-find-next grp member) (grp .find-next member))
  (define (card-group-find-prev grp member) (grp .find-prev member))
  

  ;;=======================================================================
  ;;  %card-sequence%
  ;;=======================================================================
  ;;  This works like the standard %card-group%, but it has a notion of
  ;;  ordering among its children, and therefore supports JUMP-NEXT and
  ;;  JUMP-PREV.  There are several problems here:
  ;;
  ;;    1. This should be a flag on %card-group%, not a separate class,
  ;;       so that scripters can subclass just %card-group% and not
  ;;       %card-sequence% as well.
  ;;    2. All sequences claim to be jumpable, but empty sequences will
  ;;       fail horribly.
  ;;    3. Nesting groups inside of sequences is ill-specified.

  (provide sequence %card-sequence% card-sequence?)

  (define-class %card-sequence% (%card-group%)
    (with-instance (.class)
      (def (jumpable?) #t)
      
      (def (jump)
        (if (null? (group-members self))
          (error (cat "Can't jump to sequence " (.full-name)
                      " because it contains no cards."))
          (jump (car (group-members self)))))
      
      (def (find-next member)
        (assert (member .jumpable?))
        ;; Find the node *after* member.
        (let [[remainder (memq member (group-members self))]]
          (%assert (not (null? remainder)))
          (if (null? (cdr remainder))
            (card-group-find-next (node-parent self) self)
            ;; Walk recursively through any sequences to find the first card.
            (find-first-card (cadr remainder)))))
      
      (def (find-prev member)
        (assert (member .jumpable?))
        ;; Find the node *before* member.  Notice the two (2!) lambdas in
        ;; this function, which are used to implement a form of lazy
        ;; evaluation: They keep track of how to find the node we want,
        ;; assuming the current current node is MEMBER (which is one past the
        ;; node we want).
        (let search [[members (group-members self)]
                     [candidate-func 
                      (lambda ()
                        (card-group-find-prev (node-parent self) self))]]
          (%assert (not (null? members)))
          (if (eq? (car members) member)
            (candidate-func)
            (search (cdr members)
                    (lambda ()
                      ;; This is our actual base case: It's called when
                      ;; we've located MEMBER, and it recursively looks
                      ;; for the last card in the node *before* MEMBER.
                      ;; Got it?
                      (find-last-card (car members)))))))
      
      (def (find-first-card)
        (find-first-card (first (group-members self))))
      
      (def (find-last-card)
        (find-last-card (last (group-members self))))
      )   
    )
  
  ;; TODO - Get rid of wrapper functions.
  (define card-sequence? (make-node-type-predicate %card-sequence%))

  (define-node-definer sequence %card-sequence%)

  
  ;;=======================================================================
  ;;  %card%
  ;;=======================================================================

  (provide card-next card-prev jump-next jump-prev)
  
  (with-instance (%card% .class)
    (def (jumpable?) #t)
    
    (def (find-first-card) self)
    (def (find-last-card) self))

  (define (card-next)
    (define static ((current-card) .static-node))
    (card-group-find-next (node-parent static) static))

  (define (card-prev)
    (define static ((current-card) .static-node))
    (card-group-find-prev (node-parent static) static))

  (define (jump-helper find-card str)
    (let [[c (find-card)]]
      (if c
          (jump c)
          (error (cat "No card " str " " ((current-card) .full-name)
                      " in sequence.")))))
      
  (define (jump-next) (jump-helper card-next "after"))
  (define (jump-prev) (jump-helper card-prev "before"))
  
  )
