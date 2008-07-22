;; PORTED
(module experiments (lib "halyard.ss" "halyard")
  (require (lib "base.ss" "halyard-test"))
  (require (lib "halyard-unit.ss" "halyard"))
  
  (group /experiments)

  ;;=======================================================================
  ;;  Ordered and Unordered Groups
  ;;=======================================================================
  ;;
  ;; Some ordered and unordered groups for testing purposes.
  ;;

  ;; For testing the card-sequence.ss code.
  (define (largest-containing-ordered-group 
           &key (static-node ((current-card) .class)))
    (static-node .largest-containing-ordered-group))
  
  (group /experiments/groups)

  (define-class %my-group% (%card-group%)
    (value ordered? #f))
  
  (card /experiments/groups/top)

  (group /experiments/groups/unordered-group (%my-group%)
    ;;(%card-group% :ordered? #f))
    (assert-equals #f (largest-containing-ordered-group :static-node self))
    )

  (card /experiments/groups/unordered-group/foo ()
    )

  (card /experiments/groups/unordered-group/bar ()
    (run
      (assert-equals #f (largest-containing-ordered-group)))
    )

  (group /experiments/groups/unordered-group/ordered-group ()
    (assert-equals self
                   (largest-containing-ordered-group :static-node self))
    )

  (card /experiments/groups/unordered-group/ordered-group/echo ()
    (run
      (assert-equals ((.parent) .class) (largest-containing-ordered-group)))
    )

  (card /experiments/groups/unordered-group/ordered-group/foxtrot ()
    )

  (card /experiments/groups/unordered-group/ordered-group/golf ()
    )

  (group /experiments/groups/ordered-group)

  (card /experiments/groups/ordered-group/hotel ()
    (run
      (assert-equals ((((.class) .parent) .parent) .parent)
                     (largest-containing-ordered-group)))
    )

  (card /experiments/groups/ordered-group/india ()
    )

  ;;=======================================================================
  ;;  Layout
  ;;=======================================================================
  ;;  Experimental layout framework by Brian and Eric.  We need to do a
  ;;  bunch of stuff before this is ready for prime time, perhaps including
  ;;  some tweaks to our underlying object model.
  ;;
  ;;  This code has been commented out for now (and not ported to the new
  ;;  object model), because we are reconsidering the details of our layout
  ;;  system in light of the new object model.  Basically, we're just
  ;;  keeping this around as design documentation.

  #|
  (define-element-template %colored-box%
      ;; These attributes are probably vbox-specific.
      [[left-margin :type <integer> :default 0]
       [top-margin  :type <integer> :default 0]
       [right-margin  :type <integer> :default 0]
       [bottom-margin :type <integer> :default 0]
       [halign :type <symbol> :default 'left] ; right, center, expand
       ]
      (%rectangle%)
    (on layout (&key desired-width desired-height)
      (define current (.shape))
      (define updated
        (rect 0 0
              (or desired-width (rect-width current))
              (or desired-height (rect-height current))))
      (set! (.shape) updated)))
  
  (define-element-template %vbox% 
      [[spacing :type <integer> :default 0]
       ;;[padding :type <integer> :default 0] ; Don't we wish.
       [left-padding :type <integer> :default 0]
       [top-padding :type <integer> :default 0]
       [right-padding :type <integer> :default 0]
       [bottom-padding :type <integer> :default 0]

       ;; MIXIN: Should match list for %colored-box%.
       [left-margin :type <integer> :default 0]
       [top-margin  :type <integer> :default 0]
       [right-margin  :type <integer> :default 0]
       [bottom-margin :type <integer> :default 0]
       [halign :type <symbol> :default 'left] ; right, center, expand

       ;; TODO - Are these the right design?
       [minimum-width :default 0]
       [minimum-height :default 0]
       ]
      (%box%)
    ;; TODO - Refactor me.  Please!
    (on layout (&key desired-width desired-height)
      ;; PASS 1: Lay everything out left-aligned so we can calculate
      ;; our overall width, height, etc.
      (define current-y top-padding)
      (define current-width (or desired-width 0))
      (foreach [child (node-elements self)]
        (send* child 'layout :recursive? #f :ignorable? #f)
        (set! (child .at)
              (point (+ left-padding (child .left-margin))
                     (+ current-y (child .top-margin))))
        (set! current-y (+ current-y
                           (rect-height (child .shape))
                           (child .bottom-margin)
                           spacing))
        (set! current-width
              (max current-width
                   (+ (point-x (child .at))
                      (rect-width (child .shape))
                      (child .right-margin)
                      right-padding)))
        )

      ;; INTERMEZZO: Set our own shape.
      (set! (.shape)
            (rect 0 0
                  (max minimum-width current-width)
                  (max minimum-height
                       (or desired-height 0)
                       (+ (- current-y spacing) bottom-padding))))

      ;; PASS 2: Now that we know how wide we are, align our children.
      (foreach [child (node-elements self)]
        ;; We want to align relative to this space.
        (define begin-x left-padding)
        (define end-x (- (rect-width (.shape)) right-padding))

        ;; Perform the alignment.
        (define width (+ (child .left-margin)
                         (rect-width (child .shape))
                         (child .right-margin)))
        (define halign (child .halign))
        (define offset-x
          (+ (case halign
               [[left expand] begin-x]
               [[center]      (/ (- (- end-x begin-x) width) 2)]
               [[right]       (- end-x width)]
               [else (error (cat "Unknown alignment: " halign))])
             (child .left-margin)))
        (set! (child .at) (point offset-x (point-y (child .at))))
        (when (eq? halign 'expand)
          ;; TODO - Of course, this hideously invalidates our existing
          ;; layout.  Fixing this could get pretty ugly.  Maybe we should
          ;; only allow EXPAND when we have a known, fixed width, or under
          ;; some other set of rules?
          (let [[available (- (- end-x (child .right-margin))
                              (+ begin-x (child .left-margin)))]]
            (send* child 'layout
                   :arguments (list :desired-width available
                                    :desired-height
                                    (rect-height (child .shape)))
                   :recursive? #f :ignorable? #f))))
        
      ))
  
  (define (colored-box parent &rest keys)
    (apply create %colored-box%
           :parent parent
           :shape (rect 0 0 20 20) 
           :color $color-white
           keys))

  (card /experiments/layout (%standard-test-card% :title "Layout")
    (create %vbox% :name 'vbox :at (below @title 20)
            :shape (rect 0 0 50 300) :spacing 10
            :left-padding 10 :top-padding 10
            :right-padding 10 :bottom-padding 10)
    
    (colored-box @vbox)
    (colored-box @vbox :left-margin 10)

    (create %vbox%
            :parent @vbox :name 'nested
            :shape (rect 0 0 20 10)
            :spacing 5
            :minimum-width 40)
    (colored-box @vbox/nested)
    (colored-box @vbox/nested :halign 'center)
    (colored-box @vbox/nested :halign 'right)
    (colored-box @vbox/nested :halign 'expand)

    ;; This annoys me.  Can we create an ON CHILD-ADDED event or something?
    ;; (And a matching CHILD-REMOVED?)
    (send @vbox layout))
  |#

  )
