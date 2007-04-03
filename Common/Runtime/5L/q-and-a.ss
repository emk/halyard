(module q-and-a (lib "5l.ss" "5L")
  
  (provide %question% %answer%)
  
  ;;; A question.  Individual answers should be child elements of this
  ;;; question, and should be based on the template %ANSWER% below.
  (define-element-template %question% [] (%custom-element%)
    
    ;;; Get all the answers to this question.
    (on get-answers ()
      (filter (fn (elem)
                (extends-template? elem %answer%))
              (node-elements self)))
    
    ;;; Get all the correct answers to this question.
    (on get-correct-answers ()
      (filter (fn (answer)
                (prop answer correct?))
              (send self get-answers)))
    
    ;;; Get all the incorrect answers to this question.
    (on get-incorrect-answers ()
      (filter (fn (answer)
                (not (prop answer correct?)))
              (send self get-answers)))
    
    ;;; Tells the answers to this question whether or not to ignore the
    ;;; cursor by setting WANTS-CURSOR?.  When the cursor is ignored, answers
    ;;; are drawn normally but are unaffected by the mouse.
    (on set-wants-cursor?! (enable?)
      (foreach [answer (send self get-answers)]
        (set! (prop answer wants-cursor?) enable?)))
    
    ;;; Grey-out all the incorrect answers to this question.  We do
    ;;; this by setting the answers' ENABLED? properties to #f, which
    ;;; indirectly sets WANTS-CURSOR? to #f (which it typically is by
    ;;; this point, anyway). We also set incorrect answers to not be 
    ;;; highlighted any more.
    ;;;
    ;;; If you want greyed-out buttons to actually look grey, make sure
    ;;; you handle ON DRAW-ANSWER calls with an argument DISABLED. 
    (on dim-incorrect-answers ()
      (foreach [answer (send self get-answers)]
        (set! (prop answer enabled?) #f)
        (unless (prop answer correct?)
          (set! (prop answer highlighted?) #f))))
    
    ;;; Highlight all the correct answers to this question and disable
    ;;; the incorrect ones.  If you have multiple correct answers, and you
    ;;; need to reveal them one at a time, you'll need to roll your
    ;;; own copy of this function.
    (on reveal-correct-answers (&key (refresh? #t))
      (foreach [answer (send self get-correct-answers)]
        (set! (prop answer highlighted?) #t))
      (send self dim-incorrect-answers)
      (when refresh?
        (refresh)))
    
    ;;; Override this handler to respond to a correct answer.
    (on correct-answer (answer)
      (call-next-handler))
    
    ;; Override this handler to response to an incorrect answer.
    (on incorrect-answer (answer)
      (call-next-handler))
    )
  
  ;;; An answer to a multiple choice question.  Should always be a child
  ;;; of an element based on %QUESTION%, above.
  (define-element-template %answer%
      [[highlighted? :type <boolean> :label "Currently highlighted?"
                     :default #f]
       [correct? :type <boolean> :label "Correct answer?" :default #f]]
      (%basic-button%)
    
    (on prop-change (name value prev veto)
      (case name
        [[highlighted? correct?]
         (send self draw)]
        [else
         (call-next-handler)]))
    
    (on draw-button (style)
      (send self draw-answer
        (if highlighted?
          (if correct?
            'correct
            'incorrect)
          style)))
    
    ;;; Draw this answer, clearing the viewport first.  Must be overriden
    ;;; by child templates.  Answer styles are CORRECT, INCORRECT, NORMAL,
    ;;; ACTIVE, PRESSED or DISABLED.
    (on draw-answer (style)
      (void))
    
    ;;; Respond to a button click.  This method will only need to be
    ;;; overridden in exceptional circumstances.
    (on button-clicked (event)
      (set! highlighted? #t)
      (send self set-wants-cursor?! #f)
      (refresh)
      (run-deferred
       (fn () (send self answer-chosen))))
    
    ;;; An answer has been chosen by the user.  This method will only need to
    ;;; be overridden in exceptional circumstances.
    (on answer-chosen ()
      (if correct?
        (send self correct-answer)
        (send self incorrect-answer)))
    
    ;;; This answer has been chosen, and it is correct.
    (on correct-answer ()
      (send (node-parent self) correct-answer self))
    
    ;;; This answer has been chosen, and it is incorrect.
    (on incorrect-answer ()
      (send (node-parent self) incorrect-answer self))
    )
  
  )
