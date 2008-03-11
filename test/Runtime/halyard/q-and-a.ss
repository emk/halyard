(module q-and-a (lib "halyard.ss" "halyard")
  
  (provide %question% %answer%)
  
  ;;; A question.  Individual answers should be child elements of this
  ;;; question, and should be based on the template %ANSWER% below.
  (define-class %question% (%custom-element%)
    
    ;;; Get all the answers to this question.
    (def (get-answers)
      (filter (fn (elem)
                (elem .instance-of? %answer%))
              (node-elements self)))
    
    ;;; Get all the correct answers to this question.
    (def (get-correct-answers)
      (filter (fn (answer)
                (answer .correct?))
              (.get-answers)))
    
    ;;; Get all the incorrect answers to this question.
    (def (get-incorrect-answers)
      (filter (fn (answer)
                (not (answer .correct?)))
              (.get-answers)))
    
    ;;; Tells the answers to this question whether or not to ignore the
    ;;; cursor by setting WANTS-CURSOR?.  When the cursor is ignored, answers
    ;;; are drawn normally but are unaffected by the mouse.
    (after-updating wants-cursor?!
      (foreach [answer (.get-answers)]
        (set! (answer .wants-cursor?) (.wants-cursor?))))
    
    ;;; Grey-out all the incorrect answers to this question.  We do
    ;;; this by setting the answers' ENABLED? properties to #f, which
    ;;; indirectly sets WANTS-CURSOR? to #f (which it typically is by
    ;;; this point, anyway). We also set incorrect answers to not be 
    ;;; highlighted any more.
    ;;;
    ;;; If you want greyed-out buttons to actually look grey, make sure
    ;;; you handle ON DRAW-ANSWER calls with an argument DISABLED. 
    (def (dim-incorrect-answers)
      (foreach [answer (.get-answers)]
        (set! (answer .enabled?) #f)
        (unless (answer .correct?)
          (set! (answer .highlighted?) #f))))
    
    ;;; Highlight all the correct answers to this question and disable
    ;;; the incorrect ones.  If you have multiple correct answers, and you
    ;;; need to reveal them one at a time, you'll need to roll your
    ;;; own copy of this function.
    (def (reveal-correct-answers &key (refresh? #t))
      (foreach [answer (.get-correct-answers)]
        (set! (answer .highlighted?) #t))
      (.dim-incorrect-answers)
      (when refresh?
        (refresh)))
    
    ;;; Override this handler to respond to a correct answer.
    (def (correct-answer answer)
      ((.parent) .propagate 'correct-answer answer))
    
    ;; Override this handler to respond to an incorrect answer.
    (def (incorrect-answer answer)
      ((.parent) .propagate 'incorrect-answer answer))
    )
  
  ;;; An answer to a multiple choice question.  Should always be a child
  ;;; of an element based on %QUESTION%, above.
  (define-class %answer% (%basic-button%)
    (attr highlighted? #f :type <boolean> :label "Currently highlighted?"
          :writable? #t)
    (attr correct? #f :type <boolean> :label "Correct answer?"
          :writable? #t)

    (after-updating [highlighted? correct?]
      (.invalidate))
    
    ;;; Answer states are CORRECT, INCORRECT, NORMAL, ACTIVE, PRESSED or
    ;;; DISABLED.  You will want to override the DRAW method and call
    ;;; ANSWER-STATE to determine how to draw the answer.
    (def (answer-state)
      (if (.highlighted?)
        (if (.correct?)
          'correct
          'incorrect)
        (.button-state)))
    
    ;;; Respond to a button click.  This method will only need to be
    ;;; overridden in exceptional circumstances.
    (def (click)
      (set! (.highlighted?) #t)
      (set! (.wants-cursor?) #f)
      (refresh)
      (run-deferred
       (fn () (.answer-chosen))))
    
    ;;; An answer has been chosen by the user.  This method will only need to
    ;;; be overridden in exceptional circumstances.
    (def (answer-chosen)
      (if (.correct?)
        (.correct-answer)
        (.incorrect-answer)))
    
    ;;; This answer has been chosen, and it is correct.
    (def (correct-answer)
      ((.parent) .correct-answer self))
    
    ;;; This answer has been chosen, and it is incorrect.
    (def (incorrect-answer)
      ((.parent) .incorrect-answer self))
    )
  
  )
