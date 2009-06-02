;; PORTED
(module bugs (lib "halyard.ss" "halyard")
  (require (lib "base.ss" "halyard-test"))


  ;;;======================================================================
  ;;;  Support Functions
  ;;;======================================================================

  ;;; We have our own base style that inherits from nothing else so that
  ;;; we can reproduce our bugs under precisely-controlled conditions.
  (define-stylesheet $style-bug-base
    :family "Nimbus Roman No9 L"
    :size 12
    :flags '()
    :justification 'left
    :shadow-offset 0
    :color (color 255 255 255)
    :shadow-color (color 0 0 0)
    :highlight-color (color 0 255 255)
    :highlight-shadow-color (color 0 0 0)
    :height-adjustment (percent -20))
  
  ;;; Convert a bug status symbol to a string we can display to the user.
  (define (bug-status->string status)
    (define status-str (regexp-replace* "-" (symbol->string status) " "))
    (string-uppercase! status-str)
    status-str)

  ;;; A card which demonstrates a bug.
  (define-class %bug-card% (%standard-test-card%)
    (attr bug-id :type <string> :label "Bug ID")
    (attr bug-title :type <string>)
    (attr instructions :type <string>)
    (attr status 'unfixed :type <symbol>)

    (value title (cat "Bug #" (.bug-id) ": " (.bug-title)
                      " (" (bug-status->string (.status)) ")"))

    (text instructions-elem ((below (.title-elem) 10) $title-style
                             (.instructions)))
    (box workspace ((rect 10
                          (+ 40 (rect-bottom ((.instructions-elem) .bounds)))
                          790 590)))
    )


  ;;;======================================================================
  ;;;  Bugs
  ;;;======================================================================

  (group /bugs)

  (card /bugs/bug-2061-measure-text-shadow
      (%bug-card% :bug-id "2061" :status 'fixed
                  :bug-title "measure-text ignores shadow"
                  :instructions "The drop shadow should not be chopped off.")
    (setup
      (define style (stylesheet
                     :base $style-bug-base
                     :size 36
                     :shadow-offset 10
                     :shadow-color (color 0 255 0)))
      (new-text (point 0 0) style
                "Hello there, Quincy!"
                :max-width 180 :parent @workspace)))

  (card /bugs/bug-2078-measure-text-descender
      (%bug-card% :bug-id "2078" :status 'fixed
                  :bug-title "measure-text allegedly chops descender off Q"
                  :instructions "The upper and lower rows should match.")
    (setup
      ;; I thought this was a bug, but in fact it's just the way things
      ;; look.  There may still be similar cases, though.
      (define style (stylesheet
                     :base $style-bug-base
                     :size 16
                     :family "Century Schoolbook L"
                     :height-adjustment (percent -45)))
      (new-text (point 0 0) style "Qqgjy\nQqgjy" :parent @workspace)
    
      ;; This one, on the other hand, does get clipped.  Thanks to ddc for
      ;; finding this case.
      (define style2 (stylesheet
                      :base $style-bug-base
                      :size 12
                      :family "Nimbus Sans L"
                      :height-adjustment (percent -45)))
      (new-text (point 0 50) style2 "g\ng" :parent @workspace)
      
      ;; In the above cases, the line should be automatically big enough to
      ;; fit.  But in this case, we have an oversize character (the integral
      ;; sign), and we must allocate a larger-than-normal line.
      (define style3 (stylesheet :base $style-bug-base :size 18))
      (new-text (point 0 100) style3 "&#x222B;Tj\n&#x222B;Tj"
                :parent @workspace)
      ))

  (card /bugs/bug-2493-measure-text-too-narrow
      (%bug-card% :bug-id "2493" :status 'fixed
                  :bug-title "measure-text result is too narrow"
                  :instructions "The text should read \"-T X\" below.")
    (setup
      (define style
        (stylesheet :base $style-bug-base :family "Nimbus Sans L" :size 16))
      (new-text (point 0 0) style "-T X" :parent @workspace)))
  
  (card /bugs/bug-f1360-jump-idle-problem 
      (%bug-card% :bug-id "F1360" :status 'fixed
                  :bug-title "Jumping from a card calls an extra idle"
                  :instructions (cat "You should only see a blue rectangle, "
                                     "then you should be jumped to the start "
                                     "card."))
    (setup
      (new-rectangle (rect 0 0 200 150) (color #xff 0 0) :name 'first-rect 
                     :parent @workspace)
      (delete-element @workspace/first-rect)
      (new-rectangle (rect 0 0 200 150) (color 0 0 #xff) :name 'second-rect
                     :parent @workspace)
      (nap 20)
      (delete-element @workspace/second-rect)
      (new-rectangle (rect 0 0 200 150) (color 0 #xff 0) :name 'third-rect
                     :parent @workspace)
      (jump @start)))

  (card /bugs/bug-f2977-empty-text-crash
      (%bug-card% :bug-id "F2977" :status 'fixed
                  :bug-title "Empty text box crashes when text changed"
                  :instructions (cat "This card should not raise an error."))
    (setup
      (define example
        (new-text (point 0 0) $caption-style ""
                  :name 'example :parent @workspace))
      (set! (example .text) "It works!")))

  (card /bugs/bug-f4250-whitespace-only-text-crash
      (%bug-card% :bug-id "F4250" :status 'fixed
                  :bug-title "Whitespace-only strings crash %text%"
                  :instructions (cat "This card should not raise an error."))
    (setup
      (new-text (point 0 0) $caption-style " "
                :name 'example :parent @workspace)))

  (card /bugs/bug-f17014-element-initialization 
      (%bug-card% 
       :bug-id "F17014" :status 'fixed
       :bug-title 
       "0.5.31: Does not properly initialize elements with :shown? #f"
       :instructions 
       "You should see 4 sentences below, and they should be true")
    (text hidden ((point 100 100) $caption-style "This should not be visible"
                  :shown? #f))
    (text explicit-shown ((below (.hidden) 10) $caption-style 
                          "This should be visible" :shown? #t))
    (text default-shown ((below (.explicit-shown) 10) $caption-style
                         "This should also be visible"))
    (text wants-cursor ((below (.default-shown) 10) $caption-style
                        "This text should have a finger cursor"
                        :wants-cursor? #t))
    (text in-drag-layer ((below (.wants-cursor) 10) $caption-style
                         "This text should be on top of the rectangle"
                         :dragging? #t))
    (rectangle rect ((.in-drag-layer.bounds) (color 0 70 50))))

  )

