(module bugs (lib "5l.ss" "5L")
  (require (file "base.ss"))


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
  (define-card-template %bug-card%
      [[bug-id :type <string> :label "Bug ID"]
       [bug-title :type <string>]
       [instructions :type <string>]
       [status :type <symbol> :default 'unfixed]]
      (%standard-test-card%
       :title (cat "Bug #" bug-id ": " bug-title
                   " (" (bug-status->string status) ")"))
    (text (point 10 (+ 10 (rect-bottom (send @title bounds))))
          $title-style instructions :name 'instructions)
    (box (rect 10 (+ 40 (rect-bottom (send @instructions bounds))) 790 590)
         :name 'workspace)
    )


  ;;;======================================================================
  ;;;  Bugs
  ;;;======================================================================

  (sequence bugs)

  (card bugs/bug-2061-measure-text-shadow
      (%bug-card% :bug-id "2061" :status 'fixed
                  :bug-title "measure-text ignores shadow"
                  :instructions "The drop shadow should not be chopped off.")
    (define style (stylesheet
                   :base $style-bug-base
                   :size 36
                   :shadow-offset 10
                   :shadow-color (color 0 255 0)))
    (text (point 0 0) style
          "Hello there, Quincy!"
          :max-width 180 :parent @workspace))

  (card bugs/bug-2078-measure-text-descender
      (%bug-card% :bug-id "2078" :status 'fixed
                  :bug-title "measure-text allegedly chops descender off Q"
                  :instructions "The upper and lower rows should match.")
    ;; I thought this was a bug, but in fact it's just the way things look.
    ;; There may still be similar cases, though.
    (define style (stylesheet
                   :base $style-bug-base
                   :size 16
                   :family "Century Schoolbook L"
                   :height-adjustment (percent -45)))
    (text (point 0 0) style "Qqgjy\nQqgjy" :parent @workspace)
    
    ;; This one, on the other hand, does get clipped.  Thanks to ddc for
    ;; finding this case.
    (define style2 (stylesheet
                    :base $style-bug-base
                    :size 12
                    :family "Nimbus Sans L"
                    :height-adjustment (percent -45)))
    (text (point 0 50) style2 "g\ng" :parent @workspace)
    
    ;; In the above cases, the line should be automatically big enough to
    ;; fit.  But in this case, we have an oversize character (the integral
    ;; sign), and we must allocate a larger-than-normal line.
    (define style3 (stylesheet :base $style-bug-base :size 18))
    (text (point 0 100) style3 "&#x222B;Tj\n&#x222B;Tj" :parent @workspace)
    
    )

  (card bugs/bug-2493-measure-text-too-narrow
      (%bug-card% :bug-id "2493" :status 'fixed
                  :bug-title "measure-text result is too narrow"
                  :instructions "The text should read \"-T X\" below.")
    (define style
      (stylesheet :base $style-bug-base :family "Nimbus Sans L" :size 16))
    (text (point 0 0) style "-T X" :parent @workspace))
 
  )
