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

(module halyard-unit (lib "halyard.ss" "halyard")
  (require (lib "mizzen-unit.ss" "mizzen"))
  (provide (all-from (lib "mizzen-unit.ss" "mizzen")))

  (provide %test-suite% $halyard-unit-style run-all-test-suites)
  
  (define-stylesheet $halyard-unit-style
    :family "Nimbus Sans L"
    :size 16
    :color (color 0 0 0)
    :highlight-color (color #x00 #x00 #xC0))
  
  (define-stylesheet $halyard-unit-title-style
    :base $halyard-unit-style
    :size 24)
  
  (define-stylesheet $halyard-unit-passed-style
    :base $halyard-unit-style
    :size 36
    :flags '(bold)
    :color (color #x00 #xC0 #x00))
  
  (define-stylesheet $halyard-unit-failed-style 
    :base $halyard-unit-passed-style
    :color (color #xC0 #x00 #x00))
  
  ;;; Are we current attempting to run through all the test cards
  ;;; automatically?
  (define *running-all-tests?* #t)

  ;;; When called, this function will run all the test-suite cards in the
  ;;; group "tests".
  (define (run-all-test-suites)
    (define first-test (tests/run-all .card-next))
    (when first-test
      (set! *running-all-tests?* #t)
      (jump (first-test .card-next))))

  ;;; Display the results of a set of tests on a card.
  (define-class %test-suite% (%card%)
    (attr tests)

    (def (setup)
      (super)
      (clear-dc (color #xFF #xFF #xFF))
      ;; Draw a title on the card (making it easier to tell when each
      ;; test-suite card is loaded).
      (draw-text (rect 30 30 800 100) $halyard-unit-title-style
                 (cat "Card: " (.full-name))))

    (def (run)
      (super)
      (let [[report (%test-report% .new)]]
        (foreach [test-class (.tests)]
          (test-class .run-tests report))
        (.report-test-results report)))

    (def (report-test-results report)
      (define (draw-result style text)
      (draw-text (rect 100 100 700 175) style text))
      (if (report .success?)
        (begin
          (draw-result $halyard-unit-passed-style "OK")
          (when *running-all-tests?*
            (let [[next (card-next)]]
              (if next
                (jump next)
                (set! *running-all-tests?* #f)))))
        (begin
            (draw-result $halyard-unit-failed-style "FAILED")
            (draw-text (rect 100 175 700 500) $halyard-unit-style
                       (apply string-append
                              (map (fn (failure)
                                     (string-append
                                      "<h><b>" (string->xml 
                                                (failure .title))
                                      "</b></h>\n"
                                      (string->xml 
                                       (failure .message))
                                      "\n\n"))
                                   (report .failures))))
            (set! *running-all-tests?* #f))))
    )

  ;;========================================================================
  ;;  Test fixtures
  ;;========================================================================
  ;;  We support "fixture" directories, which contain files used by various
  ;;  test suites.

  (provide halyard-fixture-dir)
  (define (halyard-fixture-dir name)
    (build-path (current-directory) "Runtime" "halyard" (cat name "-fixtures")))


  ;;========================================================================
  ;;  Standard test sequence
  ;;========================================================================
  ;;  Cards containing unit tests should all be placed into this sequence.

  (require (lib "tests.ss" "mizzen"))

  (provide tests)

  (group tests (%card-group% :ordered? #t))

  (card tests/run-all (%card%)
    (text instructions ((point 0 0) $halyard-unit-title-style
                        "Click to run tests")
      (setup
        (.center-on-parent!)))
    (clickable-zone run-zone ($screen-rect run-all-test-suites))
        
    (setup
      (clear-dc (color #xFF #xFF #xFF)))
    )

  (card tests/mizzen (%test-suite% :tests $mizzen-tests))
  
  )
