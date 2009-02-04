;; @BEGIN_LICENSE
;;
;; Halyard - Multimedia authoring and playback system
;; Copyright 1993-2009 Trustees of Dartmouth College
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

(module test-elements (lib "halyard.ss" "halyard")
  (require (lib "halyard-unit.ss" "halyard"))
  (provide %element-test-case% %element-test-suite%)
  
  ;;; Test case class for tests that involve elements.  This automatically 
  ;;; creates a temporary parent element during SETUP-TEST that becomes the
  ;;; DEFAULT-ELEMENT-PARENT during the test method, and is deleted during
  ;;; TEARDOWN-TEST.
  (define-class %element-test-case% (%test-case%)
    (attr element-parent #f :writable? #t)

    (setup-test
      ;; In case we failed to teardown the test in a previous run, we need
      ;; to make sure that the temporary-parent element is deleted.
      (delete-element-if-exists 'temporary-parent)
      (set! (.element-parent) 
            (%box% .new :bounds (rect 0 160 
                                      (rect-width $screen-rect) 
                                      (rect-height $screen-rect))
                        :name 'temporary-parent)))

    (def (run-test-method-inner)
      (assert (.element-parent))
      (with-default-element-parent (.element-parent)
        (super)))

    (teardown-test
      (when (.element-parent)
        (delete-element (.element-parent)))
      ;; Unset our element parent in case someone in our superclass
      ;; or subclass chain tries to access it for some reason.
      (set! (.element-parent) #f)))

  ;; Quick and dirty clickable text.
  (define-class %element-test-button% (%text%)
    (attr test-case)
    (attr test)
    (value style $halyard-unit-style)
    (value text ((.test) .title))
    (value clickable-where-transparent? #t)

    (def (mouse-down event)
      (.propagate 'switch-test (.test-case) (.test))))

  ;; Overlay that displays test cases and their associated tests as clickable
  ;; buttons.
  (define-class %element-interaction-overlay% (%rectangle%)
    (attr tests)
    (value bounds (rect 100 200 
                        (rect-width $screen-rect) 
                        (rect-height $screen-rect)))
    (value color (color 255 255 255 200))
    
    (setup 
      (define y 0)
      (foreach [test-case (.tests)]
        (define text
          (%text% .new :at (point 0 y) :style $halyard-unit-style 
                       :text (test-case .to-string) :parent self))
        (set! y (point-y (below text 4)))
        (foreach [test (test-case .test-methods)]
          (define button
            (%element-test-button% .new 
              :at (point 10 y) :parent self
              :test-case test-case :test test))
          (set! y (point-y (below button 4)))))
      (.fit-to-children! 5)))
  
  ;;; Card class for running element tests.  Acts like the regular
  ;;; %test-suite% class, but adds an interface for clicking on
  ;;; individual tests to run just the SETUP-TEST and test method
  ;;; itself, without doing teardown, to leave the elements around on
  ;;; the screen so they can be interacted with manually.
  ;;;
  ;;; NOTE: because this doesn't automatically call TEARDOWN-TEST on a
  ;;; test case when you run a test using the element interaction
  ;;; interface, it means that it's possible for TEARDOWN-TEST to
  ;;; never be called if you jump away from the card, which may leave
  ;;; resources not properly cleanedu up.  Because of that, you should
  ;;; avoid creating test cases that require TEARDOWN-TEST to be
  ;;; called if you're using this interface, or if you don't, be sure
  ;;; to clean up your tests by clicking on "Show element interaction
  ;;; intervace" before jumping away from the card.
  (define-class %element-test-suite% (%test-suite%)
    (attr current-test #f :writable? #t)
    (text show-interface-button
        ((point 100 140) $halyard-unit-style 
         "Show element interaction interface"
         :clickable-where-transparent? #t)
      (def (mouse-down event)
        ((.parent) .show-element-interaction-interface)))

    (def (show-element-interaction-interface)
      (.cleanup)
      (%element-interaction-overlay% .new 
        :name 'element-interaction-overlay :tests (.tests)))

    (def (cleanup)
      (delete-element-if-exists 'element-interaction-overlay)
      (when (.current-test)
        ((.current-test) .teardown-test))
      (set! (.current-test) #f))
    
    (def (switch-test test-case test)
      (.cleanup)
      (set! (.current-test) (test-case .new :test-method test))
      ((.current-test) .setup-test)
      ((.current-test) .run-test-method-inner)))
  )