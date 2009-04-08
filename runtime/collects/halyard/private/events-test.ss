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

(module events-test (lib "halyard.ss" "halyard")
  (require (lib "halyard-unit.ss" "halyard"))
  (require (lib "events.ss" "halyard/private"))

  (define-class %events-test% (%test-case%)
    (test "%event% should have handled? and stale? flags"
      (define e (%update-ui-event% .new :command 'foo))
      (e .mark-as-not-handled!) ; No way to test result.
      (assert-equals #f (e .stale?))
      (define stale (%update-ui-event% .new :command 'foo :stale? #t))
      (assert-equals #t (stale .stale?)))
    (test "%update-ui-event% should have command"
      (define e (%update-ui-event% .new :command 'foo))
      (assert-equals 'foo (e .command)))
    (test "%char-event% should have character and modifiers"
      (define e (%char-event% .new :character #\a :modifiers '(alt)))
      (assert-equals #\a (e .character))
      (assert-equals '(alt) (e .modifiers))
      (assert-equals '(alt #\a) (e .modifiers-and-character)))
    (test "%mouse-event% should have position and double-click?"
      (define e (%mouse-event% .new
                  :position (point 10 10)
                  :double-click? #t))
      (assert-equals (point 10 10) (e .position))
      (assert-equals #t (e .double-click?)))
    (test "%url-event% should have a URL"
      (define e (%url-event% .new :url "http://example.com/"))
      (assert-equals "http://example.com/" (e .url)))
    (test "%text-event% should have text"
      (define e (%text-event% .new :text "foo"))
      (assert-equals "foo" (e .text)))
    (test "%browser-navigate-event% should have a URL"
      (define e (%browser-navigate-event% .new :url "http://example.com/"))
      (assert-equals "http://example.com/" (e .url))
      (assert-equals #f (e .vetoed?))
      (e .veto!)
      (assert-equals #t (e .vetoed?)))
    (test "%progress-changed-event% should have done? and value"
      (define e (%progress-changed-event% .new :done? #f :value 0.5))
      (assert-equals #f (e .done?))
      (assert-equals 0.5 (e .value)))
    (test "%media-caption-event% should have caption"
      (define e (%media-caption-event% .new :caption "foo"))
      (assert-equals "foo" (e .caption)))
    (test "%data-received-event% should have data"
      (define e (%data-received-event% .new :data "foo"))
      (assert-equals "foo" (e .data)))
    (test "%transfer-finished-event% should have success? and message"
      (define e (%transfer-finished-event% .new :success? #f :message "foo"))
      (assert-equals #f (e .success?))
      (assert-equals "foo" (e .message)))
    )

  (card /tests/events
      (%test-suite%
       :tests (list %events-test%)))
  )