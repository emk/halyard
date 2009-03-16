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

  (define-class %event-test% (%test-case%)
    (test "<event> should have handled? and stale? flags"
      (define e (make <update-ui-event> :command 'foo))
      (assert (event? e))
      (mark-event-as-not-handled! e) ; No way to test result.
      (assert-equals #f (event-stale? e))
      (define stale (make <update-ui-event> :command 'foo :stale? #t))
      (assert-equals #t (event-stale? stale)))
    (test "<update-ui-event> should have command"
      (define e (make <update-ui-event> :command 'foo))
      (assert (update-ui-event? e))
      (assert-equals 'foo (event-command e)))
    (test "<char-event> should have character and modifiers"
      (define e (make <char-event> :character #\a :modifiers '(alt)))
      (assert (char-event? e))
      (assert-equals #\a (event-character e))
      (assert-equals '(alt) (event-modifiers e))
      (assert-equals '(alt #\a) (event-modifiers-and-character e)))
    (test "<mouse-event> should have position and double-click?"
      (define e (make <mouse-event>
                  :position (point 10 10)
                  :double-click? #t))
      (assert (mouse-event? e))
      (assert-equals (point 10 10) (event-position e))
      (assert-equals #t (event-double-click? e)))
    (test "<url-event> should have a URL"
      (define e (make <url-event> :url "http://example.com/"))
      (assert (url-event? e))
      (assert-equals "http://example.com/" (event-url e)))
    (test "<text-event> should have text"
      (define e (make <text-event> :text "foo"))
      (assert (text-event? e))
      (assert-equals "foo" (event-text e)))
    (test "<browser-navigate-event> should have a URL"
      (define e (make <browser-navigate-event> :url "http://example.com/"))
      (assert (browser-navigate-event? e))
      (assert-equals "http://example.com/" (event-url e))
      (assert-equals #f (event-vetoed? e))
      (veto-event! e)
      (assert-equals #t (event-vetoed? e)))
    (test "<progress-changed-event> should have done? and value"
      (define e (make <progress-changed-event> :done? #f :value 0.5))
      (assert (event? e))
      (assert-equals #f (event-progress-done? e))
      (assert-equals 0.5 (event-progress-value e)))
    (test "<media-event> and <media-finished-event> should exist"
      (assert (event? (make <media-event>)))
      (assert (event? (make <media-finished-event>))))
    (test "<media-caption-event> should have caption"
      (define e (make <media-caption-event> :caption "foo"))
      (assert (event? e))
      (assert-equals "foo" (event-caption e)))
    )

  (card /tests/events
      (%test-suite%
       :tests (list %event-test%)))
  )