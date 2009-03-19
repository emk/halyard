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

;;; This file contains older, deprecated APIs intended for use with legacy
;;; programs.
(module deprecated (lib "halyard.ss" "halyard")

  ;; Import %call-prim from the engine.
  ;; DO NOT CALL ANYTHING BUT LOGGING FUNCTIONS USING '%CALL-PRIM'--USE
  ;; 'CALL-PRIM' INSTEAD.
  (require #%engine-primitives)
  
  (provide app-log debug-log debug-caution warning non-fatal-error caution)

  ;;; Write a message to Halyard.log.  This log is always present on a user's
  ;;; system, and is never deleted, so use this function sparingly.
  (define (app-log msg)
    (info #f msg))
  
  ;;; Write a message to Debug.log, which is only present on developer
  ;;; systems (though the last hundred lines are always available in a
  ;;; crash report).  This is a very high-volume log, so feel free to be
  ;;; verbose.
  (define (debug-log msg)
    (debug #f msg))
  
  ;;; Print a warning message to Debug.log.  High-volume output is OK.
  (define (debug-caution msg)
    (warn #f msg))

  ;;; Warn the multimedia author about a possible problem.
  (define (warning msg)
    (warn #f msg))
  
  ;; Renamed functions.
  (define non-fatal-error report-error)
  (define caution warning)

  ;; Backwards compatibility wrappers for old Swindle-based event classes.
  (provide <event> event? event-stale?
           mark-event-as-not-handled!
           veto-event! event-vetoed?
           <update-ui-event> update-ui-event? event-command
           <char-event> char-event? event-character event-modifiers
           event-modifiers-and-character
           <mouse-event> mouse-event? event-position event-double-click?
           <url-event> url-event? event-url
           <text-event> text-event? event-text
           <browser-navigate-event> browser-navigate-event?
           <progress-changed-event> event-progress-done? event-progress-value
           <media-event> <media-finished-event>
           <media-caption-event> event-caption)

  (define <event> %event%)
  (define (event? e) (e .instance-of? %event%))
  (define (event-stale? e) (e .stale?))
  (define (mark-event-as-not-handled! e) (e .mark-as-not-handled!))
  (define (veto-event! e) (e .veto!))
  (define (event-vetoed? e) (e .vetoed?))
  (define <update-ui-event> %update-ui-event%)
  (define (update-ui-event? e) (e .instance-of? %update-ui-event%))
  (define (event-command e) (e .command))
  (define <char-event> %char-event%)
  (define (char-event? e) (e .instance-of? %char-event%))
  (define (event-character e) (e .character))
  (define (event-modifiers e) (e .modifiers))
  (define (event-modifiers-and-character e) (e .modifiers-and-character))
  (define <mouse-event> %mouse-event%)
  (define (mouse-event? e) (e .instance-of? %mouse-event%))
  (define (event-position e) (e .position))
  (define (event-double-click? e) (e .double-click?))
  (define <url-event> %url-event%)
  (define (url-event? e) (e .instance-of? %url-event%))
  (define (event-url e) (e .url))
  (define <text-event> %text-event%)
  (define (text-event? e) (e .instance-of? %text-event%))
  (define (event-text e) (e .text))
  (define <browser-navigate-event> %browser-navigate-event%)
  (define (browser-navigate-event? e)
    (e .instance-of? %browser-navigate-event%))
  (define <progress-changed-event> %progress-changed-event%)
  (define (event-progress-done? e) (e .done?))
  (define (event-progress-value e) (e .value))
  (define <media-event> %event%)
  (define <media-finished-event> %event%)
  (define <media-caption-event> %media-caption-event%)
  (define (event-caption e) (e .caption))


  )