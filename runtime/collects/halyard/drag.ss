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

(module drag (lib "halyard.ss" "halyard")
  (require (lib "animate.ss" "halyard"))

  (provide %simple-draggable-object% %draggable-object% %drag-target% 
           call-with-dragging-disabled with-dragging-disabled
           point-in-element?)
  
  
  ;;=======================================================================
  ;;  Simple Draggable Objects
  ;;=======================================================================
  ;;  This is the most basic draggable object. You can drag it to anywhere
  ;;  on the screen, and it will just sit there.

  ;; Make a moveable object -- one that can be dragged around the screen
  ;;  using the mouse.
  (define-class %simple-draggable-object% (%custom-element%)
    (attr drag-bounds $screen-rect :type <rect>
          :label "Boundary for region of motion")
    (attr float-when-dragging? #t :type <boolean>
          :label "Float above other items when dragging")
    
    (setup
      (set! (slot 'max-x)
            (- (rect-right (.drag-bounds))
               (rect-width (.shape))))
      (set! (slot 'max-y)
            (- (rect-bottom (.drag-bounds))
               (rect-height (.shape))))
      (set! (slot 'min-x) (rect-left (.drag-bounds)))
      (set! (slot 'min-y) (rect-top (.drag-bounds)))
      (set! (slot 'offset-x) 0)
      (set! (slot 'offset-y) 0))

    (def (%apply-drag-offset p)
      (let [[new-point (point (+ (point-x p) (slot 'offset-x)) 
                              (+ (point-y p) (slot 'offset-y)))]]
        (when (> (slot 'min-x) (point-x new-point))
          (set! (point-x new-point) (slot 'min-x)))
        (when (>=  (point-x new-point) (slot 'max-x))
          (set! (point-x new-point) (slot 'max-x)))
        (when (> (slot 'min-y)  (point-y new-point))
          (set! (point-y new-point) (slot 'min-y)))
        (when (>=  (point-y new-point) (slot 'max-y))
          (set!  (point-y new-point) (slot 'max-y)))
        new-point))

    (def (mouse-down event)
      (define p (event .position))
      (define bounds (offset-by-point (.shape) (.at)))
      (define at (point (rect-left bounds) 
                        (rect-top bounds)))
      (set! (slot 'offset-x) (- (point-x at) (point-x p)))
      (set! (slot 'offset-y) (- (point-y at) (point-y p)))
      (grab-mouse self)
      (.drag-started event))
    (def (mouse-moved event)
      (when (mouse-grabbed-by? self)
        (set! (.at) (.%apply-drag-offset (event .position)))))
    (def (mouse-up event)
      (when (mouse-grabbed-by? self)
        (ungrab-mouse self)
        (.drag-finished event)))

    ;; Functions to be overridden by subclasses.
    (def (drag-started event)
      (when (.float-when-dragging?)
        (set! (.dragging?) #t)))
    (def (drag-finished event)
      (when (.float-when-dragging?)
        (set! (.dragging?) #f))))


  ;;=======================================================================
  ;;  Draggable Objects
  ;;=======================================================================
  ;;  These objects may be dragged only to specified targets on the screen.
  ;;  If you drag them anywhere else, they snap back to their home point.

  (define (call-with-dragging-disabled obj thunk)
    (let [[wants-cursor? #f]]
      (dynamic-wind
       (fn ()
         (set! wants-cursor? (obj .wants-cursor?))
         (set! (obj .wants-cursor?) #f))
       thunk
       (fn ()
         (set! (obj .wants-cursor?) wants-cursor?)))))

  (define-syntax with-dragging-disabled
    (syntax-rules ()
      [(with-dragging-disabled obj . body)
       (call-with-dragging-disabled obj (fn () . body))]))

  ;; TODO Is this really the best way to choose a drop target?
  ;; I think the Mac probably works this way...
  (define (point-in-element? p elem)
    (define shape (offset-by-point (elem .shape)
                                   (local->card elem (point 0 0))))
    ;; TODO - We really need POINT->ELEMENT.
    (point-in-shape? p shape))
  
  (define-class %draggable-object% (%simple-draggable-object%)
    (attr home-point :type <point> :label "Home Point" :writable? #t)
    (value at (.home-point))

    (def (drag-finished event)
      (super)
      (define p (event .position))
      (let recurse [[targets *drag-targets*]]
        (cond
         [(null? targets)
          (.drag-failed event)]
         [(and (point-in-element? p (car targets))
               ((car targets) .drag-allowed? event self))
          ((car targets) .drag-succeeded event self)
          (.drag-succeeded event (car targets))]
         [#t
          (recurse (cdr targets))])))

    ;;; Go to POINT, taking MS milliseconds to do so.  This runs
    ;;; immediately, and will call IDLE.
    (def (go-to-point-now point &key (ms 100))
      (with-dragging-disabled self
        (animate ms (ease-in/out (slide self point)))))
    
    ;;; Like GO-TO-POINT-NOW, but the actual animation is delayed until
    ;;; the next time deferred callbacks are run.
    (def (go-to-point point &key (ms 100))
      (run-deferred (callback (.go-to-point-now point :ms ms))))

    ;;; Go this object's HOME-POINT, taking MS milliseconds to do so.  This
    ;;; runs immediately, and will call IDLE.
    (def (go-home-now &key (ms 100))
      (.go-to-point-now (.home-point) :ms ms))

    ;;; Like GO-HOME-NOW, but the actual animation is delayed until
    ;;; the next time deferred callbacks are run.
    (def (go-home &key (ms 100))
      (run-deferred (callback (.go-home-now :ms ms))))

    ;; Functions to be overridden by subclasses.
    (def (drag-failed event)
      (.go-home))
    (def (drag-succeeded event target)
      (void)))


  ;;=======================================================================
  ;;  Drag Targets
  ;;=======================================================================
  ;;  Drag targets can receive objects dropped on top of them.  What
  ;;  happens next is the drag target's responsibility.
  
  (provide *drag-targets*)

  (define *drag-targets* '())

  (define-class %drag-target% (%custom-element%)
    (setup
      ;; Add ourselves to the list of active drag targets.
      (set! *drag-targets* (append! *drag-targets* (list self))))

    (def (exit)
      (super)
      (set! *drag-targets* (remove self *drag-targets* eq?)))

    ;; Functions to be overridden by subclasses.
    (def (drag-allowed? event draggable)
      #f)
    (def (drag-succeeded event draggable)
      (void))
    )

  )