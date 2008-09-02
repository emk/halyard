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

(module state-db (lib "halyard.ss" "halyard")
  (require (lib "kernel.ss" "halyard/private"))


  ;;;======================================================================
  ;;;  State DB
  ;;;======================================================================
  ;;;  In most applications, it's necessary to update the GUI based on
  ;;;  some internal application state.  If both the GUI and the internal
  ;;;  state are complicated, the resulting application can be a mess.
  ;;;
  ;;;  One popular way to reduce this complexity is to use the
  ;;;  model-view-controller paradigm (see the web for details).  We choose
  ;;;  a similar approach: We create a global state database with
  ;;;  hierarchial keys of the form /foo/bar/baz.  To get data from the
  ;;;  state database, you need to register a listener, which will be run
  ;;;  immediately.  The engine will keep track of the data accessed, and
  ;;;  re-run the listener whenever any of that data changes.
  ;;;
  ;;;  A listener is a special kind of function.  You can create a listener
  ;;;  using DEFINE-STATE-DB-LISTENER or STATE-DB-FN.
  ;;;
  ;;;  This is an advanced language feature, and simple Halyard programs
  ;;;  will almost never need to use it.

  (provide set-state-db! register-state-db-fn!
           state-db-fn define-state-db-fn define-state-db-listener)
  
  ;;; Set the specified key in the state database.
  ;;;
  ;;; @param SYMBOL key The key to set.
  ;;; @param ANY val The new value.
  (define (set-state-db! key value)
    (call-prim 'StateDbSet key value))

  ;;; Register a listener with the state database, and call the listener
  ;;; the first time.
  ;;;
  ;;; @param NODE node The node to which this listener should be attached.
  ;;; @param LISTENER listener 
  (define (register-state-db-fn! node fn)
    (call-prim 'StateDbRegisterListener (node .full-name) fn))

  (define (make-state-db-fn f)
    (fn (listener-name listener-serial-number)
      (define (state-db key)
        (call-prim 'StateDbGet listener-name listener-serial-number key))
      (f state-db)))

  ;;; Create a function suitable for passing to REGISTER-STATE-DB-FN!.
  ;;;
  ;;; @syntax (STATE-DB-FN (state-db) body ...)
  ;;; @param FUNCTION state-db A function which will fetch data from the
  ;;;   state database.
  ;;; @param BODY body The code to run.
  (define-syntax state-db-fn
    (syntax-rules ()
      [(state-db-fn (state-db) . body)
       (make-state-db-fn (fn (state-db) . body))]))
  (define-syntax-indent state-db-fn 1)

  ;;; Equivalent to (define name (state-db-fn (state-db) ...)).
  ;;;
  ;;; @syntax (define-state-db-fn (name state-db) . body)
  (define-syntax define-state-db-fn
    (syntax-rules ()
      [(define-state-db-fn (name state-db) . body)
       (define name (state-db-fn (state-db) . body))]))
  (define-syntax-indent define-state-db-fn 1)
  
  ;;; Combines the features of REGISTER-STATE-DB-FN! and STATE-DB-FN.
  ;;;
  ;;; @syntax (DEFINE-STATE-DB-LISTENER (name state-db) body ...)
  ;;; @syntax (DEFINE-STATE-DB-LISTENER name value)
  (define-syntax (define-state-db-listener stx)
    (syntax-case stx ()
      [(define-state-db-listener (name state-db) . body)
       (quasisyntax/loc
        stx
        (define-state-db-listener name (state-db-fn (state-db) . body)))]
      [(define-state-db-listener name value)
       (quasisyntax/loc
        stx
        ;; We ignore #'NAME, but we use it to get the lexical context in
        ;; which a reasonable SELF variable is defined.
        (register-state-db-fn! #,(datum->syntax-object #'name 'self) value))]))
  (define-syntax-indent define-state-db-listener 1)


  ;;;======================================================================
  ;;;  Animated Graphic Elements
  ;;;======================================================================
  ;;;  This class is acceptable to use with state-db-related code, but
  ;;;  is deprecated for all other uses.
  ;;;
  ;;;  XXX - Note that this class has a major limitation: The x,y offsets
  ;;;  are not correctly reflected in .AT!

  (provide %animated-graphic%)

  ;;; An animated graphic is a specialized overlay that can be
  ;;; animated under state-db control.  In order to use it, create an
  ;;; %animated-graphic% passing in the list of graphics you would
  ;;; like to change between to :GRAPHICS, and the state DB path you
  ;;; would like to use to control the graphic to :STATE-PATH.  Also,
  ;;; create the following state DB keys, and set them in order to
  ;;; control the animation:
  ;;;
  ;;; <state-path>/index   Set this to the index within the GRAPHICS list
  ;;;                      that you want to be displayed.
  ;;; <state-path>/x       These do some sort of movement, not documented
  ;;; <state-path>/y       at the moment. For now, set them both to 0
  ;;;
  ;;; DEPRECATED: For normal code, please use %sprite% instead.  This
  ;;; class is primarily intended for use with Quake 2 overlays and the
  ;;; state-db.
  (define-class %animated-graphic% (%custom-element%)
    (attr state-path :type <symbol> :label "State DB Key Path")
    (attr graphics   :type <list>   :label "Graphics to display")
    (value shape (measure-graphics (.graphics)))

    (def (create-engine-node)
      (call-prim 'OverlayAnimated (.full-name)
                    (parent->card self
                                  (offset-rect (.shape) (.at)))
                    (make-node-event-dispatcher self) (.cursor)
                    (.alpha?) (.state-path)
                    (map (fn (p) (resolve-content-path "graphics" p))
                         (.graphics))))
    )


  ;;;======================================================================
  ;;;  State DB Debugging Support
  ;;;======================================================================

  (provide state-db-debug)

  (define-class %state-db-debugger%  (%invisible-element%)
    (attr path)
    (attr report-fn)

    (setup
      (define-state-db-listener (debug state-db)
        ((.report-fn) (state-db (.path))))))
  
  ;;; Here's a nasty little hack for reading the state database from
  ;;; outside an element.  Calling this from anywhere but the listener is
  ;;; definitely a bug.
  (define (state-db-debug path)
    (define result #f)
    (define (set-result! value)
      (set! result value))
    (define elem
      (%state-db-debugger% .new :parent (running-root-node)
                                :path path
                                :report-fn set-result!))
    (delete-element elem)
    result)

  
  ;;;======================================================================
  ;;;  State DB Time Support
  ;;;======================================================================
  ;;;  The state-db's /system/clock/seconds and /system/clock/milliseconds
  ;;;  values do not use the same units as CURRENT-MILLISECONDS.

  (provide state-db-seconds state-db-milliseconds)

  ;;  XXX - THESE SHOULD NOT USE STATE-DB-DEBUG!!! It's extremely slow.
  ;;  Ask one of the C++ programmers to add primitives which fetch
  ;;  these values.
  
  ;;; Get the current time in seconds, as recorded by the state-db.
  (define (state-db-seconds)
    (state-db-debug '/system/clock/seconds))

  ;;; Get the current time in miliseconds, as recorded by the state-db.
  (define (state-db-milliseconds)
    (state-db-debug '/system/clock/milliseconds))

  
  ;;;======================================================================
  ;;;  State DB Update Support
  ;;;======================================================================

  (provide update-state-db! inc-state-db! inc-state-db-if-true!)

  ;;  XXX - THIS SHOULD NOT USE STATE-DB-DEBUG!!! It's extremely slow.
  ;;  Ask one of the C++ programmers to add a primitive to do this.
  
  ;;; Apply FUNC to the current value stored at PATH, and replace it with
  ;;; the return value.
  (define (update-state-db! path func)
    (set! (state-db path) (func (state-db-debug path)))
    #f) ; Make absolutely certain we don't return the value we updated.
  
  ;;; Add AMOUNT to the value stored in the state-db at PATH.
  ;;;
  ;;; TODO - Decide whether this is redundant.
  (define (inc-state-db! path amount)
    (update-state-db! path (fn (value) (+ value amount))))

  ;;; Add AMOUNT to the value stored in the state-db at PATH, unless that
  ;;; value is #f.
  ;;;
  ;;; TODO - Decide whether this is redundant.
  (define (inc-state-db-if-true! path amount)
    (update-state-db! path
                      (fn (value)
                        (if value
                            (+ value amount)
                            value))))

  )