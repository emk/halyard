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

;; When developing Quake 2 levels, it becomes annoying to have to keep
;; using "noclip" and voyaging to specific spots on the level.  Here is an
;; API which can be used for jumping around in a controlled way.  We do not
;; recommend using this for anything but level development, because the
;; underlying "teleport" command is shady.
;;
;; To use: See DEFINE-QUAKE2-TELEPORT-LOCATION to set up named locations
;; (with optional associated code), and type "goto location-name" on the
;; Quake 2 console to jump to those locations.  To get x, y, z and yaw
;; information, use the "pos" command on the Quake 2 console.
(module teleport (lib "halyard.ss" "halyard")
  (require (lib "quake2.ss" "halyard"))

  (provide quake2-teleport define-quake2-teleport-location)

  ;;; Jump to the specified location in Quake 2.  For debugging only.
  (define (quake2-teleport x y z yaw)
    (quake2-command (cat "teleport " x " " y " " z " " yaw)))

  (with-instance %quake2-level%
    ;;; Override this function to run code before each teleport to any
    ;;; named location.
    (def (prepare-for-quake2-teleport)
      (void))

    ;;; Teleport to the location NAME.
    (def (quake2-teleport-to name)
      (define meth
        (hash-table-get (.class.teleport-locations) name
                        (fn () (error "Unknown teleport destination" name))))
      (.prepare-for-quake2-teleport)
      (instance-exec self meth))

    (advise after (setup)
      ;; Install a console command _after_ Quake 2 gets set up.  We do this
      ;; once per Quake level so we have access to the correct SELF.
      (define-quake2-command (goto teleport-destination)
        (.quake2-teleport-to (string->symbol teleport-destination))))

    (with-instance (.class)
      (attr teleport-locations (make-hash-table))

      (def (define-quake2-teleport-location name x y z yaw meth)
        (hash-table-put! (.teleport-locations) name 
                         (method ()
                           (instance-exec self meth)
                           (quake2-teleport x y z yaw))))))

  ;;; Define a location to teleport to using the "goto" command on the
  ;;; console.  An example:
  ;;;
  ;;;   (define-quake2-teleport-location bridge (-55 245 22 90)
  ;;;     (quake2-command "say Teleporting to bridge."))
  (define-syntax define-quake2-teleport-location
    (syntax-rules ()
      [(_ name (x y z yaw) . body)
       (self .define-quake2-teleport-location 'name x y z yaw
             (method () . body))]))
  )