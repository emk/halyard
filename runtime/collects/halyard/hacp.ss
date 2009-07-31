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

(module hacp (lib "halyard.ss" "halyard")
  (require (lib "url-request.ss" "halyard"))
  (require (lib "data-file.ss" "halyard"))
  (require (lib "uuid.ss" "halyard"))


  ;;=======================================================================
  ;;  Low-level HACP API
  ;;=======================================================================
  ;;  These functions send the actual HACP network requests directly.
  ;;  Generally speaking, these will only be used internally.  For detailed
  ;;  documentation, please see the HACP standard and the unit tests.

  (provide hacp-extension-register-user-request
           hacp-extension-new-session-request
           hacp-get-param-request
           hacp-put-param-request)

  (define (hacp-extension-register-user-request hacp-url uuid
                                                student-name
                                                student-id)
    (%json-request% .new
      :url (cat hacp-url "/register")
      :method 'post
      :parameters (list (cons "uuid" uuid)
                        (cons "name" student-name)
                        (cons "student_id" student-id))))

  (define (hacp-extension-new-session-request hacp-url uuid)
    (%json-request% .new
      :url (cat hacp-url "/new_session")
      :method 'post
      :parameters (list (cons "uuid" uuid))))

  ;; Get our HACP data from the server.  For now, we just throw that
  ;; data away without parsing it.
  ;;
  ;; WARNING: The (.result) member of the newly created request is expected
  ;; to change in the future.
  (define (hacp-get-param-request hacp-url session-id)
    (%easy-url-request% .new
      :url hacp-url
      :method 'post
      :parameters (list (cons "command" "getparam")
                        (cons "version" "4.0")
                        (cons "session_id" session-id))))

  (define (hacp-put-param-request hacp-url session-id key-val data)
    ;; Note that we do not percent-encode the "~" as required by the HACP
    ;; specification, because modern standards like RFC 3986 consider it an
    ;; "unreserved character" that should generally not be escaped when
    ;; encoding data in URLs.  Since HACP claims to use URL-encoding, we'll
    ;; just live dangerously and obey the current standards.
    (define core (regexp-replace* "&" (percent-encode-parameters key-val) "\n"))
    (define aicc-data
      (string-append "[Core]\n" core "\n"
                     "[Core_Lesson]\n" (percent-encode data) "\n"))
    (%easy-url-request% .new
      :url hacp-url
      :method 'post
      :parameters (list (cons "command" "putparam")
                        (cons "version" "4.0")
                        (cons "session_id" session-id)
                        (cons "aicc_data" aicc-data))))


  ;;=======================================================================
  ;;  HACP Fields
  ;;=======================================================================
  ;;  These functions are used in conjunction with the high-level API
  ;;  below.

  (provide valid-hacp-status? hacp-clear-fields! hacp-field set-hacp-field!
           set-hacp-status! set-hacp-objective-status!)

  ;;; Is 'value' a valid HACP status value?  This is mostly used for
  ;;; internal type-checking.  Note that you should <i>not</i> use
  ;;; 'browsed' without first reading the HACP manual carefully; it can
  ;;; only be used in a non-for-credit mode that we don't support.
  (define (valid-hacp-status? value)
    (and (memq value '(passed completed failed incomplete browsed
                       not-attempted))
         #t))

  ;; Internal: Convert an HACP status symbol to a string.
  (define (hacp-status->string status)
    (unless (valid-hacp-status? status)
      (error (cat "Expected a valid HACP status: " status)))
    (regexp-replace "-" (symbol->string status) " "))

  ;;; Clear all HACP fields.
  (define (hacp-clear-fields!)
    (set! *hacp-fields* (make-hash-table 'equal))
    (set! *hacp-next-objective-id* 1)
    (set! *hacp-objective-ids* (make-hash-table)))

  (define *hacp-fields* #f)
  (define *hacp-next-objective-id* #f)
  (define *hacp-objective-ids* #f)
  (hacp-clear-fields!)

  ;;; Get the value of an HACP field.
  (define (hacp-field name)
    (hash-table-get *hacp-fields* (string-downcase name) #f))

  ;;; Set the value of an HACP field.
  (define (set-hacp-field! name value)
    ;; Check errors aggressively, because these values aren't used until
    ;; later, and it will be hard to link them back to their original
    ;; callers if they're invalid.
    (unless (string? name)
      (error (cat "set-hacp-field! expects <<" name ">> to be a string")))
    (unless (or (string? value) (eq? value #f))
      (error (cat "set-hacp-field! expects <<" value
                  ">> to be a string or #f")))
    (hash-table-put! *hacp-fields* (string-downcase name) value))

  ;;; A thin wrapped around set-hacp-field! which translates status symbols
  ;;; to strings.
  (define (set-hacp-status! value)
    (set! (hacp-field "Status") (hacp-status->string value)))

  ;; Internal: Get the integer ID for the objective 'name'.
  (define (objective-name->id name)
    (unless (hash-table-has-key? *hacp-objective-ids* name)
      (hash-table-put! *hacp-objective-ids* name *hacp-next-objective-id*)
      (inc! *hacp-next-objective-id*))
    (hash-table-get *hacp-objective-ids* name))

  ;;; Set the status of the objective 'name' to a valid HACP status.
  (define (set-hacp-objective-status! name value)
    (define id (objective-name->id name))
    (set! (hacp-field (cat "J_ID." id)) (symbol->string name))
    (set! (hacp-field (cat "J_Status." id)) (hacp-status->string value)))


  ;;=======================================================================
  ;;  High-level HACP API
  ;;=======================================================================

  (provide hacp-initialize hacp-write hacp-done)

  (define *hacp-url* #f)
  (define *hacp-sid* #f)

  (define (run-request request)
    (request .wait)
    (request .response))

  ;;; Initialize an HACP session.
  ;;; TODO - Should we use symbols or strings for user prefs?
  (define (hacp-initialize hacp-url student-name)
    ;; If the user doesn't already have a UUID, assign one.
    (unless (user-pref 'uuid)
      (set! (user-pref 'uuid) (uuid)))

    ;; Register the user with the server.
    (run-request (hacp-extension-register-user-request
                  hacp-url (user-pref 'uuid) student-name (user-pref 'uuid)))

    ;; Create a new HACP session.
    (define session-info
      (run-request (hacp-extension-new-session-request hacp-url
                                                       (user-pref 'uuid))))
    (set! *hacp-url* (hash-table-get session-info "aicc_url"))
    (set! *hacp-sid* (hash-table-get session-info "aicc_sid"))

    ;; Make our initial GetParam request, which is required by the HACP
    ;; protocol.
    (run-request (hacp-get-param-request *hacp-url* *hacp-sid*))
    )

  ;;; Attempt to write our data to the server.  If sync? if #f, then
  ;;; perform the write in the background.  If sync? is #t, wait for the
  ;;; write to succeed or fail.
  (define (hacp-write &key sync?)
    (void))

  ;;; Terminate our HACP session, if one is running, and attempt to flush
  ;;; our data to the server synchronously.
  (define (hacp-done)
    (void))
  )
