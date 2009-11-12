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
                                                student-id
                                                &rest keys)
    (apply send %json-request% 'new
      :url (cat hacp-url "/register")
      :method 'post
      :parameters (list (cons "uuid" uuid)
                        (cons "name" student-name)
                        (cons "student_id" student-id))
      keys))

  (define (hacp-extension-new-session-request hacp-url uuid &rest keys)
    (apply send %json-request% 'new
      :url (cat hacp-url "/new_session")
      :method 'post
      :parameters (list (cons "uuid" uuid))
      keys))

  ;; Get our HACP data from the server.  For now, we just throw that
  ;; data away without parsing it.
  ;;
  ;; WARNING: The (.result) member of the newly created request is expected
  ;; to change to some sort of parsed representation in the future.
  (define (hacp-get-param-request hacp-url session-id &rest keys)
    (apply send %easy-url-request% 'new
      :url hacp-url
      :method 'post
      :parameters (list (cons "command" "getparam")
                        (cons "version" "4.0")
                        (cons "session_id" session-id))
      keys))

  (define (hacp-put-param-request hacp-url session-id key-val data &rest keys)
    ;; Note that we do not percent-encode the "~" as required by the HACP
    ;; specification, because modern standards like RFC 3986 consider it an
    ;; "unreserved character" that should generally not be escaped when
    ;; encoding data in URLs.  Since HACP claims to use URL-encoding, we'll
    ;; just live dangerously and obey the current standards.
    (define core (regexp-replace* "&" (percent-encode-parameters key-val) "\n"))
    (define aicc-data
      (string-append "[Core]\n" core "\n"
                     "[Core_Lesson]\n" (percent-encode data) "\n"))
    (trace 'halyard.hacp.putparam "Sending HACP PutParam to " hacp-url
           " with the data:\n" aicc-data)
    (apply send %easy-url-request% 'new
      :url hacp-url
      :method 'post
      :parameters (list (cons "command" "putparam")
                        (cons "version" "4.0")
                        (cons "session_id" session-id)
                        (cons "aicc_data" aicc-data))
      keys))


  ;;=======================================================================
  ;;  HACP Fields
  ;;=======================================================================
  ;;  These functions are used in conjunction with the high-level API
  ;;  below.

  (provide valid-hacp-status? hacp-clear-fields! hacp-field set-hacp-field!
           set-hacp-lesson-status! set-hacp-objective-status!)

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
  (define (set-hacp-lesson-status! value)
    (set! (hacp-field "Lesson_Status") (hacp-status->string value)))

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

  (provide hacp-client-initialized? hacp-initialize hacp-write hacp-done
           ;; These are slightly lower-level functions which you can
           ;; generally ignore.  They probably don't work the way you
           ;; expect them to, and they're really only needed for test
           ;; suites.
           hacp-client-state hacp-client-encountered-error? hacp-client-wait)

  (define *hacp-client* #f)

  ;;; Returns true iff we have an HACP client at the moment.
  (define (hacp-client-initialized?)
    ;; Coerce to boolean.
    (and *hacp-client* #t))

  ;;; Initialize an HACP session.
  (define (hacp-initialize hacp-url student-name)
    (set! *hacp-client*
          (%hacp-client% .new
            :parent (running-root-node) :name 'hacp-client
            :url hacp-url :student-name student-name)))

  ;;; Return the current state of our HACP client.  This function is
  ;;; mostly used for unit tests.
  (define (hacp-client-state)
    (*hacp-client* .state))

  ;;; Report whether the most recent HACP command ecountered an error.
  (define (hacp-client-encountered-error?)
    (*hacp-client* .error?))

  ;;; Block until either all outstanding requests have been processed, or
  ;;; we have encountered an error.
  (define (hacp-client-wait)
    (*hacp-client* .wait))

  ;;; Attempt to write our data to the server.  If sync? if #f, then
  ;;; perform the write in the background.  If sync? is #t, wait for the
  ;;; write to succeed or fail.
  ;;;
  ;;; If hacp-initialize was never called, this function does nothing.
  (define (hacp-write &key sync?)
    (when (hacp-client-initialized?)
      (*hacp-client* .write)
      (when sync?
        (hacp-client-wait))))

  ;;; Terminate our HACP session, if one is running, and attempt to flush
  ;;; our data to the server synchronously.
  ;;;
  ;;; If hacp-initialize was never called, this function does nothing.
  (define (hacp-done)
    (when (hacp-client-initialized?)
      (hacp-write :sync? #t)
      (delete-element *hacp-client*)
      (set! *hacp-client* #f)))

  ;;; Used internally to implement the high-level HACP client.
  (define-class %hacp-client% (%invisible-element%)
    ;;; The URL of our HACP server.  May be updated at the request of the
    ;;; remote server once the HACP session has been initialized.
    (attr url :type <string> :writable? #t)

    ;;; The full name of the student.  This is only needed for initial
    ;;; registration.
    (attr student-name :type <string>)

    ;; Our HACP session ID, as provided by the server.
    (attr session-id #f :writable? #t)

    ;; The current state of our client.
    (attr state #f :writable? #t)

    ;; Did our last HACP request encounter some sort of error?
    (attr error? #f :writable? #t)

    ;; If this field contains a string, write it to the server as
    ;; soon as possible.
    (attr data-to-write #f :writable? #t)

    ;; Initialize our HACP client and try to connect to the server.
    (run
      ;; If the current user doesn't already have a UUID, assign one.
      (unless (user-pref 'uuid)
        (set! (user-pref 'uuid) (uuid)))

      ;; Is the current user already registered with our HACP server?
      (set! (.state)
            (if (user-pref '*hacp-user-registered?* :default #f)
                'registered
                'unregistered))

      ;; Start our state machine running.
      (.%start-next-request-if-appropriate))

    ;; Do we have a request running right now?
    (def (%request-is-running?)
      (element-exists? 'request :parent self))

    ;; If we should try to run a new request now, do it.
    (def (%start-next-request-if-appropriate)
      ;; We should not be called when this HACP client is in an error state.
      (assert (not (.error?)))
      ;; We should not called when a request object exists.
      (assert (not (.%request-is-running?)))

      ;; Build a function to handle our response.
      (define (make-handler method-name next-state)
        (fn (request)
          (if (request .succeeded?)
            ;; Update our state and call the appropriate handler.
            (begin
              (set! (.state) next-state)
              (send self method-name request)
              (delete-element request)
              (.%start-next-request-if-appropriate))
            ;; Mark ourselves as having encountered an error.
            (begin
              (delete-element request)
              (set! (.error?) #t)))))

      ;; Figure out what request we should make now.
      (case (.state)
        [[unregistered]
         (hacp-extension-register-user-request
          (.url) (user-pref 'uuid) (.student-name)
          (regexp-replace* "-" (user-pref 'uuid) "")
          :parent self :name 'request
          :on-transfer-finished (make-handler '%registered 'registered))]
        [[registered]
         (hacp-extension-new-session-request
          (.url) (user-pref 'uuid)
          :parent self :name 'request
          :on-transfer-finished
          (make-handler '%session-started 'session-started))]
        [[session-started]
         (hacp-get-param-request
          (.url) (.session-id)
          :parent self :name 'request
          :on-transfer-finished (make-handler '%connected 'connected))]
        [[connected]
         ;; Normally, we don't need to do anything here, but if we have
         ;; some data to write to the server, we should start doing so
         ;; now.
         (when (.data-to-write)
           (let [[data (.data-to-write)]]
             (set! (.data-to-write) #f)
             (hacp-put-param-request
              (.url) (.session-id) data ""
              :parent self :name 'request
              :on-transfer-finished (make-handler '%connected 'connected))))]))

    (def (%registered request)
      ;; Mark this user as registered.
      (set! (user-pref '*hacp-user-registered?*) #t))

    (def (%session-started request)
      ;; Store our session information.
      (define session-info (request .response))
      (set! (.url) (hash-table-get session-info "aicc_url"))
      (set! (.session-id) (hash-table-get session-info "aicc_sid")))

    (def (%connected request)
      ;; We don't need to anything special once we're connected.
      (void))

    ;;; Attempt to write our current HACP field state to our server.
    (def (write)
      ;; Finish filling in our HACP fields.
      ;; TODO - We may want to offer an override for this so that we
      ;; can exclude "are you sure you want to exit the program?"
      ;; cards.
      (set! (hacp-field "Lesson_Location")
            (symbol->string ((current-group-member) .full-name)))

      ;; Clear any error state.
      (when (.error?)
        (set! (.error?) #f))

      ;; Record the data we need to write.  Note that this may replace
      ;; existing data that we were hoping to write.  (We sort our HACP
      ;; fields to make it easier to write unit tests that expect certain
      ;; raw HACP messages to be received by the server.)
      (set! (.data-to-write)
        (sort (hash-table-map *hacp-fields* (fn (k v) (cons k v)))
              (fn (a b) (string<? (car a) (car b)))))
 
      ;; If we don't have a current running request, restart our state
      ;; machine.
      (unless (.%request-is-running?)
        (.%start-next-request-if-appropriate)))

    ;;; Block until we have no running requests.  This generally means that
    ;;; either all outstanding requests have been completed, or we have
    ;;; encountered a network error.
    (def (wait)
      ;; Note that (.%request-is-running?) will occasionally be false
      ;; while we're switching from one request to another, but that
      ;; should happen entirely inside of (idle), making it invisible to
      ;; us.
      (while (.%request-is-running?)
        (idle)))
    )

  )
