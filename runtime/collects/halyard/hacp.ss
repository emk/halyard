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
  ;;  High-level HACP API
  ;;=======================================================================

  (provide valid-hacp-status?)

  ;;; Is 'value' a valid HACP status value?  This is mostly used for
  ;;; internal type-checking.
  (define (valid-hacp-status? value)
    (and (memq value '(passed completed failed incomplete browsed
                       not-attempted))
         #t))

  )
