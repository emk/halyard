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

  (provide hacp-extension-register-user-request
           hacp-extension-new-session-request)

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

  )
