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

(module json-request (lib "halyard.ss" "halyard")
  (require (lib "url-request.ss" "halyard"))
  (require (lib "json.ss" "json-scheme"))

  (provide %json-request%)

  ;;; An HTTP request receiving (and perhaps sending) JSON (JavaScript
  ;;; Object Notation), a common format for serializing data structures in
  ;;; web applications.
  (define-class %json-request% (%easy-url-request%)
    ;;; The data to send in a JSON POST request (optional).
    (attr data #f)

    (default accept "application/json")
    (default content-type "application/json")
    (default body
      (with-output-to-string
        (lambda () (json-write (.data)))))

    (def (transfer-finished event)
      (super)
      (set! (slot 'message) (event .message)))

    ;;; Either return the result of the JSON request, or raise an error if
    ;;; the transfer failed (or if we're still waiting for it to finish).
    (def (response)
      (cond
       [(not (.finished?))
        (error (cat "JSON request to " (.url) " not finished"))]
       [(not (.succeeded?))
        (error (cat "JSON error: " (slot 'message)))]
       [else
        (json-read (open-input-string (.response-body)))]))
    )

  )
