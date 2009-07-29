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

(module url-request (lib "halyard.ss" "halyard")
  (require (lib "kernel.ss" "halyard/private"))
  (require (lib "events.ss" "halyard/private"))
  (require (lib "json.ss" "json-scheme"))


  ;;=======================================================================
  ;;  Helper Routines
  ;;=======================================================================

  (provide encode-url-parameters)

  ;; Encode an assoc mapping parameter names strings to parameter values
  ;; as type application/x-www-form-urlencoded.  For now, parameter values
  ;; must be strings.
  (define (encode-url-parameters parameters)
    (call-prim 'UrlRequestEncodeUrlParameters
               ;; Flatten parameters into a list.
               (let recurse [[parameters parameters]]
                 (if (null? parameters)
                     '()
                     (cons (car (car parameters))
                           (cons (cdr (car parameters))
                                 (recurse (cdr parameters))))))))


  ;;=======================================================================
  ;;  %url-request%
  ;;=======================================================================

  (provide %url-request%)

  ;;; An asynchronous request to download (or upload to) a URL.  This
  ;;; is actually implemented using libcurl, so it should support (perhaps
  ;;; with a bit of tweaking of the C++ code) a wide variety of protocols.
  (define-class %url-request% (%invisible-element%)
    ;;; The URL to download (or upload to).
    (attr url :type <string>)

    ;;; The HTTP method to use.  For other protocols, such as FTP, use the
    ;;; analogous HTTP method: FTP download -> GET, FTP upload -> PUT, etc.
    ;;; Supported values are 'get and 'post.
    (attr method 'get :type <symbol>)

    ;;; The HTTP 'Accept:' header to use with the request.  Pass #f to use
    ;;; a reasonable default value.
    (attr accept #f)

    ;;; The MIME type to use for the request body, if any.
    (attr content-type #f)

    ;;; The data to send in the request body, if any.
    (attr body #f)

    ;;; The MIME Content-Type of the response.  This will return #f if the
    ;;; content type is not yet known.
    (def (response-content-type)
      (call-prim 'UrlRequestGetResponseContentType (.full-name)))

    ;;; Called when we need to update our progress bar, if we have one.
    (def (progress-changed event)
      (void))

    ;;; Called when a new chunk of data is received from the server.  The
    ;;; data is provided as (event .data).
    (def (data-received event)
      (void))

    ;;; Called when the transfer is done.  To discover what happened, call
    ;;; (event .succeeded?) and (event .message?).
    (def (transfer-finished event)
      (void))

    ;; Internal.
    (def (create-engine-node)
      (call-prim 'UrlRequest (.full-name)
                 (make-node-event-dispatcher self)
                 (.url))
      (when (.accept)
        (call-prim 'UrlRequestConfigureSetHeader (.full-name)
                   "Accept" (.accept)))
      (case (.method)
        [[get] (void)]
        [[post]
         (call-prim 'UrlRequestConfigurePost (.full-name)
                    (.content-type) (.body))]
        [else
         (error (cat self ": Unknown request method " (.method)))])
      (call-prim 'UrlRequestStart (.full-name))
      )
    )


  ;;=======================================================================
  ;;  %easy-url-request%
  ;;=======================================================================

  (provide %easy-url-request%)

  ;;; This subclass of %url-request% collects the downloaded data
  ;;; internally, and generally tries to provide a higher-level interface
  ;;; for downloading URLs.  If you need to be notified when the transfer
  ;;; is finished, override .transfer-finished and call (super) before
  ;;; running your own code.
  (define-class %easy-url-request% (%url-request%)
    ;;; An optional alist of HTTP request parameters, mapping attribute
    ;;; name strings to value strings.  This will be passed as ordinary URL
    ;;; parameters for a GET request, and as an HTTP body of type
    ;;; application/x-www-form-urlencoded for a POST request.
    (attr parameters #f)

    (def (initialize &rest keys)
      (super)
      (set! (slot 'finished?) #f)
      (set! (slot 'succeeded?) #f)
      (set! (slot 'response-body-chunks) '())
      (set! (slot 'response-body) #f)
      (set! (slot 'transfer-finished-event) #f)

      ;; If we have parameters, figure out how to pass them.
      (when (.parameters)
        (let [[encoded (encode-url-parameters (.parameters))]]
          (case (.method)
            [[get]
             (when (regexp-match (pregexp "\\?") (.url))
               (error (cat "Cannot add more parameters to " (.url))))
             (set! (.url) (cat (.url) "?" encoded))]
            [[post]
             (when (.body)
               (error (cat "Already have a body to POST to " (.url))))
             (set! (.content-type) "application/x-www-form-urlencoded")
             (set! (.body) encoded)]
          [else
           (error (cat "Do not know how to pass URL parameters to HTTP method "
                       (.method)))]))))
      
    (def (data-received event)
      (set! (slot 'response-body-chunks)
            (cons (event .data) (slot 'response-body-chunks))))

    (def (transfer-finished event)
      (set! (slot 'transfer-finished-event) event)
      (set! (slot 'finished?) #t)
      (set! (slot 'succeeded?) (event .succeeded?))
      (when (.succeeded?)
        (set! (slot 'response-body)
              (apply string-append (reverse (slot 'response-body-chunks)))))
      (set! (slot 'response-body-chunks) '()))

    ;;; Has this request finished?
    (def (finished?)
      (slot 'finished?))

    ;;; Was this request successful?
    (def (succeeded?)
      (slot 'succeeded?))

    ;;; Return the response body.  If the transfer failed, or is still
    ;;; running, returns #f.
    (def (response-body)
      (slot 'response-body))

    ;;; Either return the response body in an appropriate format, or raise
    ;;; an error if the transfer failed (or if we're still waiting for it
    ;;; to finish).
    (def (response)
      (cond
       [(not (.finished?))
        (error (cat "Request to " (.url) " not finished"))]
       [(not (.succeeded?))
        (error (cat "Request error: "
                    ((slot 'transfer-finished-event) .message)))]
       [else
        (.parse-response)]))

    ;;; Wait until this response is completed.
    (def (wait)
      (while (not (.finished?))
        (idle)))

    ;;; "Protected" method (for overriding by subclasses only).  If you
    ;;; want to convert (.response-body) to a different format, override
    ;;; this method.  You may also want to check (.response-content-type).
    (def (parse-response)
      (.response-body))

    )


  ;;=======================================================================
  ;;  %json-request%
  ;;=======================================================================

  (provide %json-request%)

  ;;; An HTTP request receiving (and perhaps sending) JSON (JavaScript
  ;;; Object Notation), a common format for serializing data structures in
  ;;; web applications.
  (define-class %json-request% (%easy-url-request%)
    ;;; The data to send in a JSON POST request (optional).
    (attr data :mandatory? #f)

    (default accept "application/json")

    ;; If :data was passed, then set up :content-type and :body for a JSON
    ;; request.  Otherwise, assume only the response uses JSON.  The
    ;; rarely-used ':skip-if-missing-values? #t' argument will cause these
    ;; defaults to be ignored if :data was not passed.
    (default content-type
      ;; Make sure we try to read the value (.data).
      (begin (.data) "application/json")
      :skip-if-missing-values? #t)
    (default body
      (with-output-to-string
        (lambda () (json-write (.data))))
      :skip-if-missing-values? #t)

    (def (parse-response)
      (json-read (open-input-string (.response-body))))
    )


  )