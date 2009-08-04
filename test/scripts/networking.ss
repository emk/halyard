;; Various cards that only work when we have a network.  For best results,
;; run tools/http-test-server.rb before experimenting with the cards in
;; this section.
(module networking (lib "halyard.ss" "halyard")
  (require (lib "halyard-unit.ss" "halyard"))
  (require (lib "test-elements.ss" "halyard"))
  (require (lib "url-request.ss" "halyard"))
  (require (lib "hacp.ss" "halyard"))
  (require (lib "data-file.ss" "halyard"))
  (require (lib "uuid.ss" "halyard"))
  (require (lib "base.ss" "halyard-test"))

  (group /networking (%card-group% :skip-when-jumping-to-each-card? #t))


  ;;=======================================================================
  ;;  Demo cards
  ;;=======================================================================
  
  (card /networking/http (%standard-test-card% :title "Asynchronous HTTP")
    (edit-box result
        ((inset-rect $screen-rect 50)
         ""
         :multiline? #t
         :font-size 12))

    (text status ((below (.result) 10) $caption-style "Downloading"))

    (elem progress-bar (%progress-bar% :bounds (rect 600 580 750 600)))

    (elem req (%url-request% :url "http://iml.dartmouth.edu/halyard")
      (def (display text)
        (set! (.parent.result.text)
              (string-append (.parent.result.text) text)))

      (def (status text)
        (set! (.parent.status.text) text))

      (def (progress-changed event)
        (.parent.progress-bar.progress-changed event))

      (def (data-received event)
        (.display (regexp-replace* "\r\n" (event .data) "\n")))

      (def (transfer-finished event)
        (.status (cat (if (event .succeeded?)
                          "Transfer succeeded"
                          "Transfer failed")
                      ": " (event .message))))
      )
    )


  ;;=======================================================================
  ;;  Unit tests
  ;;=======================================================================

  (group /networking/tests)

  ;; The address of Sinatra-based test server.  This is implemented by
  ;; tools/http-test-server.rb, which must be run manually.
  (define $server "http://localhost:4567")


  ;;=======================================================================
  ;;  percent-encode unit tests
  ;;=======================================================================

  (define-class %percent-encode-test% (%test-case%)
    (test "percent-encode should escape everything but unreserved characters"
      (assert-equals "abc%26%3d%20123_-.~" (percent-encode "abc&= 123_-.~")))
    (test "percent-encode-parameters should encode an assoc of URL parameters"
      (assert-equals "foo=bar&escaped=%26%3d%20q"
                     (percent-encode-parameters '(("foo" . "bar")
                                                  ("escaped" . "&= q"))))))


  ;;=======================================================================
  ;;  %url-request% unit tests
  ;;=======================================================================

  (define-class %url-request-test-case% (%element-test-case%)
    (def (perform request)
      (request .wait)
      (define result (request .response))
      (delete-element request)
      result)
    (def (get url &rest keys)
      (.perform
       (apply send %easy-url-request% 'new
              :url (cat $server url)
              keys)))
    (def (post url content-type body &rest keys)
      (.perform
       (apply send %easy-url-request% 'new
              :url (cat $server url)
              :method 'post
              :content-type content-type
              :body body
              keys)))
    )

  (define-class %test-server-present-test% (%url-request-test-case%)
    (test "tools/http-test-server.rb should be running"
      (assert-equals "Hello!\n" (.get "/hello"))))

  (define-class %http-get-test% (%url-request-test-case%)
    (test "GET should return the response body"
      (assert-equals "Hello!\n" (.get "/hello")))

    (test "GET should support large responses"
      (define (build-hello count)
        ;; Repeat "Hello!\n" COUNT times.  Only works for powers of 2.
        (if (= 1 count)
          "Hello!\n"
          (let [[half (build-hello (/ count 2))]]
            (string-append half half))))
      (assert-equals (build-hello 1024) (.get "/hello?count=1024")))

    (test "GET should fail if it encounters an HTTP error"
      (assert-raises exn:fail? (.get "/not-found")))

    (test "GET should provide a correct Content-Type"
      (define request
        (%easy-url-request% .new :url (cat $server "/hello")))
      (request .wait)
      (assert-equals "text/html" (request .response-content-type)))

    (test "GET should allow sending custom Accept headers"
      (assert-equals "text/x-foo"
                     (.get "/headers/Accept" :accept "text/x-foo")))
    )

  (define-class %http-post-test% (%url-request-test-case%)
    (test "POST should upload data and return the response body"
      (assert-equals "post: Hello" (.post "/upload" "text/plain" "Hello")))
    )

  (define-class %http-post-form-test% (%url-request-test-case%)
    (test "Parameters should be sent as application/x-www-form-urlencoded"
      (define request (%easy-url-request% .new
                        :url (cat $server "/echo")
                        :method 'post
                        :parameters '(("foo" . "bar")
                                      ("escaped" . "&= q"))))
      (request .wait)
      (assert-equals "application/x-www-form-urlencoded"
                     (request .response-content-type))
      (assert-equals "foo=bar&escaped=%26%3d%20q" (request .response))))

  (define-class %json-request-test% (%url-request-test-case%)
    (test "JSON GET requests should work"
      (define request
        (%json-request% .new :url (cat $server "/add")
                             :parameters '(("x" . "1") ("y" . "2"))))
      (request .wait)
      (assert-equals 3 (request .response)))

    (test "JSON POST requests should work"
      (define request (%json-request% .new
                        :url (cat $server "/echo")
                        :method 'post
                        :data '(1 2 3)))
      (request .wait)
      (assert-equals '(1 2 3) (request .response)))

    (test "Failed JSON requests should raise an error"
      (define request (%json-request% .new :url (cat $server "/not-found")))
      (request .wait)
      (assert-raises exn:fail? (request .response)))
    )

  (card /networking/tests/url-request
      (%test-suite%
       :tests (list %percent-encode-test%
                    %test-server-present-test%
                    %http-get-test%
                    %http-post-test%
                    %http-post-form-test%
                    %json-request-test%)))


  ;;=======================================================================
  ;;  HACP unit tests
  ;;=======================================================================

  (define $hacp-url (cat $server "/hacp"))

  (define $student-uuid "44463f20-b4c6-4a3e-abf6-b942d010deb3")
  (define $student-name "J. Student")
  (define $student-id   "12345")

  (define $hacp-session-id (cat $student-uuid ":123:4567"))

  (define-class %hacp-low-level-test% (%element-test-case%)
    (test "hacp-extension-register-user-request should register user"
      (define request
        (hacp-extension-register-user-request $hacp-url $student-uuid
                                              $student-name $student-id))
      (request .wait)
      (assert (hash-table? (request .response))))

    (test "hacp-extension-new-session-request should return HACP session info"
      (define request
        (hacp-extension-new-session-request $hacp-url $student-uuid))
      (request .wait)
      (assert-equals $hacp-url (hash-table-get (request .response) "aicc_url"))
      (assert (regexp-match (pregexp (cat $student-uuid ":\\d+:\\d+"))
                            (hash-table-get (request .response) "aicc_sid"))))

    (test "hacp-get-param-request should issue an HACP GetParam request"
      (define request (hacp-get-param-request $hacp-url $hacp-session-id))
      (request .wait)
      ;; TODO - Actually parse GetParam result.  Someday.  For now, the
      ;; (.result) value is undefined.
      (assert (request .succeeded?)))

    (test "hacp-put-param-request should write current state to HACP server"
      (define request
        (hacp-put-param-request $hacp-url $hacp-session-id
                                '(("Lesson_Location" . "/start")
                                  ("Lesson_Status" . "incomplete")
                                  ("Score" . "72,100")
                                  ("Time" . "00:05:00")
                                  ("J_ID.1" . "/part1")
                                  ("J_Status.1" . "completed")
                                  ("J_ID.2" . "/part2")
                                  ("J_Status.2" . "incomplete"))
                                "data\n[foo]"))
      (request .wait)
      (assert (request .succeeded?)))

    )

  (define-class %hacp-fields-test% (%test-case%)
    (setup-test
      (hacp-clear-fields!))

    (test "valid-hacp-status? should return true for all valid statuses"
      (foreach [status '(passed completed failed incomplete browsed
                                not-attempted)]
        (assert (valid-hacp-status? status))))
    (test "valid-hacp-status? should return false for other values"
      (foreach [status '(not-valid "not valid" 1)]
        (assert (not (valid-hacp-status? status)))))

    (test "hacp-field and set-hacp-field! should get and set field values"
      (assert-equals #f (hacp-field "foo"))
      (set! (hacp-field "foo") "bar")
      (assert-equals "bar" (hacp-field "foo")))
    (test "hacp-clear-fields! should clear all field values"
      (set! (hacp-field "foo") "bar")
      (hacp-clear-fields!)
      (assert-equals #f (hacp-field "foo")))
    (test "hacp-field should treat field names as case insensitive"
      (set! (hacp-field "FOO") "bar")
      (assert-equals "bar" (hacp-field "foo")))
    (test "set-hacp-field! should clear the field when passed #f"
      (set! (hacp-field "foo") "bar")
      (set! (hacp-field "foo") #f)
      (assert-equals #f (hacp-field "foo")))
    (test "set-hacp-field! should require keys to be strings"
      (assert-raises exn:fail? (set! (hacp-field 'foo) "bar")))
    (test "set-hacp-field! should not allow values besides strings and #f"
      (assert-raises exn:fail? (set! (hacp-field "foo") 'bar)))

    (test "set-hacp-lesson-status! should save status symbols as strings"
      (set! (hacp-lesson-status) 'incomplete)
      (assert-equals "incomplete" (hacp-field "Lesson_Status"))
      (set! (hacp-lesson-status) 'not-attempted)
      (assert-equals "not attempted" (hacp-field "Lesson_Status")))
    (test "set-hacp-status! should raise an error if status is invalid"
      (assert-raises exn:fail? (set! (hacp-lesson-status) 'not-valid)))

    (test "set-hacp-objective-status! should set objective fields"
      (set! (hacp-objective-status '/section1) 'passed)
      (set! (hacp-objective-status '/section2) 'not-attempted)
      (assert-equals "/section1"     (hacp-field "J_ID.1"))
      (assert-equals "passed"        (hacp-field "J_Status.1"))
      (assert-equals "/section2"     (hacp-field "J_ID.2"))
      (assert-equals "not attempted" (hacp-field "J_Status.2"))
      (set! (hacp-objective-status '/section2) 'incomplete)
      (assert-equals "incomplete"    (hacp-field "J_Status.2")))

    )

  (define-class %hacp-high-level-test% (%element-test-case%)
    (setup-test
      ;; Reset our HACP request log on the server.
      (define request
        (%easy-url-request% .new
          :url (cat $server "/hacp2/log/reset")
          :method 'post :parameters '()))
      (request .wait)
      (assert (request .succeeded?))
      (delete-element request))

    (def (hacp-log)
      ;; Fetch the contents of our HACP request log.
      (define request
        (%easy-url-request% .new :url (cat $server "/hacp2/log")))
      (request .wait)
      (request .response))

    (test "hacp-initialize should assign a UUID if the user doesn't have one"
      (with-temporary-user-data ()
        (assert (not (uuid? (user-pref 'uuid))))
        (hacp-initialize (cat $server "/hacp2") $student-name)
        (assert (uuid? (user-pref 'uuid)))
        (hacp-done)))

    (test "High-level HACP API should write user data to server in background"
      (with-temporary-user-data ()
        ;; Use a known UUID for convenience.
        (set! (user-pref 'uuid) $student-uuid)

        (hacp-initialize (cat $server "/hacp2") $student-name)
        (hacp-write :sync? #f)
        (hacp-done)

        (assert-equals "register new_session GetParam PutParam PutParam"
                       (.hacp-log))))

    (test "hacp-initialize should not re-register known users"
      (with-temporary-user-data ()
        ;; Use a known UUID for convenience and mark as registered.
        (set! (user-pref 'uuid) $student-uuid)
        (set! (user-pref '*hacp-user-registered?*) #t)

        (hacp-initialize (cat $server "/hacp2") $student-name)
        (hacp-done)
        
        (assert-equals "new_session GetParam PutParam" (.hacp-log))))
    )

  (card /networking/tests/hacp
      (%test-suite%
       :tests (list %hacp-low-level-test%
                    %hacp-fields-test%
                    %hacp-high-level-test%)))

  )
