;; Various cards that only work when we have a network.  For best results,
;; run tools/http-test-server.rb before experimenting with the cards in
;; this section.
(module networking (lib "halyard.ss" "halyard")
  (require (lib "halyard-unit.ss" "halyard"))
  (require (lib "test-elements.ss" "halyard"))
  (require (lib "url-request.ss" "halyard"))
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
        (.status (cat (if (event .success?)
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

  (define-class %url-request-test-case% (%element-test-case%)
    (def (perform request)
      (request .wait)
      (define result (request .response-body))
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
      (assert-equals #f (.get "/not-found")))

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

  (card /networking/tests/url-request
      (%test-suite%
       :tests (list %test-server-present-test%
                    %http-get-test%
                    %http-post-test%)))

  )
