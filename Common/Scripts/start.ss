;;=========================================================================
;;  ImlUnit Glue Code
;;=========================================================================
;;  For regression-testing purposes, we want to directly interface with
;;  the ImlUnit testing harness.  This is slightly tricky, because we want
;;  to pass ImlUnit the "source code" of each test case as well as the
;;  actual result.

(define-syntax test
  (syntax-rules ()
    [(test sexpr)
     (call-5l-prim 'test (value->string 'sexpr) sexpr)]))


;;=========================================================================
;;  Simple Test Cases
;;=========================================================================

(test #t)

(set-engine-var! 'foo "bar")
(test (equal? (engine-var 'foo) "bar"))

(debug-log "Completed simple tests")
(5l-log "Completed simple tests")


;;=========================================================================
;;  TArgumentList Test Cases
;;=========================================================================

(define (test-arg-type type values)
  (define (prefix-symbol prefix sym)
    (string->symbol (cat prefix sym)))
  (let loop [[i 0] [values values]]
    (unless (null? values)
      (call-5l-prim (prefix-symbol "set_wanted_" type) i)
      (call-5l-prim (prefix-symbol "test_check_" type) (car values))
      (loop (+ i 1) (cdr values)))))

(test-arg-type 'string '("" "hello"))
(test-arg-type 'int32 '(-2147483648 0 2147483647))
(test-arg-type 'uint32 '(0 1 4294967295))
(test-arg-type 'bool '(#t #f))
(test-arg-type 'bool '(0 #f))
(test-arg-type 'double '(-1.0 0.0 1.0))
(test-arg-type 'double '(-1 0 1))
(test-arg-type 'TPoint (list (point 1 2)))
(test-arg-type 'TRect (list (rect 1 2 3 4)))
(test-arg-type 'Color (list (color 1 2 3 4)))


;;=========================================================================
;;  Language Test Cases
;;=========================================================================

(define (mark-card-as-seen card-name)
  (debug-log (cat "Marking " card-name))
  (set-engine-var! (cat "seen-" card-name) "1"))

(card start
  (mark-card-as-seen "start")
  (jump test-1))

(card test-1
  (mark-card-as-seen "test-1")
  (jump test-2))

(card test-2
  (mark-card-as-seen "test-2")
  (jump test-callbacks))

(define *before-callback-flag* #f)
(define *after-callback-flag* #f)

(define (test-callback code)
  (call-5l-prim 'testcallback code))

(card test-callbacks
  ;; Test a simple callback.
  (define callback-ran? #f)
  (test-callback (callback (set! callback-ran? #t)))
  (test callback-ran?)

  ;; Test a jumping callback.
  (set! *before-callback-flag* #f)
  (set! *after-callback-flag* #f)
  (test-callback (callback
		   (set! *before-callback-flag* #t)
		   (jump test-callbacks-2)
		   (set! *after-callback-flag* #t))))

(card test-callbacks-2
  (test (eq? *before-callback-flag* #t))
  (test (eq? *after-callback-flag* #f))
  (jump test-pause))

(card test-pause
  (call-5l-prim 'testpause)
  (jump test-timeout))

(define *timeout-start* #f)

(card test-timeout
  (set! *timeout-start* (current-milliseconds))
  (call-5l-prim 'testtimeout 1 'timeout-done))

(card timeout-done
  (test (>= (current-milliseconds) (+ *timeout-start* 1000)))
  (jump test-nap))

(card test-nap
  (set! *timeout-start* (current-milliseconds))
  (call-5l-prim 'testnap 2)
  (test (>= (current-milliseconds) (+ *timeout-start* 200)))
  (jump done))

(card done
  (exit-script))
