;;=========================================================================
;;  ImlUnit Glue Code
;;=========================================================================
;;  For regression-testing purposes, we want to directly interface with
;;  the ImlUnit testing harness.  This is slightly tricky, because we want
;;  to pass ImlUnit the "source code" of each test case as well as the
;;  actual result.

(define-syntax test
  (syntax-rules ()
    [(_ sexpr)
     (%call-5l-prim 'test (value->string 'sexpr) sexpr)]))


;;=========================================================================
;;  Simple Test Cases
;;=========================================================================

(test #t)

(set-engine-var! 'foo "bar")
(test (equal? (engine-var 'foo) "bar"))

(debug-log "Completed simple tests")
(log "Completed simple tests")


;;=========================================================================
;;  TArgumentList Test Cases
;;=========================================================================

(define (test-arg-type type values)
  (define (prefix-symbol prefix sym)
    (string->symbol (string-append prefix (symbol->string sym))))
  (let loop ((i 0) (values values))
    (unless (null? values)
      (%call-5l-prim (prefix-symbol "set_wanted_" type) i)
      (%call-5l-prim (prefix-symbol "test_check_" type) (car values))
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
  (let ((var-name (string->symbol (cat "seen-" card-name))))
    (set-engine-var! var-name "1")))

(card start
  (mark-card-as-seen "start")
  (jump test-1))

(card test-1
  (mark-card-as-seen "test-1")
  (jump test-2))

(card test-2
  (mark-card-as-seen "test-2")
  (exit-script))
