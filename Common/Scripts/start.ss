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

(define-engine-variable foo foo "")

(set! foo "bar")
(test (equal? foo "bar"))

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
;;  Basic Language Test Cases
;;=========================================================================

(define (mark-card-as-seen card-name)
  (debug-log (cat "Marking " card-name))
  (set! (engine-var (cat "seen-" card-name)) "1"))

(card start
  (mark-card-as-seen "start")
  (jump test-1))

(card test-1
  (mark-card-as-seen "test-1")
  (jump test-2))

(card test-2
  (mark-card-as-seen "test-2")
  (jump test-variables))

(define/p *vartest* #f)

(card test-variables
  (foreach [val (list (void) "str" 'sym -2147483648 2147483647 4294967295
                      -1 0 1 -1.0 0.0 1.0 #f #t)]
    (set! *vartest* val)
    (test (equal? *vartest* val)))
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
  (jump test-callback-args))

(card test-callback-args
  (define (f h w l)
    (test (equal? h "hello"))
    (test (equal? w 'world))
    (test (equal? l (list "foo" 'bar))))
  (call-5l-prim 'testcallbackargs f)
  (jump test-stop))

(card test-stop
  (call-5l-prim 'teststop (card-name test-pause))
  (test #f))

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
  (jump advanced-language-test-cases))


;;=========================================================================
;;  Advanced Language Test Cases
;;=========================================================================

(card advanced-language-test-cases

  ;; Test (define ...).
  (define x #f)
  (test (eq? x #f))
  (set! x 10)
  (test (eq? x 10))
  (define y 20)
  (test (eq? (+ x y) 30))

  ;; Test variable interpolation.
  (define bar 3)
  (test (equal? "foo $bar" "foo 3"))
  (test (equal? "foo${bar}baz" "foo3baz"))
  (test (equal? "foo$(+ bar 2)baz" "foo5baz"))
  (test (equal? (string-length "$$") 1))

  ;; Test keywords.
  (test (eq? :foo ':foo))

  ;; Generalized setters.
  (define test-list (list 1 2 3))
  (set! (car test-list) 0)
  (test (equal? '(0 2 3) test-list))

  ;; Magic variables (and more define tests...).
  (define-values [x-real x-shadow] (values 10 20))
  (define (magic dummy)
    (test (eq? (* 2 x-real) x-shadow))
    x-real)
  (define (set-magic! dummy val)
    (set! x-real val)
    (set! x-shadow (* 2 val)))
  (let []
    (define-symbol-macro magic-x (magic "whatever"))
    (test (eq? magic-x 10))
    (set! magic-x 30)
    (test (eq? magic-x 30)))

  (jump argument-lists))


;;=========================================================================
;;  Argument Lists
;;=========================================================================

;; Traditional functions.
(define (t1) 'ok)
(define (t2 . x) x)
(define (t3 x . y) y)
(define (t4 x y) y)

;; Optional keywords.
(define (o1 &opt x) x)
(define (o2 x &opt (y 1) (z (* y 2))) (list y z))

;; Rest arguments.
(define (r1 &rest x) x)
(define (r2 x &rest y) y)

;; Keyword arguments.
(define (k1 &key x y) (list x y))
(define (k2 &key (x 'foo) (y 'bar)) (list x y))
(define (k3 &key (x1 :x 'foo) (y1 :y 'bar)) (list x1 y1))
(define (k4 n &key (x n) (y (* 2 n))) (list x y))
(define (k5 &key x &rest y) (list x y))

(card argument-lists
  (test (eq? (t1) 'ok))
  (test (equal? (t2 1 2 3) '(1 2 3)))
  (test (equal? (t3 1 2 3) '(2 3)))
  (test (equal? (t3 1 2) '(2)))
  (test (eq? (o1) #f))
  (test (eq? (o1 2) 2))
  (test (equal? (o2 0) '(1 2)))
  (test (equal? (o2 0 2) '(2 4)))
  (test (equal? (o2 0 2 3) '(2 3)))
  (test (equal? (r1 1 2 3) '(1 2 3)))
  (test (equal? (r2 1 2 3) '(2 3)))
  (test (equal? (k1) '(#f #f)))
  (test (equal? (k1 :x 1) '(1 #f)))
  (test (equal? (k1 :y 2) '(#f 2)))
  (test (equal? (k1 :y 2 :x 1) '(1 2)))
  (test (equal? (k2) '(foo bar)))
  (test (equal? (k2 :x 'baz) '(baz bar)))
  (test (equal? (k2 :y 'baz) '(foo baz)))
  (test (equal? (k2 :x 'baz :y 'moby) '(baz moby)))
  (test (equal? (k3 :x 'baz) '(baz bar)))
  (test (equal? (k3 :y 'baz) '(foo baz)))
  (test (equal? (k3 :x 'baz :y 'moby) '(baz moby)))
  (test (equal? (k4 1) '(1 2)))
  (test (equal? (k4 1 :x 2) '(2 2)))
  (test (equal? (k4 1 :y 3) '(1 3)))
  (test (equal? (k4 1 :x 2 :y 3) '(2 3)))
  (test (equal? (k5) '(#f ())))
  (test (equal? (k5 :x 2) '(2 (:x 2))))
  (test (equal? (k5 :y 1 :x 2) '(2 (:y 1 :x 2))))
  (jump syntax-tests))


;;=========================================================================
;;  Syntax
;;=========================================================================

(card syntax-tests

  (test (eq? ((fn (x) (* x x)) 3) 9))
  (test (eq? ((callback 1)) 1))

  (jump done))

(card done
  (exit-script))

