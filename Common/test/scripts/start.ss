;;=========================================================================
;;  ImlUnit Glue Code
;;=========================================================================
;;  For regression-testing purposes, we want to directly interface with
;;  the ImlUnit testing harness.  This is slightly tricky, because we want
;;  to pass ImlUnit the "source code" of each test case as well as the
;;  actual result.
(module start (lib "halyard.ss" "halyard")

  (define-syntax check
    (syntax-rules ()
      [(check sexpr)
       (check-with-label 'sexpr sexpr)]))
  
  (define (check-with-label label success?)
    (call-prim 'Test (value->string label) (value->boolean success?)))

  ;;=======================================================================
  ;;  Simple Test Cases
  ;;=======================================================================

  (check #t)
  
  (define-engine-variable foo foo "")

  (set! foo "bar")
  (check (equal? foo "bar"))

  (debug-log "Completed simple tests")
  (app-log "Completed simple tests")


  ;;=======================================================================
  ;;  TArgumentList Test Cases
  ;;=======================================================================

  (define (check-arg-type type values)
    (define (prefix-symbol prefix sym)
      (string->symbol (cat prefix sym)))
    (let loop [[i 0] [values values]]
      (unless (null? values)
        (call-prim (prefix-symbol "Set_Wanted_" type) i)
        (call-prim (prefix-symbol "Test_Check_" type) (car values))
        (loop (+ i 1) (cdr values)))))

  (check-arg-type 'string '("" "hello"))
  (check-arg-type 'int32 '(-2147483648 0 2147483647))
  (check-arg-type 'uint32 '(0 1 4294967295))
  (check-arg-type 'bool '(#t #f))
  (check-arg-type 'double '(-1.0 0.0 1.0))
  (check-arg-type 'double '(-1 0 1))
  (check-arg-type 'TPoint (list (point 1 2)))
  (check-arg-type 'TRect (list (rect 1 2 3 4)))
  (check-arg-type 'TPolygon (list (polygon) 
                                  (polygon (point 0 0) (point 2 0) 
                                           (point 1 2))))
  (check-arg-type 'Color (list (color 1 2 3 4)))


  ;;=======================================================================
  ;;  Basic Language Test Cases
  ;;=======================================================================

  (define (mark-card-as-seen card-name)
    (debug-log (cat "Marking " card-name))
    (set! (engine-var (cat "seen-" card-name)) "1"))

  (card /start ()
    (run
      (mark-card-as-seen "start")
      (jump /test-1)))

  (card /test-1 ()
    (run
      (mark-card-as-seen "test-1")
      (jump /test-2)))

  (card /test-2 ()
    (run
      (mark-card-as-seen "test-2")
      (jump /test-variables)))

  (define/p *vartest* #f)

  (card /test-variables ()
    (run
      (foreach [val (list (void) "str" 'sym -2147483648 2147483647 4294967295
                          -1 0 1 -1.0 0.0 1.0 #f #t (point 10 20)
                          (rect 11 21 31 41) (color 12 22 32 42))]
        (set! *vartest* val)
        (check (equals? *vartest* val)))
      (jump /test-callbacks)))

  (define *before-callback-flag* #f)
  (define *after-callback-flag* #f)

  (define (test-callback code)
    (call-prim 'TestCallback code))

  (card /test-callbacks ()
    (run
      ;; Test a simple callback.
      (define callback-ran? #f)
      (test-callback (callback (set! callback-ran? #t)))
      (check callback-ran?)
    
      ;; Test a jumping callback.
      (set! *before-callback-flag* #f)
      (set! *after-callback-flag* #f)
      (test-callback (callback
                       (set! *before-callback-flag* #t)
                       (jump /test-callbacks-2)
                       (set! *after-callback-flag* #t)))))

  (card /test-callbacks-2 ()
    (run
      (check (eq? *before-callback-flag* #t))
      (check (eq? *after-callback-flag* #f))
      (jump /test-callback-args)))
  
  (card /test-callback-args ()
    (run
      (define (f h w l)
        (check (equal? h "hello"))
        (check (equal? w 'world))
        (check (equal? l (list "foo" 'bar))))
      (call-prim 'TestCallbackArgs f)
      (jump /test-stop)))

  (card /test-stop ()
    (run
      (call-prim 'TestStop (card-name /test-pause))
      (check #f)))

  (card /test-pause ()
    (run
      (call-prim 'TestPause)
      (jump /advanced-language-test-cases)))


  ;;=======================================================================
  ;;  Advanced Language Test Cases
  ;;=======================================================================

  (card /advanced-language-test-cases ()
    (run
    
      ;; Test (define ...).
      (define x #f)
      (check (eq? x #f))
      (set! x 10)
      (check (eq? x 10))
      (define y 20)
      (check (eq? (+ x y) 30))
    
      ;; Test variable interpolation.
      ;; (define bar 3)
      ;; (check (equal? "foo $bar" "foo 3"))
      ;; (check (equal? "foo${bar}baz" "foo3baz"))
      ;; (check (equal? "foo$(+ bar 2)baz" "foo5baz"))
      ;; (check (equal? (string-length "$$") 1))
    
      ;; Test keywords.
      (check (eq? :foo ':foo))
    
      ;; Generalized setters.
      (define test-list (list 1 2 3))
      (set! (car test-list) 0)
      (check (equal? '(0 2 3) test-list))
    
      ;; Magic variables (and more define tests...).
      (define-values [x-real x-shadow] (values 10 20))
      (define (magic dummy)
        (check (eq? (* 2 x-real) x-shadow))
        x-real)
      (define (set-magic! dummy val)
        (set! x-real val)
        (set! x-shadow (* 2 val)))
      (let []
        (define-symbol-macro magic-x (magic "whatever"))
        (check (eq? magic-x 10))
        (set! magic-x 30)
        (check (eq? magic-x 30)))
      
      (jump /argument-lists)))


  ;;=======================================================================
  ;;  Argument Lists
  ;;=======================================================================

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

  (card /argument-lists ()
    (run
      (check (eq? (t1) 'ok))
      (check (equal? (t2 1 2 3) '(1 2 3)))
      (check (equal? (t3 1 2 3) '(2 3)))
      (check (equal? (t3 1 2) '(2)))
      (check (eq? (o1) #f))
      (check (eq? (o1 2) 2))
      (check (equal? (o2 0) '(1 2)))
      (check (equal? (o2 0 2) '(2 4)))
      (check (equal? (o2 0 2 3) '(2 3)))
      (check (equal? (r1 1 2 3) '(1 2 3)))
      (check (equal? (r2 1 2 3) '(2 3)))
      (check (equal? (k1) '(#f #f)))
      (check (equal? (k1 :x 1) '(1 #f)))
      (check (equal? (k1 :y 2) '(#f 2)))
      (check (equal? (k1 :y 2 :x 1) '(1 2)))
      (check (equal? (k2) '(foo bar)))
      (check (equal? (k2 :x 'baz) '(baz bar)))
      (check (equal? (k2 :y 'baz) '(foo baz)))
      (check (equal? (k2 :x 'baz :y 'moby) '(baz moby)))
      (check (equal? (k3 :x 'baz) '(baz bar)))
      (check (equal? (k3 :y 'baz) '(foo baz)))
      (check (equal? (k3 :x 'baz :y 'moby) '(baz moby)))
      (check (equal? (k4 1) '(1 2)))
      (check (equal? (k4 1 :x 2) '(2 2)))
      (check (equal? (k4 1 :y 3) '(1 3)))
      (check (equal? (k4 1 :x 2 :y 3) '(2 3)))
      (check (equal? (k5) '(#f ())))
      (check (equal? (k5 :x 2) '(2 (:x 2))))
      (check (equal? (k5 :y 1 :x 2) '(2 (:y 1 :x 2))))
      (jump /g1/start)
      ))


  ;;=======================================================================
  ;;  Ordered & Unordered Groups
  ;;=======================================================================

  (define *last-card* #f)
  (define *last-group* #f)

  (group /g1 (%card-group% :ordered? #f)
    (run
      (check (not ((static-root-node) .ordered?)))
      (check (eq? #f (.ordered?)))
      (set! *last-group* /g1)))
  
  (card /g1/start ()
    (run
      (check (eq? *last-group* /g1))
      (set! *last-card* /g1/start)
      (jump @s1)))

  (group /g1/s1 ()
    (run
      (check (eq? #t (.ordered?)))
      (set! *last-group* /g1/s1)))

  (card /g1/s1/c1 ()
    (run
      (check (eq? *last-group* /g1/s1))
      (check (eq? *last-card* /g1/start))
      (check (not (card-prev)))
      (check (eq? '/g1/s1/c2 (card-name (card-next))))
      (check (eq? /g1/s1/c2 (@c2 .resolve-path :running? #f)))
      (check (eq? /g1/s1/c2 (@s1/c2 .resolve-path :running? #f)))
      (set! *last-card* /g1/s1/c1)
      (jump @c2)))

  (card /g1/s1/c2 ()
    (run
      (check (eq? *last-card* /g1/s1/c1))
      (check (eq? '/g1/s1/c1 (card-name (card-prev))))
      (set! *last-card* /g1/s1/c2)
      (jump (card-next))))

  (group /g1/s1/s2 (%card-group% :ordered? #t)
    (run
      (check (eq? #t (.ordered?)))
      (set! *last-group* /g1/s1/s2)))

  (card /g1/s1/s2/c1 ()
    (run
      (check (eq? *last-group* /g1/s1/s2))
      (check (eq? *last-card* /g1/s1/c2))
      (check (eq? '/g1/s1/c2 (card-name (card-prev))))
      (set! *last-card* /g1/s1/s2/c1)
      (jump @c3)))

  (card /g1/s1/s2/c2 () ; We jump here out of order!
    (run
      (check (eq? *last-card* /g1/s1/c3))
      (set! *last-card* /g1/s1/s2/c2)
      (jump @c4)))

  (card /g1/s1/c3 ()
    (run
      (check (eq? *last-card* /g1/s1/s2/c1))
      (check (eq? '/g1/s1/s2/c2 (card-name (card-prev))))
      (set! *last-card* /g1/s1/c3)
      (jump @s2/c2)))

  (card /g1/s1/c4 ()
    (run
      (check (eq? *last-card* /g1/s1/s2/c2))
      (check (not (card-next)))
      (set! *last-card* /g1/s1/c4)
      (jump /g1/done)))

  (card /g1/done ()
    (run
      (check (eq? *last-card* /g1/s1/c4))
      (jump /template-tests-1)))


  ;;=======================================================================
  ;;  Templates and Other Goodies
  ;;=======================================================================

  (define *ttvar1* #f)
  (define *ttvar2* #f)

  (define-class %card-template-1% (%card%)
    (attr prop-a :type <string>  :label "Prop A")
    (attr prop-b :type <integer> :label "Prop B")
    
    (run
      (check (not *ttvar1*))
      (check (not *ttvar2*))
      (check (instance-of? (.prop-a) <string>))
      (check (instance-of? (.prop-b) <integer>))
      (set! *ttvar1* #t)))

  (define-class %card-template-2% (%card-template-1%)
    (attr prop-c :type <string> :label "Prop C")
    (attr prop-d 20 :type <integer> :label "Prop D")
    (value prop-b 10)

    (run
      (check *ttvar1*)
      (check (not *ttvar2*))
      (check (instance-of? (.prop-c) <string>))
      (check (instance-of? (.prop-d) <integer>))
      (set! *ttvar2* #t)))

  (card /template-tests-1 (%card-template-2%)
    (value prop-a "foo")
    (value prop-c "bar")

    (run 
      (check *ttvar1*)
      (check *ttvar2*)
      (check (equal? (.prop-a) "foo"))
      (check (equal? (.prop-b) 10))
      (check (equal? (.prop-c) "bar"))
      (check (equal? (.prop-d) 20))
      (set! *ttvar1* #f)
      (set! *ttvar2* #f)
      (jump /template-tests-2)))

  (card /template-tests-2 (%card-template-2%)
    (value prop-a "baz")
    (value prop-c "moby")
    (value prop-d 30)

    (run
      (check *ttvar1*)
      (check *ttvar2*)
      (check (equal? (.prop-a) "baz"))
      (check (equal? (.prop-b) 10))
      (check (equal? (.prop-c) "moby"))
      (check (equal? (.prop-d) 30))
      (jump /template-tests-5)))

  (define-class %card-template-3% (%card%)
    (def (message-1)
      'foo))

  (card /template-tests-5 (%card-template-3%)
    (def (message-1)
      (super))
    (def (message-2)
      'bar)
    (run
      (check (eq? (.message-1) 'foo))
      (check (eq? (.message-2) 'bar))
      (jump /syntax-tests)))


  ;;=======================================================================
  ;;  Syntax
  ;;=======================================================================

  (card /syntax-tests ()
    (run
      (check (eq? ((fn (x) (* x x)) 3) 9))
      (check (eq? ((callback 1)) 1))

      (jump /swindle-tests)))


  ;;=======================================================================
  ;;  Swindle
  ;;=======================================================================

  (defclass <swindle-test> (<object>)
    simple-slot
    (typed-slot :type <integer>)
    (init-value-slot :initvalue 10)
    (init-func-slot :initializer (fn () 100)))

  (defmethod generic-test-method (arg1 arg2)
    'unspecific)

  (defmethod generic-test-method ((arg1 <integer>) arg2)
    'arg1-int)

  (defmethod generic-test-method (arg1 (arg2 <string>))
    'arg2-string)

  (defmethod generic-test-method ((arg1 <integer>) (arg2 <string>))
    'int-and-string)

  (defmethod generic-test-method ((arg1 = 1) arg2)
    'arg1-singleton)

  (defmethod generic-test-method ((arg1 = 1) (arg2 <string>))
    'singleton-string)

  (defclass <swindle-test-subclass> (<swindle-test>)
    subclass-slot)

  (defclass <swindle-test-other-subclass> (<swindle-test>)
    other-subclass-slot)

  (defclass <swindle-test-single-subclass> (<swindle-test-subclass>)
    single-subclass-slot)

  (defclass <swindle-test-multiple-subclass> (<swindle-test-subclass>
                                              <swindle-test-other-subclass>)
    multiple-subclass-slot)

  (defmethod inheritance-test-method ((arg1 <swindle-test>) 
                                      (arg2 <swindle-test>))
    'unspecific)

  (defmethod inheritance-test-method ((arg1 <swindle-test-subclass>)
                                      (arg2 <swindle-test>))
    'arg1-sub)

  (defmethod inheritance-test-method ((arg1 <swindle-test>)
                                      (arg2 <swindle-test-other-subclass>))
    'arg2-other-sub)

  (defmethod inheritance-test-method ((arg1 <swindle-test-subclass>)
                                      (arg2 <swindle-test-other-subclass>))
    'both-sub-other)

  (defmethod inheritance-test-method ((arg1 <swindle-test-single-subclass>)
                                      (arg2 <swindle-test-other-subclass>))
    'arg1-single)

  (defmethod inheritance-test-method ((arg1 <swindle-test-multiple-subclass>)
                                      (arg2 <swindle-test-multiple-subclass>))
    'both-multiple)

  (card /swindle-tests ()
    (run
      (define test-obj-1 (make <swindle-test> 
                           :simple-slot 'foo
                           :typed-slot 10))
      (define test-obj-2 (make <swindle-test>
                           :simple-slot "bar"
                           :typed-slot 15
                           :init-value-slot 100
                           :init-func-slot 10))
      (check (class? <swindle-test>))
      (check (instance-of? test-obj-1 <swindle-test>))
      (check (instance-of? test-obj-2 <swindle-test>))
      (check (not (instance-of? test-obj-1 <integer>)))
      (check (eq? (swindle-test-simple-slot test-obj-1) 'foo))
      (check (eq? (swindle-test-typed-slot test-obj-1) 10))
      (check (eq? (swindle-test-init-value-slot test-obj-1) 10))
      (check (eq? (swindle-test-init-func-slot test-obj-1) 100))
      (check (instance-of? (swindle-test-simple-slot test-obj-2) <string>))
      (check (equal? (swindle-test-simple-slot test-obj-2) "bar"))
      (check (eq? (swindle-test-init-value-slot test-obj-2) 100))
      (check (eq? (swindle-test-init-func-slot test-obj-2) 10))

      (check (eq? (generic-test-method test-obj-1 test-obj-2)
                  'unspecific))
      (check (eq? (generic-test-method 10 test-obj-2)
                  'arg1-int))
      (check (eq? (generic-test-method test-obj-1 "foo")
                  'arg2-string))
      (check (eq? (generic-test-method 42 "baz")
                  'int-and-string))
      (check (eq? (generic-test-method 1 test-obj-2)
                  'arg1-singleton))
      (check (eq? (generic-test-method 1 "foo")
                  'singleton-string))
    
      (define sub (make <swindle-test-subclass>))
      (define other-sub (make <swindle-test-other-subclass>))
      (define single (make <swindle-test-single-subclass>))
      (define multiple (make <swindle-test-multiple-subclass>))

      (check (eq? (inheritance-test-method test-obj-1 test-obj-2)
                  'unspecific))
      (check (eq? (inheritance-test-method sub test-obj-2)
                  'arg1-sub))
      (check (eq? (inheritance-test-method test-obj-1 other-sub)
                  'arg2-other-sub))
      (check (eq? (inheritance-test-method sub other-sub)
                  'both-sub-other))
      (check (eq? (inheritance-test-method single other-sub)
                  'arg1-single))
      (check (eq? (inheritance-test-method multiple multiple)
                  'both-multiple))
      (check (eq? (inheritance-test-method multiple other-sub)
                  'both-sub-other))
      (check (eq? (inheritance-test-method single multiple)
                  'arg1-single))
    
      (jump /script-editor-db-tests)))


  ;;=======================================================================
  ;;  ScriptEditorDB
  ;;=======================================================================

  (card /script-editor-db-tests ()
    (run
      ;; As long as we want to test the ScriptEditorDB from within
      ;; CommonTest, we can only do it when TSchemeInterpreter is set up.  So
      ;; we call it from inside our TSchemeInterpreter tests, right here.
      (call-prim 'TestScriptEditorDB)
      (jump /layout)))


  ;;=======================================================================
  ;;  Layout
  ;;=======================================================================

  (require (lib "layout.ss" "halyard"))

  (card /layout ()
    (run
      ;; Test <layout> defaults.
      (define layout (make <layout>))
      (check (layout? layout))
      (check (eq? (layout-hspace layout) 0))
      (check (eq? (layout-vspace layout) 0))
      (check (eq? (layout-box-shape layout) #f))

      (define (test-box box left top right bottom)
        (check (equals? box (rect left top right bottom))))
    
      (define (test-shape-used w h)
        ;;(print "SHAPE: ")
        ;;(print w)
        ;;(print h)
        ;;(print (layout-shape-used layout))
        ;;(newline)
        (check (equals? (layout-shape-used layout) (rect 0 0 w h))))
    
      ;; Test basic layout.
      (set! layout (make <layout>
                     :hspace 7 :vspace 5
                     :box-shape (rect 0 0 13 11)))
      (check (eq? (layout-hspace layout) 7))
      (check (eq? (layout-vspace layout) 5))
      (check (equals? (layout-box-shape layout) (rect 0 0 13 11)))
      (test-box (add-box! layout)
                0 0 13 11)
      (test-shape-used 13 11)
      (test-box (add-box! layout :width 19)
                0 (+ 11 5) 19 (+ 11 5 11))
      (test-shape-used 19 (+ 11 5 11))
      (test-box (add-box! layout :height 17)
                0 (+ 11 5 11 5) 13 (+ 11 5 11 5 17))
      (test-shape-used 19 (+ 11 5 11 5 17))
      (next-column! layout)
      (test-shape-used 19 (+ 11 5 11 5 17))
      (test-box (add-box! layout)
                (+ 19 7) 0 (+ 19 7 13) 11)
      (test-shape-used (+ 19 7 13) (+ 11 5 11 5 17))
      (test-box (add-box! layout :shape (rect 0 0 23 500))
                (+ 19 7) (+ 11 5) (+ 19 7 23) (+ 11 5 500))
      (test-shape-used (+ 19 7 23) (+ 11 5 500))
    
      ;; Test box history.
      (check (equals? (layout-nth-box layout 1)
                      (rect 0 (+ 11 5) 19 (+ 11 5 11))))
      (check (equals? (layout-nth-box-at layout 1) (point 0 (+ 11 5))))
      (check (equals? (layout-nth-box-shape layout 1) (rect 0 0 19 11)))
      (check (= (layout-box-count layout) 5))
    
      ;; Test convencience functions.
      (set! layout (make <layout> :vspace 5 :box-shape (rect 0 0 40 10)))
      (check (equals? (layout-next-box-at! layout :width 50) (point 0 0)))
      (check (equals? (layout-current-box-shape layout) (rect 0 0 50 10)))
    
      (jump /mizzen)))

  
  ;;=======================================================================
  ;;  Mizzen unit tests
  ;;=======================================================================

  (require (lib "mizzen-unit.ss" "mizzen"))
  (require (lib "tests.ss" "mizzen"))

  ;; Test report class that collects the reports and also sends them to the
  ;; engine primitive Test.
  (define-class %engine-test-report% (%test-report%)
    (def (report-failure! test-case exception)
      (super)
      (check-with-label (((test-case) .test-method) .title) #f))
    (def (report-success!)
      (super)
      (check-with-label "Mizzen test succeeded." #t)))

  (card /mizzen ()
    (run
      (define report (%engine-test-report% .new))
      (foreach [test-class $mizzen-tests]
        (test-class .run-tests report))
      (jump /cpp-tests)))


  ;;=======================================================================
  ;;  C++ unit tests
  ;;=======================================================================

  (card /cpp-tests ()
    (run
      (call-prim 'RunAllCppTests)
      (jump /done)))

  (card /done ()
    (run
      (exit-script)))

  )
