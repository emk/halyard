(module benchmark (lib "halyard.ss" "halyard")
  (require (lib "base.ss" "halyard-test"))
  (require (lib "benchmark.ss" "mizzen"))

  ;; We need to benchmark some kernel internals, so we need access to
  ;; private APIs.
  (require (lib "kernel.ss" "halyard/private"))


  ;;=========================================================================
  ;;  Benchmark card
  ;;=========================================================================

  (define-stylesheet $benchmark-style
    :base $base-style
    :size 18
    :color $color-black
    :highlight-color (color 0 0 #x80))

  (define-stylesheet $benchmark-number-style
    :base $benchmark-style
    :justification 'right)

  (define-class %result-column% (%text%)
    (default style $benchmark-number-style)
    (def (append! str)
      (set! (.text) (cat (.text) "\n" str))))

  (card /benchmark (%white-test-card% :title "Benchmarks")
    (elem microseconds
        (%result-column% 
         :at (below @title-elem 20) :text "<h><b>Microseconds</b></h>"))
    (elem bytes
        (%result-column%
         :at (to-the-right-of @microseconds 20) :text "<h><b>Bytes</b></h>"))
    (elem label
        (%result-column%
         :at (to-the-right-of @bytes 20) :text "<h><b>Benchmark</b></h>"
         :style $benchmark-style))

    (run
      (foreach [benchmark (all-benchmarks)]
        (idle)
        (define report (benchmark .run))
        ((.microseconds) .append! (number->string (report .microseconds)))
        ((.bytes) .append! (number->string (report .bytes)))
        ((.label) .append! ((report .benchmark) .name)))
      ((.label) .append! "(Done!)"))
    )
  

  ;;=========================================================================
  ;;  Benchmarks
  ;;=========================================================================

  (define-class %parent% ()
    (def (test-method arg1 arg2 &rest args)
      (void)))
  
  (define-class %child% (%parent%)
    (def (test-method arg1 arg2 &rest args)
      (super)))
  
  (define-class %grandchild% (%child%)
    (def (test-method arg1 arg2 &rest args)
      (super)))
  
  (define-benchmark "Dispatch with lots of arguments" 10000
    (define grandchild (%grandchild% .new))
    (benchmark
      (grandchild .test-method 'foo 'bar 'baz 'quuz 'zot 'tiddle 'hello 
                  'goodbye 'something 'something-else 'lorem 'ipsum 'dolor
                  'sit 'amet 1 2 3 4 5 6 7 8 9 10)))

  (define-benchmark "Call have-prim? primitive" 100
    (benchmark (have-prim? 'NoSuchPrimitive)))

  (define-benchmark "Create and destroy %invisible-element%" 100
    (benchmark
      (let [[e (%invisible-element% .new)]]
        (delete-element e))))

  (define-benchmark "Create and destroy %box%" 100
    (benchmark
      (let [[e (%box% .new :at (point 10 10) :shape (shape 100 100))]]
        (delete-element e))))

  (define-benchmark "Create and destroy %custom-element%" 100
    (benchmark
      (let [[e (%custom-element% .new :at (point 10 10) 
                                      :shape (shape 100 100))]]
        (delete-element e))))
  )