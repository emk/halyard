(module benchmark (lib "halyard.ss" "halyard")
  (require (lib "base.ss" "halyard-test"))
  (require (lib "benchmark.ss" "mizzen"))


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
  )