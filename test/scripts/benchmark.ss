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


  ;;=======================================================================
  ;;  Drawing benchmarks
  ;;=======================================================================
  ;;  These are located here, and not in halyard/benchmarks, because some
  ;;  of them require graphics or styles.

  (define-benchmark "clear-dc 400x400 opaque overlay" 500
    (define dc
      (%custom-element% .new :at (point 0 0) :shape (shape 400 400) :alpha? #f))
    (with-dc dc
      (benchmark (clear-dc (color 255 0 0))))
    (delete-element dc))

  (define-benchmark "draw-rectangle 400x400 opaque overlay" 500
    (define dc
      (%custom-element% .new :at (point 0 0) :shape (shape 400 400) :alpha? #f))
    (with-dc dc
      (benchmark (draw-rectangle (rect 0 0 400 400) (color 255 0 0))))
    (delete-element dc))

  (define-benchmark "clear-dc 400x400 alpha overlay" 500
    (define dc
      (%custom-element% .new :at (point 0 0) :shape (shape 400 400) :alpha? #t))
    (with-dc dc
      (benchmark (clear-dc (color 255 0 0 128))))
    (delete-element dc))

  (define-benchmark "draw-rectangle 400x400 alpha overlay" 500
    (define dc
      (%custom-element% .new :at (point 0 0) :shape (shape 400 400) :alpha? #f))
    (with-dc dc
      (benchmark (draw-rectangle (rect 0 0 400 400) (color 255 0 0 128))))
    (delete-element dc))

  (define-benchmark "draw-graphic 110x110 on alpha overlay" 500
    (define dc
      (%custom-element% .new
                        :at (point 0 0)
                        :shape (measure-graphic "lens.png")
                        :alpha? #t))
    (with-dc dc
      (benchmark (draw-graphic (point 0 0) "lens.png")))
    (delete-element dc))

  (define-benchmark "mask 128x128 on alpha overlay" 500
    (define dc
      (%custom-element% .new
                        :at (point 0 0)
                        :shape (measure-graphic "mask/mask.png")
                        :alpha? #t))
    (with-dc dc
      (benchmark (mask (point 0 0) "mask/mask.png")))
    (delete-element dc))

  (define-benchmark "draw-text 13 18-point letters on alpha overlay" 500
    (define shape (measure-text $caption-style "Hello, world!"))
    (define dc (%custom-element% .new :at (point 0 0) :shape shape :alpha? #t))
    (with-dc dc
      (benchmark (draw-text shape $caption-style "Hello, world!")))
    (delete-element dc))

  (define-benchmark "measure-text 13 18-point letters" 500
    (benchmark (measure-text $caption-style "Hello, world!")))
    
  )