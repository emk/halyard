;;; A command-line callable benchmark driver.  You can run this from mzscheme
;;; by typing:
;;;
;;;   PLTCOLLECTS=:/path/to/halyard/test/Runtime
;;;   mzscheme -u benchmark-driver.ss
(module benchmark-driver "mizzen.ss"


  ;;=======================================================================
  ;;  Unit tests
  ;;=======================================================================
  ;;  Before running the benchmarks, make sure that our unit tests all
  ;;  pass.  There's no point in speeding things up unless they still work.

  (require "mizzen-unit.ss" "mizzen-unit-test.ss" "ruby-objects-test.ss")

  (printf "Running unit tests~n")
  (run-tests (append $all-mizzen-unit-tests
                     $all-ruby-object-tests))

  ;;=======================================================================
  ;;  Benchmarks
  ;;=======================================================================

  (require "benchmark.ss")

  (printf "Running benchmarks~n")
  (foreach [benchmark (all-benchmarks)]
    (define report (benchmark .run))
    (printf "~a ~a ~a~n" (report .microseconds) (report .bytes)
            ((report .benchmark) .name)))

  )
