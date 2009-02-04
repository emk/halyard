;; @BEGIN_LICENSE
;;
;; Mizzen - Scheme object system
;; Copyright 2006-2009 Trustees of Dartmouth College
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as
;; published by the Free Software Foundation; either version 2.1 of the
;; License, or (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;; 
;; You should have received a copy of the GNU Lesser General Public
;; License along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
;; USA.
;;
;; @END_LICENSE

;;; A benchmarking library and some standard benchmarks.  This helps
;;; considerably with performance-tuning mizzen.
(module benchmark "mizzen.ss"


  ;;=======================================================================
  ;;  Benchmarking library
  ;;=======================================================================

  (provide %benchmark% %benchmark-report% memory-allocated all-benchmarks
           define-benchmark)
  
  (define-class %benchmark% ()
    (attr name :type <string>)
    (attr count :type <number>)
    (attr function :type <function>)
    
    ;;; Run the benchmark and return the number of microseconds required
    ;;; for a single iteration.
    (def (run)
      (define function (.function))
      (define count (.count))
      ;; Collect garbage before running benchmark.
      (collect-garbage)
      (with-values [[total-milliseconds bytes] (function count)]
        (%benchmark-report% .new
          :benchmark self
          :microseconds (/ (* 1000.0 total-milliseconds) count)
          :bytes bytes))))

  (define-class %benchmark-report% ()
    (attr benchmark :type %benchmark%)
    (attr microseconds :type <number>)
    (attr bytes :type <number>))

  (define-syntax memory-allocated
    (syntax-rules ()
      [(_ body ...)
       (let [[start (current-memory-use)]]
         body ...
         (- (current-memory-use) start))]))

  (define *benchmarks* '())

  (define (all-benchmarks)
    (reverse *benchmarks*))

  ;; We need to define a setter so we can change *benchmarks* from macros
  ;; expanded outside of this module.
  (define (set-benchmarks! lst)
    (set! *benchmarks* lst))

  (define-syntax define-benchmark
    (syntax-rules (benchmark)
      [(_ name count init-code ... (benchmark body ...))
       (set-benchmarks!
        (cons
         (%benchmark% .new
           :name name :count count
           :function (fn (remaining)
                       init-code ...
                       (define bytes (memory-allocated body ...))
                       (define start (current-milliseconds))
                       (let next [[remaining remaining]]
                         (unless (zero? remaining)
                           body ...
                           (next (- remaining 1))))
                       (values (- (current-milliseconds) start) bytes)))
         *benchmarks*))]))


  ;;=======================================================================
  ;;  Calibration
  ;;=======================================================================
  ;;  These measurements help us interpret other benchmark results.

  (define-benchmark "(collect-garbage)" 1
    (benchmark (collect-garbage)))

  (define-benchmark "Do nothing" 1000000
    (benchmark #f))


  ;;=======================================================================
  ;;  PLT primitives
  ;;=======================================================================

  (define-benchmark "PLT: hash-table-get" 1000000
    (define table (make-hash-table))
    (hash-table-put! table 'x 1)
    (benchmark (hash-table-get table 'x)))

  (define-benchmark "PLT: hash-table-get default" 1000000
    (define table (make-hash-table))
    (benchmark (hash-table-get table 'x 1)))

  (define-benchmark "PLT: hash-table-get default with closure" 1000000
    (define table (make-hash-table))
    (benchmark (hash-table-get table 'x (fn () 1))))

  (define-benchmark "PLT: hash-table-put!" 1000000
    (define table (make-hash-table))
    (benchmark (hash-table-put! table 'x 1)))


  ;;=======================================================================
  ;;  Method dispatch
  ;;=======================================================================

  (define-class %simple-dispatch% ()
    (def (do-nothing)
      #f))

  (define-benchmark "Simple method dispatch" 1000000
    (define obj (%simple-dispatch% .new))
    (benchmark (obj .do-nothing)))

  (define-class %super-dispatch% (%simple-dispatch%)
    (def (do-nothing)
      (super)))

  (define-benchmark "Calling super" 1000000
    (define obj (%super-dispatch% .new))
    (benchmark (obj .do-nothing)))

  (define-class %attribute-holder% ()
    (attr x 10 :writable? #t))

  (define-benchmark "Read attr" 1000000
    (define obj (%attribute-holder% .new))
    (benchmark (obj .x)))

  (define-benchmark "Write attr" 1000000
    (define obj (%attribute-holder% .new))
    (benchmark (set! (obj .x) 10)))


  ;;=======================================================================
  ;;  Object creation
  ;;=======================================================================

  (define-class %boring-class% ())

  (define-benchmark "Create class without attributes" 10000
    (benchmark (%boring-class% .new)))

  (define-class %complicated-class% ()
    (dotimes [i 20]
      (.attr (symcat "a" i) :default (method () 1)))
    (attr b (+ (.a1) (.a2) (.a3) (.a4) (.a5))))

  (define-benchmark "Create class with many attributes" 1000
    (benchmark (%complicated-class% .new)))
  

  ;;=======================================================================
  ;;  instance-of?
  ;;=======================================================================

  (define-class %instance-of-test-1% ())
  (define-class %instance-of-test-2% (%instance-of-test-1%))
  (define-class %instance-of-test-3% (%instance-of-test-2%))
  (define-class %instance-of-test-4% (%instance-of-test-3%))
  (define-class %instance-of-test-5% (%instance-of-test-4%))
  (define-class %instance-of-test-6% (%instance-of-test-5%))

  (define-benchmark "instance-of? (1 level)" 10000
    (define obj (%instance-of-test-1% .new))
    (benchmark (obj .instance-of? %instance-of-test-1%)))

  (define-benchmark "instance-of? (6 levels)" 10000
    (define obj (%instance-of-test-6% .new))
    (benchmark (obj .instance-of? %instance-of-test-1%)))

  )