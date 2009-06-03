;; @BEGIN_LICENSE
;;
;; Halyard - Multimedia authoring and playback system
;; Copyright 1993-2009 Trustees of Dartmouth College
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

(module benchmark (lib "halyard.ss" "halyard")
  (require (lib "benchmark.ss" "mizzen"))

  ;; We need to benchmark some kernel internals, so we need access to
  ;; private APIs.
  (require (lib "kernel.ss" "halyard/private"))

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

  (define-benchmark "Call UntracedNoop primitive" 10000
    (benchmark (call-prim 'UntracedNoop)))

  (define-benchmark "Call Noop primitive" 10000
    (benchmark (call-prim 'Noop)))

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