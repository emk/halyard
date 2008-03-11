(module errortrace-test (lib "5l.ss" "halyard")
  ;;;
  ;;; The purpose of this module is to provide a unit test for the 
  ;;; errortrace machinery.  As such, it should be loaded using the 
  ;;; errortrace compiler, and it shouldn't be edited in such a way
  ;;; as to change the line numbers, without also updating the line
  ;;; numbers in the corresponding unit tests.
  ;;;

  (provide a method-error-test)

  (define (a n)
    (+ 1 (b n)))

  (define (b n)
    (+ 1 (c n)))

  (define (c n)
    (+ 1 (d n)))

  (define (d n)
    (+ 1 (e n)))

  (define (e n)
    (+ 1 (f n)))

  (define (f n)
    (+ 1 (g n)))

  (define (g n)
    (+ 1 (h n)))

  (define (h n)
    (+ 1 (i n)))

  (define (i n)
    (+ 1 n))

  (define-class %errortrace-test% ()
    (attr foo)
    (def (test-method x)
      (define b (%another-test% .new :bar 10))
      (* (b .test-me x) (.foo))))

  (define-class %another-test% ()
    (attr bar)
    (def (test-me x)
      (+ (.bar) x)))

  (define (method-error-test)
    (define t (%errortrace-test% .new :foo 3))
    (/ 1 (t .test-method "hello!")))
)