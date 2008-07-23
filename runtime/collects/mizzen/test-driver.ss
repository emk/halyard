;;; A command-line callable unit-test driver.  You can run this from
;;; mzscheme by typing:
;;;
;;;   PLTCOLLECTS=:/path/to/halyard/test/Runtime
;;;   mzscheme -u test-driver.ss
(module test-driver "mizzen.ss"

  (require "mizzen-unit.ss" "tests.ss")

  (printf "Running unit tests~n")
  (run-tests $mizzen-tests)

  )