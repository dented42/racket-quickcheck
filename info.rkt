#lang setup/infotab

(define blurb '("DeinProgramm - QuickCheck"))
(define primary-file "main.rkt")
(define deps '("base" "scribble-lib"))

(define compile-omit-files
  '("examples.scm"
    "packages.scm"
    "quickcheck-test.scm"
    "quickcheck.scm"
    "random.scm"))


