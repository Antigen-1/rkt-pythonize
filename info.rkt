#lang info
(define collection "rkt-pythonize")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/rkt-pythonize.scrbl" ())))
(define pkg-desc "An LE-to-Python compiler behind one macro")
(define version "3.5.4")
(define pkg-authors '(zhanghao))
(define license '(Apache-2.0 OR MIT))
