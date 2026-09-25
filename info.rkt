#lang info
(define collection "rkt-pythonize")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/rkt-pythonize.scrbl" ())))
(define pkg-desc "A Racket-hosted DSL that compiles to Python")
(define version "2.0.0")
(define pkg-authors '(zhanghao))
(define license '(Apache-2.0 OR MIT))
(define raco-commands
  (list (list "rkt-pythonize" '(submod "cli.rkt" main) "compile a rkt-pythonize module to Python" #f)))
(define racket-launcher-names '("rkt-pythonize"))
(define racket-launcher-libraries '("cli.rkt"))
