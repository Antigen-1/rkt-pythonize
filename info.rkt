#lang info
(define collection "rkt-pythonize")
(define deps '("base" "nanopass"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/rkt-pythonize.scrbl" ())))
(define pkg-desc "A Lisp-to-Python transpiler")
(define version "1.1.0")
(define pkg-authors '(zhanghao))
(define license '(Apache-2.0 OR MIT))
(define raco-commands
  (list (list "rkt-pythonize" '(submod "main.rkt" main) "transpile LB to Python" #f)))
(define racket-launcher-names '("rkt-pythonize"))
(define racket-launcher-libraries '("main.rkt"))
