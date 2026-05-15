#lang racket/base
(require "base.rkt" "assignment.rkt" "closure.rkt" "check-uniquification.rkt" racket/runtime-path json)
(provide compile-L0
         current-primitives
         (rename-out (parse-LS parse-L0)
                     (unparse-LS unparse-L0)
                     (LS L0))
         core-py
         primitives)

(define-runtime-path core-py "core.py")

(define compile-L0
  (compose1 (lambda (json) (jsexpr->string json #:null 'none)) render-LB analyze-closure check-uniquification expand-assignments))

