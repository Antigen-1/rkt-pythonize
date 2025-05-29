#lang racket/base
(require nanopass/base json "explicit.rkt" "partial-evaluate.rkt")
(provide parse-L4 unparse-L4 L4 add-quote)

(define-language L4
  (extends L3)
  (terminals (+ (constant (c))))
  (Expr (e body)
        (+ c)))

(define constant?
  (lambda (v)
    (or (string? v)
        (json-number? v)
        (boolean? v)
        (and (hash? v) (jsexpr? v #:null 'none)))))

(define-parser parse-L4 L4)

(define-pass add-quote : L4 (ir) -> L3 ()
  (Expr : Expr (ir) -> Expr ()
        (,c `',c)))
