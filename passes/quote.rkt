#lang racket/base
(require nanopass/base "explicit.rkt")
(provide parse-L4 unparse-L4 L4 add-quote)

(define-language L4
  (extends L3)
  (terminals (+ (constant (c))))
  (Expr (e body)
        (+ c)))

(define constant?
  (lambda (v)
    (or (string? v)
        (fixnum? v)
        (flonum? v)
        (boolean? v))))

(define-parser parse-L4 L4)

(define-pass add-quote : L4 (ir) -> L3 ()
  (Expr : Expr (ir) -> Expr ()
        (,c `',c)))
