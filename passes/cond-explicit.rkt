#lang racket/base
(require nanopass/base "more-cond.rkt")
(provide L12 parse-L12 unparse-L12 make-cond-explicit)

(define-language L12
  (extends L11)
  (Else (el)
        (- (else e))
        (+ (else e* ...)))
  (Expr (e body)
        (- (cond (e0 e1) ... el))
        (+ (cond (e0 e1 ...) ... el))))

(define-parser parse-L12 L12)

(define-pass make-cond-explicit
  : L12 (ir) -> L11 ()
  (Expr : Expr (ir) -> Expr ()
        ((cond (,[e0] ,[e1] ...) ...
               (else ,[e*] ...))
         (define then-exprs (map (lambda (e) `(begin ,e ...)) e1))
         `(cond
            (,e0 ,then-exprs)
            ...
            (else (begin ,e* ...))))
        )
  )
