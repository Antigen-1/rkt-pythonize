#lang racket/base
(require nanopass/base "uniquify.rkt")
(provide parse-L2 unparse-L2 L2 make-explicit)

(define-language L2
  (extends L1)
  (Expr (e body)
   (- (lambda (x* ...) body))
   (+ (lambda (x* ...) body* ...))))

(define-parser parse-L2 L2)

(define-pass make-explicit :
  L2 (ir) -> L1 ()
  (Expr : Expr (ir) -> Expr ()
        ((lambda (,x* ...) ,[body*] ...)
         `(lambda (,x* ...)
            (begin ,body* ...)))))
