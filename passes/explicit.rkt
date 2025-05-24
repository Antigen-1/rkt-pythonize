#lang racket/base
(require nanopass/base "uniquify.rkt")
(provide parse-L3 unparse-L3 L3 make-explicit)

(define-language L3
  (extends L2)
  (Expr (e body)
   (- (lambda (x* ...) body))
   (+ (lambda (x* ...) body* ...))
   (- (let/cc x body))
   (+ (let/cc x body* ...))))

(define-parser parse-L3 L3)

(define-pass make-explicit :
  L3 (ir) -> L2 ()
  (Expr : Expr (ir) -> Expr ()
        ((lambda (,x* ...) ,[body*] ...)
         `(lambda (,x* ...)
            (begin ,body* ...)))
        ((let/cc ,x ,[body*] ...)
         `(let/cc ,x
            (begin ,body* ...)))))
