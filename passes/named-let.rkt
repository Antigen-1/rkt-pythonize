#lang racket/base
(require nanopass/base "let.rkt")
(provide L6 unparse-L6 parse-L6 expand-named-let)

(define-language L6
  (extends L5)
  (Expr (e body)
        (+ (let x ((x* e*) ...)
             body* ...))))

(define-parser parse-L6 L6)

(define-pass expand-named-let :
  L6 (ir) -> L5 ()
  (Expr : Expr (ir) -> Expr ()
        ((let ,x ((,x* ,[e*]) ...)
           ,[body*] ...)
         `(letrec ((,x (lambda (,x* ...)
                         ,body* ...)))
            (,x ,e* ...)))))
