#lang racket/base
(require nanopass/base "stream.rkt")
(provide L11 parse-L11 unparse-L11 expand-more-cond)

(define-language L11
  (extends L10)
  (Else (el)
        (+ (else e)))
  (Expr (e body)
        (+ (cond (e0 e1) ... el))))

(define-parser parse-L11 L11)

(define-pass expand-more-cond
  : L11 (ir) -> L10 ()
  (Expr : Expr (ir) -> Expr ()
        ((cond (,[e0] ,[e1]) ... (else ,[e2]))
         (foldr
          (lambda (cond then else)
            `(if ,cond
                 ,then
                 ,else))
          e2
          e0 e1))))
