#lang racket/base
(require racket/list nanopass/base "uniquify.rkt")
(provide parse-L2 unparse-L2 L2 expand-internal-begin)

(define (statement->statement-list st)
  (nanopass-case (L2 Expr) st
                 ((begin ,e* ...)
                  (append* (map statement->statement-list e*)))
                 (else (list st))))

(define-pass expand-internal-begin :
  L2 (ir) -> L2 ()
  (Expr : Expr (ir) -> Expr ()
        ((begin
           ,[body*] ...)
         (define new-body* (append* (map statement->statement-list body*)))
         `(begin
            ,new-body* ...))))
