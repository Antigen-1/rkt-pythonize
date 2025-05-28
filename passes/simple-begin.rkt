#lang racket/base
(require nanopass/base "uniquify.rkt")
(provide L2 parse-L2 unparse-L2 reduce-simple-begin-forms)

(define-pass reduce-simple-begin-forms :
  L2 (ir) -> L2 ()
  (Expr : Expr (ir) -> Expr ()
        ((begin ,[e])
         e)))
