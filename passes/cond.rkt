#lang racket/base
(require nanopass/base "named-let.rkt")
(provide parse-L7 unparse-L7 L7 expand-cond)

(define-language L7
  (extends L6)
  (Expr (e body)
        (- (if e0 e1 e2))
        (+ (and e* ...)
           (or e* ...)
           (if e0 e1 e2)
           (if e0 e1)
           )))

(define-parser parse-L7 L7)

(define-pass expand-cond :
  L7 (ir) -> L6 ()
  (Expr : Expr (ir) -> Expr ()
        ((and) #t)
        ((and ,[e*] ... ,[e0])
         (foldr (lambda (e b)
                  (define v (gensym))
                  `(let ((,v ,e))
                     (if ,v
                         ,b
                         #f)))
                e0 e*))
        ((or) #f)
        ((or ,[e*] ... ,[e0])
         (foldr (lambda (e b)
                  (define v (gensym))
                  `(let ((,v ,e))
                     (if ,v
                         ,v
                         ,b)))
                e0 e*))
        ((if ,[e0] ,[e1])
         `(if ,e0 ,e1 'none))))
