#lang racket/base
(require nanopass/base "../core/main.rkt" racket/list)
(provide L0 parse-L0 unparse-L0 L0-uniquify)

(define (L0-uniquify code (env (hasheq)))
  (define-pass helper :
    L0 (ir) -> L0 ()
    (Expr : Expr (ir) -> Expr ()
          ((lambda (,x* ...) ,body)
           (cond ((check-duplicates x* eq?)
                  =>
                  (lambda (sym) (raise-syntax-error
                                 'lambda
                                 "Duplicate identifier ~a"
                                 sym))))
           (define rx* (map gensym x*))
           (define nenv
             (foldl
              (lambda (x rx env)
                (hash-set env x rx))
              env
              x* rx*))
           `(lambda (,rx* ...) ,(L0-uniquify body nenv)))
          ((set! ,x ,[e])
           `(set! ,(hash-ref env x (lambda () (raise-syntax-error 'L0-uniquify (format "Variable ~a not found" x)))) ,e))
          (,x `,(hash-ref env x (lambda () (raise-syntax-error 'L0-uniquify (format "Variable ~a not found" x)))))
          ))
  (helper code))
