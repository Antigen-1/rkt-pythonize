#lang racket/base
(require nanopass/base racket/list "quote.rkt")
(provide parse-L5 unparse-L5 L5 expand-let)

(define-language L5
  (extends L4)
  (Expr (e body)
        (+
         (let ((x e) ...)
           body ...)
         (letrec ((x e) ...)
           body ...))))

(define-parser parse-L5 L5)

(define (expand-let code)
  (define none-sym (gensym 'none))
  (define-pass L5->L4 :
    L5 (ir) -> L4 ()
    (Expr : Expr (ir) -> Expr ()
          ((let ((,x ,[e]) ...)
             ,[body] ...)
           `((lambda (,x ...)
               ,body ...)
             ,e ...))
          ((letrec ((,x ,[e]) ...)
             ,[body] ...)
           (define nones (make-list (length x) none-sym))
           `((lambda (,x ...)
               ((lambda ()
                  (set! ,x ,e)
                  ...))
               ,body ...)
             ,nones ...))))
  (L5->L4 (parse-L5 `(let ((,none-sym none))
                       ,(unparse-L5 code)))))
