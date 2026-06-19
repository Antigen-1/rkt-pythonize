#lang racket/base
(require nanopass/base racket/list racket/match "quote.rkt")
(provide parse-L5 unparse-L5 L5 expand-let expand-let*)

(define-language L5
  (extends L4)
  (Expr (e body)
        (+
         (let ((x e) ...)
           body ...)
         (letrec ((x e) ...)
           body ...))))

(define-parser parse-L5 L5)

(define (expand-let* expr)
  (match expr
    [`(let* () ,body* ...)
     `(begin ,@(map expand-let* body*))]
    [`(let* ((,x ,e) ,rest ...) ,body* ...)
     `(let ((,x ,(expand-let* e)))
        ,(expand-let* `(let* ,rest ,@body*)))]
    [(? pair? p)
     (map expand-let* p)]
    [_ expr]))

(define-pass expand-let :
  L5 (ir) -> L4 ()
  (Expr : Expr (ir) -> Expr ()
        ((let ((,x ,[e]) ...)
           ,[body] ...)
         `((lambda (,x ...)
             ,body ...)
           ,e ...))
        ((letrec ((,x ,[e]) ...)
           ,[body] ...)
         (define nones (make-list (length x) `','none))
         `((lambda (,x ...)
             ((lambda ()
                (set! ,x ,e)
                ...))
             ,body ...)
           ,nones ...))))
