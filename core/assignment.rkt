#lang racket/base
(require "closure.rkt" racket/set nanopass/base)
(provide LS unparse-LS parse-LS expand-assignments)

(define-language LS 
    (extends LC)
    (Expr (e body) 
        (- (lambda (a* ...) body))
        (+ (set! x e)
           (lambda (x* ...) body))))

(define-parser parse-LS LS)

(define (find-assignments ast)
    (define assignments (mutable-seteq))
    (define-pass helper : LS (ir) -> LS ()
        (Expr : Expr (ir) -> Expr ()
            ((set! ,x ,[e])
             (set-add! assignments x)
             `(set! ,x ,e))))
    (helper ast)
    assignments)

(define (expand-assignments ast)
    (define assignments (find-assignments ast))
    (define-pass helper : LS (ir) -> LC ()
        (Expr : Expr (ir) -> Expr ()
            (,x 
             (define id (gensym 'x))
             (if (set-member? assignments x) `(unbox (lambda (,id) ,id) ,x) `,x))
            ((set! ,x ,[e])
             (define id (gensym 'x))
             `(set-box! (lambda (,id) ,id) ,x ,e))
            ((lambda (,x* ...) ,[body])
             (parse-LC (list 'lambda (map (lambda (x) (if (set-member? assignments x) (list 'box x) x)) x*) (unparse-LC body))))))
    (helper ast))