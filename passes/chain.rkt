#lang racket/base
(require nanopass/base "cond.rkt")
(provide L8 parse-L8 unparse-L8 expand-chain)

(define-language L8
  (extends L7)
  (terminals (+ (name (nm))))
  (Expr (e body)
        (+ (=> e0 nm* ...)
           (=>! e0 nm* ... nm0 e1))))

(define name? string?)

(define-parser parse-L8 L8)

(define (expand-chain code)
  (define get-attribute-sym (gensym 'get-attribute))
  (define set-attribute-sym (gensym 'set-attribute!))
  (define-pass L8->L7 :
    L8 (ir) -> L7 ()
    (Expr : Expr (ir) -> Expr ()
          ((=> ,[e0] ,nm* ...)
           (foldr
            (lambda (name expr)
              `(,get-attribute-sym ,expr ,name))
            e0 nm*))
          ((=>! ,[e0] ,nm* ... ,nm0 ,[e1])
           `(,set-attribute-sym
             ,(foldr
               (lambda (name expr)
                 `(,get-attribute-sym ,expr ,name))
               e0 nm*)
             ,nm0
             ,e1))))
  (L8->L7
   (parse-L8
    `(let ((,get-attribute-sym get-attribute)
           (,set-attribute-sym set-attribute!))
       ,(unparse-L8 code)))))
