#lang racket/base
(require nanopass/base "vm.rkt")
(provide L10 parse-L10 unparse-L10 expand-stream)

(define-language L10
  (extends L9)
  (Expr (e body)
        (+ (stream-cons e0 e1)
           (stream-car e)
           (stream-cdr e))))

(define-parser parse-L10 L10)

(define (expand-stream code)
  (define make-stream-sym (gensym 'make-stream))
  (define get-car-sym (gensym 'get-car))
  (define get-cdr-sym (gensym 'get-cdr))
  (define-pass L10->L9 : L10 (ir) -> L9 ()
    (Expr : Expr (ir) -> Expr ()
          ((stream-cons ,[e0] ,[e1])
           (define result-sym (gensym 'result))
           (define stream-sym (gensym 'stream))
           `(letrec ((,stream-sym
                      (,make-stream-sym
                       (let ((,result-sym 'none))
                         (lambda ()
                           (set! ,result-sym ,e0)
                           (=>! ,stream-sym "car" (lambda () ,result-sym))
                           ,result-sym))
                       (let ((,result-sym 'none))
                         (lambda ()
                           (set! ,result-sym ,e1)
                           (=>! ,stream-sym "cdr" (lambda () ,result-sym))
                           ,result-sym)))))
              ,stream-sym))
          ((stream-car ,[e])
           `((,get-car-sym ,e)))
          ((stream-cdr ,[e])
           `((,get-cdr-sym ,e)))))
  (L10->L9
   (parse-L10
    `(let ((,make-stream-sym (#%vm-procedure stream-type 2))
           (,get-car-sym (lambda (o) (=> o "car")))
           (,get-cdr-sym (lambda (o) (=> o "cdr")))
           )
       ,(unparse-L10 code)))))
