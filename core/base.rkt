#lang racket/base
(require nanopass/base json racket/contract)
(provide LB unparse-LB parse-LB render-LB primitives (contract-out (current-primitives (parameter/c (listof symbol?)))))

(define-language LB
  (entry Expr)
  (terminals
    (primitive (pr))
    (datum (d)))
  (Expr (e body)
    pr
    'd
    (ref d)
    (closure d0 d1 d2)
    (if e0 e1 e2)
    (e0 e* ...)))

(define primitives '(
                     apply make-procedure make-python-procedure vm-apply
                     dynamic-require
                     get-attribute set-attribute!
                     none
                     ! @ ? <! length set-box! unbox box
                     not
                     equal? eq?
                     + - * / quotient modulo negate
                     is-a? object-type stream-type box-type
                     ))

(define current-primitives (make-parameter primitives))

(define primitive?
  (lambda (v)
    (memq v (current-primitives))))
(define datum?
  (lambda (v)
    (jsexpr? v #:null 'none)))

(define-parser parse-LB LB)

(define (render-LB ast)
  (nanopass-case (LB Expr) ast
                 (,pr (hasheq 'type "prim"
                              'name (symbol->string pr)))
                 (',d (hasheq 'type "datum"
                              'value `,d))
                 ((ref ,d) (hasheq 'type "ref"
                                   'location `,d))
                 ((closure ,d1 ,d2 ,d3)
                  (hasheq 'type "closure" 
                          'args d1
                          'free d2 
                          'code d3))
                 ((if ,e0 ,e1 ,e2)
                  (hasheq 'type "if"
                          'cond (render-LB e0)
                          'then (render-LB e1)
                          'otherwise (render-LB e2)))
                 ((,e0 ,e* ...)
                  (hasheq 'type "app"
                          'func (render-LB e0)
                          'args (map render-LB e*)))
                 (else (raise-argument-error 'render-LB "A LB expression" ast))))

(module+ test
  (require rackunit)
  (check-equal? (render-LB 'dynamic-require)
                (hasheq 'type "prim"
                        'name "dynamic-require"))
  (check-equal? (render-LB (parse-LB ''1))
                (hasheq 'type "datum"
                        'value 1))
  (check-equal? (render-LB (parse-LB ''(1 2 "")))
                (hasheq 'type "datum"
                        'value '(1 2 "")))
  (check-equal? (parameterize ((current-primitives '(a)))
                  (render-LB (parse-LB 'a)))
                (hasheq 'type "prim"
                        'name "a"))
  )
