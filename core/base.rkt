#lang racket/base
(require nanopass/base json racket/contract)
(provide LB unparse-LB parse-LB render-LB primitives lambda-arguments (contract-out (current-primitives (parameter/c (listof symbol?)))))

(define-language LB
  (entry Expr)
  (terminals
    (variable (x))
    (primitive (pr))
    (datum (d)))
  (Argument (a)
    (box x)
    x)
  (Expr (e body)
    x
    pr
    'd
    (if e0 e1 e2)
    (lambda (a* ...) body)
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

(define variable?
  (lambda (v)
    (and (symbol? v) (not (primitive? v)))))
(define primitive?
  (lambda (v)
    (memq v (current-primitives))))
(define datum?
  (lambda (v)
    (jsexpr? v #:null 'none)))

(define (lambda-arguments ast)
  (nanopass-case (LB Expr) ast
    ((lambda (,a* ...) ,body)
     (map 
      (lambda (a) 
        (nanopass-case (LB Argument) a
          ((box ,x) x)
          (,x x)))
      a*))))

(define-parser parse-LB LB)

(define (render-LB ast)
  (nanopass-case (LB Expr) ast
                 (,pr (hasheq 'type "prim"
                              'name (symbol->string pr)))
                 (,x (hasheq 'type "var"
                             'name (symbol->string x)))
                 (',d (hasheq 'type "datum"
                              'value `,d))
                 ((if ,e0 ,e1 ,e2)
                  (hasheq 'type "if"
                          'cond (render-LB e0)
                          'then (render-LB e1)
                          'otherwise (render-LB e2)))
                 ((lambda (,a* ...) ,body)
                  (hasheq 'type "lambda"
                          'args (map
                                 (lambda (a)
                                  (nanopass-case (LB Argument) a
                                    ((box ,x) (hasheq 'type "boxed" 'name (symbol->string x)))
                                    (,x (hasheq 'type "unboxed" 'name (symbol->string x)))))
                                 a*)
                          'body (render-LB body)))
                 ((,e0 ,e* ...)
                  (hasheq 'type "app"
                          'func (render-LB e0)
                          'args (map render-LB e*)))
                 (else (raise-argument-error 'render-LB "A LB expression" ast))))

(module+ test
  (require rackunit)
  (check-equal? (render-LB 'x)
                (hasheq 'type "var"
                        'name "x"))
  (check-equal? (render-LB 'dynamic-require)
                (hasheq 'type "prim"
                        'name "dynamic-require"))
  (check-equal? (render-LB (parse-LB '(x y)))
                (hasheq 'type "app"
                        'func (hasheq 'type "var" 'name "x")
                        'args (list (hasheq 'type "var" 'name "y"))))
  (check-equal? (render-LB (parse-LB ''1))
                (hasheq 'type "datum"
                        'value 1))
  (check-equal? (render-LB (parse-LB ''(1 2 "")))
                (hasheq 'type "datum"
                        'value '(1 2 "")))
  (check-equal? (render-LB (parse-LB '(lambda (x (box y)) x)))
                (hasheq 'type "lambda"
                        'args (list (hasheq 'type "unboxed" 'name "x")
                                    (hasheq 'type "boxed" 'name "y"))
                        'body (hasheq 'type "var"
                                      'name "x")))
  (check-equal? (parameterize ((current-primitives '(a)))
                  (render-LB (parse-LB 'a)))
                (hasheq 'type "prim"
                        'name "a"))
  )
