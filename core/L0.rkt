#lang racket/base
(require nanopass/base)
(provide L0 unparse-L0 parse-L0 render-L0)

(define-language L0
  (terminals
    (variable (x))
    (primitive (pr))
    (datum (d)))
  (Expr (e body)
    x
    pr
    'd
    (begin e* ...)
    (if e0 e1 e2)
    (lambda (x* ...) body)
    (set! x e)
    (e0 e* ...)))

(define variable?
  (lambda (v)
    (and (symbol? v) (not (primitive? v)))))
(define primitive?
  (lambda (v)
    (memq v '(apply
              dynamic-require
              get-attribute set-attribute!
              none object-type
              closure? vm-apply
              ! @ <!
              equal? eq?
              + - * / quotient modulo negate
              is-a?))))
(define datum?
  (lambda (v)
    (or (flonum? v)
        (fixnum? v)
        (string? v)
        (boolean? v)
        (and (list? v)
             (andmap datum? v)))))

(define-parser parse-L0 L0)

(module+ test
  (require rackunit)
  (let ((form '(begin (if '1
                          (lambda (x) x)
                          (set! x ((lambda (y) y) '2))))))
    (check-equal?
     form
     (unparse-L0 (parse-L0 form))))
  )

(define (render-L0 ast)
  (nanopass-case (L0 Expr) ast
                 (,pr (hasheq 'type "prim"
                              'name (symbol->string pr)))
                 (,x (hasheq 'type "var"
                             'name (symbol->string x)))
                 (',d (hasheq 'type "datum"
                              'value `,d))
                 ((begin ,e* ...)
                  (hasheq 'type "begin"
                          'seq (map render-L0 e*)))
                 ((if ,e0 ,e1 ,e2)
                  (hasheq 'type "if"
                          'cond (render-L0 e0)
                          'then (render-L0 e1)
                          'otherwise (render-L0 e2)))
                 ((lambda (,x* ...) ,body)
                  (hasheq 'type "lambda"
                          'args (map symbol->string x*)
                          'body (render-L0 body)))
                 ((set! ,x ,e)
                  (hasheq 'type "set!"
                          'var (symbol->string x)
                          'value (render-L0 e)))
                 ((,e0 ,e* ...)
                  (hasheq 'type "app"
                          'func (render-L0 e0)
                          'args (map render-L0 e*)))
                 (else (raise-argument-error 'render-L0 "A L0 expression" ast))))

(module+ test
  (check-equal? (render-L0 'x)
                (hasheq 'type "var"
                        'name "x"))
  (check-equal? (render-L0 'dynamic-require)
                (hasheq 'type "prim"
                        'name "dynamic-require"))
  (check-equal? (render-L0 (parse-L0 '(x y)))
                (hasheq 'type "app"
                        'func (hasheq 'type "var" 'name "x")
                        'args (list (hasheq 'type "var" 'name "y"))))
  (check-equal? (render-L0 (parse-L0 ''1))
                (hasheq 'type "datum"
                        'value 1))
  (check-equal? (render-L0 (parse-L0 ''(1 2 "")))
                (hasheq 'type "datum"
                        'value '(1 2 "")))
  (check-equal? (render-L0 (parse-L0 '(lambda (x) x)))
                (hasheq 'type "lambda"
                        'args '("x")
                        'body (hasheq 'type "var"
                                      'name "x"))))
