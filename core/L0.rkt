#lang racket/base
(require nanopass/base json racket/list racket/set)
(provide L0 unparse-L0 parse-L0 render-L0 primitives)

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

(define primitives '(
                     apply make-procedure make-python-procedure closure? vm-apply
                     dynamic-require
                     get-attribute set-attribute!
                     none
                     ! @ ? <! length
                     not
                     equal? eq?
                     + - * / quotient modulo negate
                     is-a? object-type stream-type
                     ))

(define variable?
  (lambda (v)
    (and (symbol? v) (not (primitive? v)))))
(define primitive?
  (lambda (v)
    (memq v primitives)))
(define datum?
  (lambda (v)
    (jsexpr? v #:null 'none)))

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

(define (free-variables e)
  (nanopass-case (L0 Expr) e
                 ((lambda (,x* ...)
                    ,e)
                  (set-subtract (free-variables e) (apply seteq x*)))
                 (,pr (seteq))
                 (,x (seteq x))
                 (',d (seteq))
                 ((begin ,body* ...)
                  (apply set-union (map free-variables body*)))
                 ((if ,e0 ,e1 ,e2)
                  (set-union (free-variables e0)
                             (free-variables e1)
                             (free-variables e2)))
                 ((set! ,x ,e)
                  (set-add (free-variables e) x))
                 ((,e0 ,e* ...)
                  (apply set-union (map free-variables (cons e0 e*))))))

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
                  (cond ((check-duplicates x* eq?)
                         =>
                         (lambda (sym)
                           (raise-syntax-error 'lambda (format "Duplicate identifier ~a" sym)))))
                  (hasheq 'type "lambda"
                          'args (map symbol->string x*)
                          'free (map symbol->string (set->list (set-subtract (free-variables body) (apply seteq x*))))
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
                        'free '()
                        'body (hasheq 'type "var"
                                      'name "x")))
  (check-exn exn:fail:syntax? (lambda () (render-L0 (parse-L0 '(lambda (x x) x))))))
