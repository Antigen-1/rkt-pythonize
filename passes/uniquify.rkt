#lang racket/base
(require nanopass/base uuid "../core/main.rkt")
(provide L1 parse-L1 unparse-L1
         (rename-out (n:uniquify uniquify)))

(define-language L1
  (extends L0)
  (terminals
   (- (primitive (pr)))
   (- (variable (x)))
   (+ (variable (x))))
  (Expr (e body)
        (- pr x)
        (+ x)))

(define-parser parse-L1 L1)

(define (variable? v)
  (symbol? v))

(define (uniquify code (table (hasheq)))
  (define-pass L1->L0 : L1 (ir) -> L0 ()
    (Expr : Expr (ir) -> Expr ()
          ((set! ,x ,e)
           (define sym (hash-ref table x x))
           `(set! ,sym ,e))
          (,x
           (define sym (hash-ref table x x))
           `,sym)
          ((lambda (,x* ...) ,body)
           (define new-symbols (map (lambda (x) (uuid-symbol)) x*))
           (define new-table
             (foldl (lambda (x new-sym table) (hash-set table x new-sym)) table x* new-symbols))
           `(lambda (,new-symbols ...)
              ,(uniquify body new-table)))))
  (L1->L0 code))
(define (n:uniquify code)
  (uniquify code))
