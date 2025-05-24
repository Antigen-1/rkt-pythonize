#lang racket/base
(require nanopass/base uuid "cps.rkt")
(provide L2 parse-L2 unparse-L2
         (rename-out (n:uniquify uniquify)))

(define-language L2
  (extends L1)
  (terminals
   (- (primitive (pr)))
   (- (variable (x)))
   (+ (variable (x))))
  (Expr (e body)
        (- pr x)
        (+ x)))

(define-parser parse-L2 L2)

(define (variable? v)
  (symbol? v))

(define (uniquify code (table (hasheq)))
  (define-pass L2->L1 : L2 (ir) -> L1 ()
    (Expr : Expr (ir) -> Expr ()
          ((set! ,x ,[e])
           (define sym (hash-ref table x x))
           `(set! ,sym ,e))
          (,x
           (define sym (hash-ref table x x))
           `,sym)
          ((let/cc ,x ,body)
           (define new-symbol (uuid-symbol))
           `(let/cc ,new-symbol
              ,(uniquify body (hash-set table x new-symbol))))
          ((lambda (,x* ...) ,body)
           (define new-symbols (map (lambda (x) (uuid-symbol)) x*))
           (define new-table
             (foldl (lambda (x new-sym table) (hash-set table x new-sym)) table x* new-symbols))
           `(lambda (,new-symbols ...)
              ,(uniquify body new-table)))))
  (L2->L1 code))
(define (n:uniquify code)
  (uniquify code))
