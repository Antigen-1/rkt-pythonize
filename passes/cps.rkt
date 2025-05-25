#lang racket/base
(require nanopass/base uuid "../core/L0.rkt")
(provide L1 parse-L1 unparse-L1 cps)

(define-language L1
  (extends L0)
  (Expr (e body)
        (+ (let/cc x body))))

(define-parser parse-L1 L1)

(define-pass cps-transform : L1 (ir) -> L0 ()
  (Expr : Expr (ir) -> Expr ()
        (,x
         `(lambda (cc)
            (cc ,x)))
        (,pr
         `(lambda (cc)
            (cc ,pr)))
        (',d
         `(lambda (cc)
            (cc ',d)))
        ((set! ,x ,[e])
         `(lambda (cc)
            (,e (lambda (value)
                  (cc (set! ,x value))))))
        ((lambda (,x* ...) ,[body*])
         (define cc (uuid-symbol))
         `(lambda (cc)
            (cc
             (lambda (,cc ,x* ...)
               (,body* ,cc)))))
        ((begin ,[e*] ...)
         `(lambda (cc)
            (,(foldr (lambda (e cont) `(lambda (_) (,e ,cont))) 'cc e*)
             none)))
        ((if ,[e0] ,[e1] ,[e2])
         `(lambda (cc)
            (,e0 (lambda (v0)
                   (if v0
                       (,e1 cc)
                       (,e2 cc))))))
        ((let/cc ,x ,[body])
         (define cc (uuid-symbol))
         `(lambda (,cc)
            ((lambda (,x) (,body ,cc))
             (lambda (_ x) (,cc x)))))
        ((,[e0] ,[e*] ...)
         (define syms (build-list (length (cons e0 e*))
                                  (lambda (_) (uuid-symbol))))
         (define cc (uuid-symbol))
         `(lambda (,cc)
            ,(foldr
              (lambda (sym e expr)
                `(,e (lambda (,sym)
                       ,expr)))
              `(,(car syms) ,cc ,(cdr syms) ...)
              syms (cons e0 e*))))))

(define (cps code)
  (define compiled (cps-transform code))
  (parse-L0
   `(,(unparse-L0 compiled)
     (lambda (x) x))))
