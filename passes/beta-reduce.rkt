#lang racket/base
(require nanopass/base "cps.rkt")
(provide L1 parse-L1 unparse-L1 beta-reduce)

(define (get-references expr id)
  (define cnt (box null))
  (define-pass helper :
    L1 (ir) -> L1 ()
    (Expr : Expr (ir) -> Expr ()
          (,x (if (eq? x id)
                  (set-box! cnt (cons 'ref (unbox cnt)))
                  (void))
              `,x)
          ((set! ,x ,[e])
           (if (eq? x id)
               (set-box! cnt (cons 'set (unbox cnt)))
               (void))
           `(set! ,x ,e))))
  (helper expr)
  (unbox cnt))
(define (replace-id-with expr id val)
  (define-pass helper :
    L1 (ir) -> L1 ()
    (Expr : Expr (ir) -> Expr ()
          (,x (if (eq? x id)
                  `,val
                  `,x))))
  (helper expr))
(define (immediate? expr)
  (nanopass-case (L1 Expr) expr
                 (,x #t)
                 (,pr #t)
                 (',d #t)
                 ((lambda (,x* ...) ,e) #t)
                 (else #f)))

(define-pass beta-reduce :
  L1 (ir) -> L1 ()
  (Expr : Expr (ir) -> Expr ()
        ((let/cc ,x ,e)
         (if (= 0 (length (get-references e x)))
             `,e
             `(let/cc ,x ,e)))
        (((lambda (,x* ...)
            ,[e])
          ,[e*] ...)
         (if (= (length x*) (length e*))
             (letrec ((us (map (lambda (x) (get-references e x)) x*))
                      ;; Remove unused identifiers
                      (table (foldr
                              (lambda (x u e t)
                                (if (and (= 0 (length u)) (immediate? e))
                                    t
                                    (cons (list x u e) t)))
                              null
                              x* us e*))
                      (nx* (map car table))
                      (nu* (map cadr table))
                      (ne* (map caddr table))
                      ;; Replace identifers that are not assigned using set! with their values
                      (ntable (foldr
                               (lambda (x u e t)
                                 (if (and (not (memq 'set u))
                                          (= (length u) 1)
                                          (immediate? e))
                                     (cons (replace-id-with (car t) x e)
                                           (cdr t))
                                     (cons (car t) (cons (list x u e) (cdr t)))))
                               (cons e null)
                               nx* nu* ne*))
                      (ne (car ntable))
                      (nnx* (map car (cdr ntable)))
                      (nnu* (map cadr (cdr ntable)))
                      (nne* (map caddr (cdr ntable))))
               (if (= (length nnx*) 0)
                   `,ne
                   `((lambda (,nnx* ...)
                       ,ne)
                     ,nne* ...)))
             `((lambda (,x* ...)
                 ,e)
               ,e* ...)))))
