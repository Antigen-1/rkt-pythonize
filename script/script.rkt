#lang racket/base
(require "../passes/main.rkt" nanopass/base racket/list)
(provide LS parse-LS unparse-LS LS->L)

(define-language LS 
    (extends L)
    (entry Script)
    (Definition (def)
        (+ (define x e)))
    (Statement (st)
        (+ def e))
    (Script (sc)
        (+ (#%script-begin st ...))))

(define-parser parse-LS LS)

(define-pass LS->L :
    LS (ir) -> L ()
    (Script : Script (ir) -> Expr ()
        ((#%script-begin ,st ...)
         (let*-values (((exprs defs)
                        (partition
                            (lambda (s) 
                                (nanopass-case (LS Statement) s
                                    (,e #t)
                                    (else #f)))
                            st))
                       ((id+vals)
                        (map 
                         (lambda (d)
                            (nanopass-case (LS Definition) d
                                ((define ,x ,e)
                                 (cons x e))))
                         defs)))
            (cond ((check-duplicates id+vals eq? #:key car)
                   =>
                   (lambda (p)
                     (define id (car p))
                     (raise-syntax-error 'LS->L (format "Duplicate identifier ~a" id)))))
            (parse-L
                (list 'letrec (map (lambda (p) (list (car p) (unparse-LS (cdr p)))) id+vals)
                      (cons 'begin (map unparse-LS exprs))))))))