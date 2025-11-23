#lang racket/base
(require "closure.rkt" racket/set nanopass/base)
(provide (all-defined-out))

(define (check-uniquification ast)
    (define variables (mutable-seteq))
    (define-pass helper : LC (ir) -> LC ()
        (Expr : Expr (ir) -> Expr ()
            ((lambda (,a* ...) ,[body])
             (for-each 
              (lambda (x) 
                (if (and (not (set-member? variables x)) (set-add! variables x) #t)
                    (void)
                    (raise-syntax-error 'check-uniquification (format "Duplicate variable ~a" x))))
              (lambda-argument-symbol a*))
             `(lambda (,a* ...) ,body))))
    (helper ast))