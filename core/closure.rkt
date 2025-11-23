#lang racket/base
(require "base.rkt" nanopass/base racket/set racket/list)
(provide LC unparse-LC parse-LC analyze-closure lambda-argument-symbol)

(define-language LC 
    (entry Expr)
    (extends LB)
    (terminals (+ (variable (x))))
    (Argument (a) (+ x (box x)))
    (Expr (e body) 
        (- (ref d))
        (+ x (lambda (a* ...) body))))

(define-parser parse-LC LC)

(define variable?
  (lambda (v)
    (and (symbol? v) (not (memq v primitives)))))

(define (lambda-argument-symbol as)
    (map (lambda (a) (nanopass-case (LC Argument) a ((box ,x) x) (,x x))) as))
(define (render-argument a)
    (nanopass-case (LC Argument) a
        ((box ,x) (hasheq 'type "boxed"))
        (,x (hasheq 'type "unboxed"))))

(define (free-variables ast)
    (nanopass-case (LC Expr) ast
        (,x (seteq x))
        (,pr (seteq))
        (',d (seteq))
        ((if ,e0 ,e1 ,e2) (apply set-union (map free-variables (list e0 e1 e2))))
        ((,e0 ,e* ...) (apply set-union (map free-variables (cons e0 e*))))
        ((lambda (,a* ...) ,body) (set-subtract (free-variables body) (apply seteq (lambda-argument-symbol a*))))))

(module+ test
    (require rackunit)
    (check-equal? (free-variables (parse-LC '(lambda (x (box y)) z)))
                  (seteq 'z)))

(define (analyze-closure ast (table (hasheq)))
    (define-pass LC->LB : LC (ir) -> LB ()
        (Expr : Expr (ir) -> Expr ()
            ((lambda (,a* ...) ,body)
             (let* (;; Relocate free variables
                    (as (lambda-argument-symbol a*))
                    (fs (set->list (free-variables ast)))
                    (an (length a*))
                    (fn (length fs))
                    (fi (map (lambda (f) (hash-ref table f (lambda () (raise-syntax-error 'analyze-closure (format "variable ~a not found" f))))) fs))
                    (ntable1 (foldl (lambda (fv ind t) (hash-set t fv ind)) (hasheq) fs (range 0 fn)))
                    ;; Locate current arguments
                    (ntable2 (foldl (lambda (s i t) (hash-set t s (+ i fn))) ntable1 as (range 0 an))))
                `(closure
                  ;; arguments
                  ,(map render-argument a*) 
                  ;; free variables
                  ,fi 
                  ;; codes
                  ,(render-LB (analyze-closure body ntable2)))))
            (,x `(ref ,(hash-ref table x (lambda () (raise-syntax-error 'analyze-closure (format "variable ~a not found" x))))))))
    (LC->LB ast))