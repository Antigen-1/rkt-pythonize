#lang racket/base
(require nanopass/base "chain.rkt")
(provide expand-vm L9 parse-L9 unparse-L9)

(define-language L9
  (extends L8)
  (terminals (+ (arity (ar))))
  (Expr (e body)
        (+ (#%vm-procedure e ar)
           (#%scm-procedure e ar))))

(define (arity? v)
  (or (not v) (exact-integer? v)))

(define-parser parse-L9 L9)

(define (expand-vm code)
  (define mp-sym (gensym 'make-procedure))
  (define mpp-sym (gensym 'make-python-procedure))
  (define va-sym (gensym 'vm-apply))
  (define ad-sym (gensym '<!))
  (define-pass L9->L8 :
    L9 (ir) -> L8 ()
    (Expr : Expr (ir) -> Expr ()
          ((#%vm-procedure ,[e] ,ar)
           (if ar
               (let* ((x* (build-list ar (lambda (_) (gensym 'x))))
                      (l-sym (gensym 'l))
                      (steps (map (lambda (x) `(,ad-sym ,l-sym ,x)) x*))
                      (v-sym (gensym 'v)))
                 `(let ((,v-sym ,e))
                    (lambda (,x* ...)
                      (let ((,l-sym '()))
                        ,steps ...
                        (,va-sym ,v-sym ,l-sym)))))
               (let ((l-sym (gensym 'l))
                     (v-sym (gensym 'v)))
                 `(let ((,v-sym ,e))
                    (,mp-sym
                     (lambda (,l-sym)
                       (,va-sym ,v-sym ,l-sym)))))))
          ((#%scm-procedure ,[e] ,ar)
           `(,mpp-sym ,e ,ar))))
  (L9->L8
   (parse-L9
    `(let ((,mp-sym make-procedure)
           (,mpp-sym make-python-procedure)
           (,va-sym vm-apply)
           (,ad-sym <!))
       ,(unparse-L9 code)))))
