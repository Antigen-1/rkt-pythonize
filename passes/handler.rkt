#lang racket/base
(require nanopass/base "cond-explicit.rkt")
(provide L13 parse-L13 unparse-L13 expand-exn-handler)

(define-language L13 
    (extends L12)
    (Expr (e body)
        (+ (with-handler e body ...)
           (throw e))))

(define-parser parse-L13 L13)

(define (expand-exn-handler code)
    (define throw-sym (gensym 'throw))
    (define return1-sym (gensym 'return1))
    (define return2-sym (gensym 'return2))
    (define-pass helper
        : L13 (ir) -> L12 ()
        (Expr : Expr (ir) -> Expr ()
            ((throw ,[e]) `(,throw-sym ,e))
            ((with-handler ,[e] ,[body] ...)
             `(let/cc ,return1-sym
                (let ((,throw-sym (let/cc ,return2-sym (,return1-sym (,e (let/cc cc (,return2-sym cc)))))))
                    ,body ...)))
        )
    )
    ;; `raise` serves as the default error handler
    (helper (parse-L13 `(let ((,throw-sym raise)) ,(unparse-L13 code))))
)