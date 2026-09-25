#lang racket/base

;; LB -> LB: refuse a statement where an expression belongs.
;;
;; The grammar keeps statements and expressions apart, but nanopass reads a
;; statement in an expression position happily enough -- and the shape of
;; (define x e) is the shape of a call.  This pass is where that is caught, so
;; that the compiler can take an expression for an expression.  It changes
;; nothing: it answers the program it was given.

(require nanopass/base
         racket/list
         "../core/base.rkt")

(provide check-expressions)

(define (check-expressions program)
  (check-stmt program)
  program)

(define (check-stmt s)
  (nanopass-case (LB Stmt) s
    ;; a procedure has a body of statements; a value has one expression
    ((define ,b ,body) (if (pair? b) (check-stmt body) (check-expr body)))
    ((set! ,x ,e1) (check-expr e1))
    ((begin ,s* ...) (for-each check-stmt s*))
    ((if ,e1 ,s1 ,s2) (check-expr e1) (check-stmt s1) (check-stmt s2))
    (else (check-expr s))))

(define (check-expr e)
  (nanopass-case (LB Expr) e
    ((if ,e1 ,e2 ,e3) (check-expr e1) (check-expr e2) (check-expr e3))
    ((begin ,e1 ,e* ...) (check-expr e1) (for-each check-expr e*))
    ((with-handler ,e1 ,e2) (check-expr e1) (check-expr e2))
    ((trampoline ,e1) (check-expr e1))
    ((raise ,e1) (check-expr e1))
    ((,e0 ,e* ...) (check-callee e0) (check-expr e0) (for-each check-expr e*))
    (,x (void))
    (,l (void))
    (',d (void))
    ;; what is left is a statement: the grammar has no expression production
    ;; for it, so it can only be here by being written here
    (else
     (error 'check-expressions "a statement cannot be used as an expression: ~a" e))))

;; A form whose head is a statement keyword, where an expression was wanted.
(define (check-callee e0)
  (when (symbol? e0)
    (case e0
      [(define set!)
       (error 'check-expressions "a statement cannot be used as an expression: ~a" e0)]
      [(import)
       (error 'check-expressions
              "import is not a form: (import-module \"name\") is how a module becomes a value")]
      [(trampoline with-handler)
       (error 'check-expressions
              "~a takes one body in LB: LE, the language a source is written in, takes any number"
              e0)]
      [else (void)])))
