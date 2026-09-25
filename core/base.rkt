#lang racket/base

;; The LB language: the language the transpiler compiles.
;;
;; A program is a statement; a file with several top-level forms is read as
;; `(begin form ...)`.
;;
;; Statements and expressions are different things here, and the grammar says
;; so.  A statement is what a body, a `begin` in statement position, and the
;; branch of a statement `if` are made of; an expression is what `if`, the
;; argument of a call, and a `begin` in expression position are made of.  A
;; definition, an assignment and a multi-body `with-handler` or `trampoline`
;; are statements, so they cannot turn up in the middle of an expression and
;; mean something else there.
;;
;; Two notes about the grammar below:
;;
;; * The statement forms come before the expression production of `Stmt`: a form
;;   whose head is a keyword has to be read as that form, not as an application
;;   of a variable that happens to share the name.
;; * nanopass reads a statement where an expression is expected happily enough,
;;   and hands the compiler a statement node.  The compiler is where that is
;;   refused, with a message that says what happened.
;;
;; `import` is not a form: a Python module is a value, so `import-module` (the
;; runtime function around `importlib.import_module`) is how a program gets one.

(require nanopass/base
         racket/list)

(provide LB
         parse-LB
         unparse-LB
         variable?
         literal?
         datum?
         procedure-signature?
         binding?
         binding-name
         binding-parts)

(define-language LB
  (entry Stmt)
  (terminals
   (variable (x))
   (literal (l))
   (datum (d))
   (binding (b)))
  (Stmt (s body)
        ;; `define` has one shape, not two.  A binding is either a variable or
        ;; the signature of a procedure, and one terminal covers both: two
        ;; three-element productions would not work, because nanopass does not
        ;; fall back to the next production when a form matches the shape of one
        ;; but fails its terminal check.
        (define b body)
        (set! x e)
        (begin s ...)
        (if e1 s1 s2)
        e)
  (Expr (e)
        x
        l
        'd
        (if e1 e2 e3)
        (begin e1 e* ...)
        (with-handler e1 e2)
        (trampoline e1)
        (raise e1)
        (e0 e* ...)))

;; A variable is just a symbol: anything that is not a literal or a form is a
;; name, and the code generator decides later what a name means.
(define (variable? v)
  (symbol? v))

;; Self-evaluating values.  Lists and symbols need a quote, exactly as in
;; Scheme; tuples and dicts do not.
(define (literal? v)
  (or (integer? v)
      (flonum? v)
      (string? v)
      (boolean? v)
      (and (vector? v) (andmap datum? (vector->list v)))
      (and (hash? v) (andmap datum? (hash-keys v)) (andmap datum? (hash-values v)))))

;; The signature of a procedure: (name param ...) or (name param ... . rest).
;; The rest parameter collects the remaining arguments into a list.
(define (procedure-signature? v)
  (and (pair? v)
       (variable? (car v))
       (let loop ([params (cdr v)])
         (cond [(null? params) #t]
               [(pair? params) (and (variable? (car params)) (loop (cdr params)))]
               [else (variable? params)]))))

;; What a `define` binds: a variable, or a procedure signature.
(define (binding? v)
  (or (variable? v) (procedure-signature? v)))

;; The name a binding binds.
(define (binding-name b)
  (if (pair? b) (car b) b))

;; The parameters of a procedure binding, as (values fixed rest): the parameters
;; before the dot, and the rest parameter or #f.
(define (binding-parts b)
  (let loop ([params (cdr b)] [fixed '()])
    (cond [(null? params) (values (reverse fixed) #f)]
          [(pair? params) (loop (cdr params) (cons (car params) fixed))]
          [else (values (reverse fixed) params)])))

;; Everything a quote may produce.
(define (datum? v)
  (or (integer? v)
      (flonum? v)
      (string? v)
      (boolean? v)
      (symbol? v)
      (and (list? v) (andmap datum? v))
      (and (vector? v) (andmap datum? (vector->list v)))
      (and (hash? v) (andmap datum? (hash-keys v)) (andmap datum? (hash-values v)))))

(define-parser parse-LB LB)
