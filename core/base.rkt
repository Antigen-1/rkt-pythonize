#lang racket/base

;; LB: the one and only language of this project.
;;
;; The source syntax is Racket's s-expression syntax, so `read` *is* the front
;; end -- there is no lexer of our own.  The inspiration taken from Clojure is
;; only the shape of the transpiler: stay small, avoid clever transformations,
;; and generate Python that a human can still read.  There is no CPS conversion
;; and no Python runtime library: every line of the generated program comes from
;; this language.
;;
;;   e ::= x                        variable
;;       | l                        self-evaluating literal
;;       | 'd                       quoted datum
;;       | (define x e)             bind a value
;;       | (define (x x* ...) e)    bind a procedure
;;       | (trampoline e ...)       trampoline boundary: a tail call inside it
;;                                  returns a thunk instead of growing the
;;                                  Python stack
;;       | (set! x e)               assign
;;       | (raise e)                raise an exception
;;       | (with-handler e1 e2)     call e2 with e1 installed as the handler
;;                                  that `raise` reports to
;;       | (begin e ...)            sequence
;;       | (if e1 e2 e3)            conditional
;;       | (e0 e* ...)              application
;;
;;   d ::= int | float | string | boolean | symbol | list | tuple | dict
;;   l ::= int | float | string | boolean | tuple | dict   (self-evaluating)
;;
;;   int     1   -2   +3
;;   float   1.0   -2.5e3   .5
;;   string  "a\n"
;;   boolean #t   #f
;;   symbol  foo
;;   list    (1 2 3)
;;   tuple   #(1 2 3)
;;   dict    #hash((a . 1) ("b" . 2))
;;
;; A program is one expression; a file with several top-level forms is read as
;; `(begin form ...)`.

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
         import-spec?
         binding-name
         binding-parts)

(define-language LB
  (entry Expr)
  (terminals
   (variable (x))
   (literal (l))
   (datum (d))
   (binding (b))
   (import-spec (spec)))
  (Expr (e body)
        x
        l
        'd
        ;; `define` has one shape, not two.  A binding is either a variable or
        ;; the signature of a procedure, and one terminal covers both: two
        ;; three-element productions would not work, because nanopass does not
        ;; fall back to the next production when a form matches the shape of one
        ;; but fails its terminal check.
        (define b e)
        (trampoline body ...)
        (set! x e)
        (raise e)
        (with-handler e1 e2)
        (begin e ...)
        (if e1 e2 e3)
        ;; like every keyword form, `import` has to be listed before the
        ;; application production, whose shape it shares
        (import spec* ...)
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

;; What an `import` imports: a module, a module under another name, or names a
;; module exports.
;;
;;   os                    -> import os
;;   (as os.path path)     -> import os.path as path
;;   (ref math sqrt pi)    -> from math import sqrt, pi
(define (import-spec? v)
  (or (variable? v)
      (and (list? v)
           (pair? v)
           (case (car v)
             [(as) (and (= 3 (length v)) (variable? (cadr v)) (variable? (caddr v)))]
             [(ref) (and (>= (length v) 3) (andmap variable? (cdr v)))]
             [else #f]))))

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
