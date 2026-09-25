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
         datum?)

(define-language LB
  (entry Expr)
  (terminals
   (variable (x))
   (literal (l))
   (datum (d)))
  (Expr (e body)
        x
        l
        'd
        ;; NOTE: the function shape has to be listed before (define x e): both
        ;; are three elements long, and nanopass does not fall back to the next
        ;; production once a production has matched the shape of a form but
        ;; failed one of its terminal checks.
        (define (x x* ...) e)
        (define x e)
        (trampoline body ...)
        (set! x e)
        (raise e)
        (with-handler e1 e2)
        (begin e ...)
        (if e1 e2 e3)
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
