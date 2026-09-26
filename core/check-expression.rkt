#lang racket/base

;; LE -> LE, checking that statements and expressions stay in their own places,
;; and leaving the program as it is.
;;
;; A statement is what a body, a begin in statement position and the branch of a
;; statement if are made of; an expression is what an if, a call argument and a
;; begin in expression position are made of.  Nothing in an expression position
;; is a statement, so a definition cannot hide inside one and mean something
;; else there, and a body ends with the expression that is its value.

(require racket/list)

(provide check-expression-program)

(define (head stx) (and (pair? (syntax->list stx)) (syntax-e (car (syntax->list stx)))))
(define (statement-form? stx) (memq (head stx) '(define set!)))

(define (form-location stx)
  (define line (syntax-line stx))
  (if line
      (format "~a:~a:~a" (syntax-source stx) line (add1 (syntax-column stx)))
      "?"))

(define (not-le stx message)
  (error 'check-expression "~a: ~a: ~a" (form-location stx) message (syntax->datum stx)))

;; forms that are Racket's, not LE's
(define racket-forms
  '(let let* letrec let-values letrec-values let*-values let-syntax letrec-syntax
    cond when unless case do lambda define-syntax define-syntaxes
    define-syntax-rule define-for-syntax begin-for-syntax define-values require
    provide quasiquote unquote unquote-splicing syntax-rules syntax-case
    syntax-parse match match-lambda struct guard parameterize with-handlers
    module module+ case-lambda))

(define (check-expression-program forms)
  (for ([f (in-list forms)]) (check-statement f))
  forms)

;; a statement: an expression, a definition, an assignment, a begin or an if
(define (check-statement stx)
  (cond [(not (syntax->list stx)) (void)]
        [else
         (define parts (syntax->list stx))
         (case (head stx)
           [(define)
            (define target (cadr parts))
            (when (null? (cddr parts)) (not-le stx "a definition needs a body"))
            (if (identifier? target)
                (begin (when (not (null? (cdr (cddr parts))))
                         (not-le stx "a value definition takes one expression"))
                       (check-expression (caddr parts)))
                (check-body (cddr parts)))]
           [(set!)
            (when (not (= 3 (length parts))) (not-le stx "an assignment takes one expression"))
            (check-expression (caddr parts))]
           [(begin) (for ([f (in-list (cdr parts))]) (check-statement f))]
           [(if)
            (when (not (= 4 (length parts))) (not-le stx "an if takes three parts"))
            (check-expression (cadr parts))
            (check-statement (caddr parts))
            (check-statement (cadddr parts))]
           [else (check-expression stx)])]))

;; a body: statements, and the value of its last form is the value of the body
(define (check-body forms)
  (when (null? forms) (error 'check-expression "a body needs at least one form"))
  (for ([f (in-list (drop-right forms 1))]) (check-statement f))
  (when (statement-form? (last forms))
    (not-le (last forms) "a body ends with the expression that is its value"))
  (check-expression (last forms)))

;; an expression: nothing inside it is a statement
(define (check-expression stx)
  (cond [(not (syntax->list stx)) (void)]
        [else
         (define parts (syntax->list stx))
         (case (head stx)
           [(quote) (void)]
           [(define set!) (not-le stx "a statement where an expression belongs")]
           [(if)
            (when (not (= 4 (length parts))) (not-le stx "an if takes three parts"))
            (for ([p (in-list (cdr parts))]) (check-expression p))]
           [(begin) (for ([p (in-list (cdr parts))]) (check-expression p))]
           [(lambda)
            (when (null? (cddr parts)) (not-le stx "a lambda needs a body"))
            (check-body (cddr parts))]
           [(with-handler)
            (when (< (length parts) 3) (not-le stx "a with-handler takes a handler and a body"))
            (check-expression (cadr parts))
            (check-body (cddr parts))]
           [(trampoline)
            (when (null? (cdr parts)) (not-le stx "a trampoline takes the function to call"))
            (check-body (cdr parts))]
           [else
            (when (memq (head stx) racket-forms) (not-le stx "a Racket form, not LE"))
            (for ([p (in-list parts)]) (check-expression p))])]))
