#lang racket/base

;; LE -> LB: make the sequencing of a body explicit.
;;
;; LE is LB with the sugar a source is written in.  The core language gives
;; `with-handler` and `trampoline` one body each, because both of them are
;; expressions; LE lets a source write as many bodies as it likes, and this pass
;; wraps them in the `begin` the core language has room for:
;;
;;   (trampoline e1 e2)      -> (trampoline (begin e1 e2))
;;   (with-handler h e1 e2)  -> (with-handler h (begin e1 e2))
;;   (define (f x) e1 e2)    -> (define (f x) (begin e1 e2))
;;
;; One body is left alone, and a `(trampoline)` with no body at all is not the
;; form: it is an application of a variable of that name.

(require nanopass/base
         racket/list
         "../core/base.rkt")

(provide LE
         parse-LE
         unparse-LE
         make-explicit)

(define-language LE
  (extends LB)
  (Stmt (s body)
        (- (define b body))
        (+ (define b body s* ...)))
  (Expr (e)
        (- (with-handler e1 e2)
           (trampoline e1))
        (+ (with-handler e1 e2 e* ...)
           (trampoline e1 e* ...))))

(define-parser parse-LE LE)

;; An LE program, as an LB program.
(define (make-explicit program)
  (parse-LB (explicit (unparse-LE program))))

(define (explicit e)
  (cond
    ;; a quoted datum is data, not code: nothing inside one is rewritten
    [(and (pair? e) (eq? (car e) 'quote)) e]
    [(and (pair? e) (eq? (car e) 'define)) (explicit-define e)]
    [(and (pair? e) (eq? (car e) 'trampoline)) (explicit-trampoline e)]
    [(and (pair? e) (eq? (car e) 'with-handler)) (explicit-with-handler e)]
    [else (explicit-all e)]))

(define (explicit-all e)
  (cond [(pair? e) (cons (explicit (car e)) (explicit-all (cdr e)))]
        [else e]))

;; A definition binds one name and may hold several bodies; a value definition
;; with several of them is left for the compiler to refuse, since only a
;; procedure has a body to sequence.
(define (explicit-define e)
  (define bodies (map explicit (cddr e)))
  (cond [(null? (cdr bodies)) (list 'define (cadr e) (car bodies))]
        [else (list 'define (cadr e) (cons 'begin bodies))]))

(define (explicit-trampoline e)
  (define bodies (map explicit (cdr e)))
  (cond [(null? bodies) (explicit-all e)]
        [(null? (cdr bodies)) (list 'trampoline (car bodies))]
        [else (list 'trampoline (cons 'begin bodies))]))

(define (explicit-with-handler e)
  (define parts (map explicit (cdr e)))
  (cond [(or (null? parts) (null? (cdr parts))) (explicit-all e)]
        [(null? (cddr parts)) (cons 'with-handler parts)]
        [else (list 'with-handler (car parts) (cons 'begin (cdr parts)))]))
