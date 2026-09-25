#lang racket/base

;; Tests for the LB language definition (core/base.rkt).
;;
;; `read` gives us the datums; `parse-LB` turns one into an LB expression and
;; `unparse-LB` turns it back, so a round trip is the natural way to test the
;; grammar.

(require rackunit
         racket/list
         "../core/base.rkt")

(module+ test
  (define (round-trip datum)
    (unparse-LB (parse-LB datum)))

  ;; forms
  (check-equal? (round-trip '(define x 1)) '(define x 1))
  (check-equal? (round-trip '(define (f x y) (g x y))) '(define (f x y) (g x y)))
  (check-equal? (round-trip '(define (f) 1)) '(define (f) 1))
  (check-equal? (round-trip '(define (f x . rest) (g x rest)))
                '(define (f x . rest) (g x rest)))
  (check-equal? (round-trip '(define (f . rest) rest)) '(define (f . rest) rest))
  (check-equal? (round-trip '(trampoline (f 1) (g 2))) '(trampoline (f 1) (g 2)))
  (check-equal? (round-trip '(set! x 1)) '(set! x 1))
  (check-equal? (round-trip '(raise "boom")) '(raise "boom"))
  (check-equal? (round-trip '(with-handler h (f))) '(with-handler h (f)))
  (check-equal? (round-trip '(begin 1 2 3)) '(begin 1 2 3))
  (check-equal? (round-trip '(if #t 1 2)) '(if #t 1 2))
  ;; `import` is not a form: a module is a value, and import-module is how a
  ;; program gets one, so these are applications like any other
  (check-equal? (round-trip '(import math)) '(import math))
  (check-equal? (round-trip '(import math) ) '(import math))
  (check-equal? (round-trip '(f x y)) '(f x y))
  (check-equal? (round-trip '(f)) '(f))
  (check-equal? (round-trip 'x) 'x)

  ;; literals are self-evaluating; lists and symbols need a quote
  (check-equal? (round-trip 1) 1)
  (check-equal? (round-trip -2.5) -2.5)
  (check-equal? (round-trip "a\n") "a\n")
  (check-equal? (round-trip #t) #t)
  (check-equal? (round-trip #(1 2 3)) #(1 2 3))
  (check-equal? (round-trip #hash((a . 1))) #hash((a . 1)))
  (check-equal? (round-trip ''a) ''a)
  (check-equal? (round-trip ''(1 (2) #(3) #hash((a . "b"))))
                ''(1 (2) #(3) #hash((a . "b"))))

  ;; nesting
  (check-equal? (round-trip '(define (main)
                              (with-handler (lambda (e) (raise e))
                                            (begin (f 1) (if #t (g) (h))))))
                '(define (main)
                   (with-handler (lambda (e) (raise e))
                                 (begin (f 1) (if #t (g) (h))))))

  ;; the terminal predicates
  (check-true (variable? 'x))
  (check-false (variable? "x"))
  (check-true (literal? 1))
  (check-true (literal? #(1 2)))
  (check-true (literal? #hash((a . 1))))
  (check-false (literal? '(1 2)))
  (check-false (literal? 'x))
  (check-true (datum? '(1 #(2) #hash((a . 1)) x "s" #t 1.5)))
  (check-false (datum? '(1 . 2)))
  (check-false (datum? (list 1 (cons 2 3))))

  ;; The grammar is structural: a form whose shape does not match is simply an
  ;; application (of a variable with that name), so these parse -- the code
  ;; generator is the place that rejects a call to a form keyword.
  (check-equal? (round-trip '(if 1 2)) '(if 1 2))
  (check-equal? (round-trip '(if 1 2 3 4)) '(if 1 2 3 4))
  (check-exn exn:fail? (lambda () (parse-LB '(define 1 2))))
  (check-exn exn:fail? (lambda () (parse-LB '(define (f 1) 2))))
  (check-exn exn:fail? (lambda () (parse-LB '(define (f . 1) 2))))
  (check-exn exn:fail? (lambda () (parse-LB '(define (1 . rest) 2))))
  (check-exn exn:fail? (lambda () (parse-LB '(set! 1 2))))
  ;; statements and expressions are different nonterminals
  (check-equal? (round-trip '(begin (define x 1) (print x)))
                '(begin (define x 1) (print x)))
  (check-equal? (round-trip '(if #t (define x 1) 2)) '(if #t (define x 1) 2))
  (check-equal? (round-trip '(trampoline 1)) '(trampoline 1))
  (check-equal? (round-trip '(with-handler h 1)) '(with-handler h 1))
  (check-equal? (round-trip '(raise 1)) '(raise 1))
  (check-equal? (round-trip '(begin)) '(begin))
  (check-equal? (round-trip '(trampoline)) '(trampoline))

  ;; things that are not expressions at all
  (check-exn exn:fail? (lambda () (parse-LB '())))
  (check-exn exn:fail? (lambda () (parse-LB '(1 . 2))))
  (check-exn exn:fail? (lambda () (parse-LB ''(1 . 2)))))
