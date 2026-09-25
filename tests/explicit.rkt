#lang racket/base

;; Tests for the surface language and the pass that makes its bodies explicit:
;; LE is LB with any number of bodies in `with-handler` and `trampoline`, and
;; `make-explicit` wraps the extra ones in a `begin`.

(require rackunit
         racket/string
         "../main.rkt"
         "utilities.rkt")

(module+ test
  (define (explicit source)
    (unparse-LB (make-explicit (parse-LE source))))

  (test-case "a body is made explicit with a begin"
    (check-equal? (explicit '(trampoline 1)) '(trampoline 1))
    (check-equal? (explicit '(trampoline 1 2)) '(trampoline (begin 1 2)))
    (check-equal? (explicit '(trampoline 1 2 3)) '(trampoline (begin 1 2 3)))
    (check-equal? (explicit '(with-handler h 1)) '(with-handler h 1))
    (check-equal? (explicit '(with-handler h 1 2 3)) '(with-handler h (begin 1 2 3))))

  (test-case "the pass rewrites every position a form can be written in"
    (check-equal? (explicit '(begin (define x (trampoline 1 2)) (print x)))
                  '(begin (define x (trampoline (begin 1 2))) (print x)))
    (check-equal? (explicit '(define (f) (trampoline 1 2)))
                  '(define (f) (trampoline (begin 1 2))))
    (check-equal? (explicit '(if #t (trampoline 1 2) (with-handler h 1 2)))
                  '(if #t (trampoline (begin 1 2)) (with-handler h (begin 1 2))))
    ;; a quoted datum is data, not code
    (check-equal? (explicit '(print (quote (trampoline 1 2))))
                  '(print (quote (trampoline 1 2)))))

  (test-case "a definition may have several bodies too"
    (check-equal? (explicit '(define (f x) 1)) '(define (f x) 1))
    (check-equal? (explicit '(define (f x) 1 2)) '(define (f x) (begin 1 2)))
    (check-equal? (explicit '(define (f) (define n 1) (set! n 2) n))
                  '(define (f) (begin (define n 1) (set! n 2) n)))
    (check-equal? (explicit '(define x 1)) '(define x 1))
    (check-python-output
     #<<SRC
(define (f x) (print "a") (print "b") (+ x 1))
(print (f 41))
SRC
     "a\nb\n42\n")
    ;; a value takes one expression, and says so
    (check-exn exn:fail? (lambda () (transpile "(define x 1 2)\n"))))

  (test-case "a form with no body at all is an application"
    (check-equal? (explicit '(trampoline)) '(trampoline))
    (check-equal? (explicit '(with-handler)) '(with-handler)))

  (test-case "LB itself takes one body"
    ;; in LB the shape of (trampoline 1 2) is the shape of a call, and the
    ;; compiler says what it is
    (check-exn exn:fail? (lambda () (check-expressions (parse-LB '(trampoline 1 2)))))
    (check-exn exn:fail? (lambda () (check-expressions (parse-LB '(with-handler h 1 2)))))
    (check-equal? (unparse-LB (parse-LB '(trampoline 1))) '(trampoline 1))
    ;; the surface language takes any number of bodies
    (check-python-output "(print (trampoline 1 2))" "2\n"))

  (test-case "the surface language compiles and runs"
    (check-python-output
     #<<SRC
(define (count n acc)
  (trampoline (print n) (if (= n 0) acc (count (- n 1) (+ acc 1)))))
(print (count 2 0))
SRC
     "2\n1\n0\n2\n")
    (check-python-output
     #<<SRC
(with-handler print (print "a") (raise "boom"))
SRC
     "a\nboom\n")))
