#lang racket/base

;; Tests for the front end: LB source, expanded by Racket's expander with LB's
;; forms bound in a namespace of our own.

(require rackunit
         racket/string
         "../core/expander.rkt")

(module+ test
  (define (expanded source) (syntax->datum (expand-source source)))

  (test-case "the forms that bind nothing are opaque applications"
    (check-equal? (expanded "(if x y z)")
                  '(begin (#%app #%lb-if (#%top . x) (#%top . y) (#%top . z))))
    (check-equal? (expanded "(with-handler h (f x))")
                  '(begin (#%app #%lb-with-handler (#%top . h) (#%app (#%top . f) (#%top . x)))))
    (check-equal? (expanded "(trampoline (f))")
                  '(begin (#%app #%lb-trampoline (#%app (#%top . f)))))
    (check-equal? (expanded "(raise 1)") '(begin (#%app #%lb-raise (quote 1)))))

  (test-case "a definition is a definition the expander knows"
    (check-equal? (expanded "(define x 1)") '(begin (define-values (x) (quote 1))))
    (check-equal? (expanded "(define (f x) (if x 1 2))")
                  '(begin (define-values (f)
                            (lambda (x) (#%app #%lb-if x (quote 1) (quote 2))))))
    (check-equal? (expanded "(set! x (f))")
                  '(begin (set! (#%top . x) (#%app (#%top . f))))))

  (test-case "a free variable is a top reference, a bound one is not"
    (check-equal? (expanded "(define (f x) x)")
                  '(begin (define-values (f) (lambda (x) x)))))

  (test-case "a macro is a Racket transformer over the argument forms"
    (check-equal? (expanded "(defmacro (my-or a b) (list 'if a #t b))\n(my-or p q)")
                  '(begin (begin)
                          (begin (#%app #%lb-if (#%top . p) (quote #t) (#%top . q)))))
    ;; and macros nest, and expand where they are written
    (check-equal? (expanded "(defmacro (my-or a b) (list 'if a #t b))\n(print (my-or (my-or p q) r))")
                  '(begin (begin)
                          (#%app (#%top . print)
                                 (#%app #%lb-if
                                        (#%app #%lb-if (#%top . p) (quote #t) (#%top . q))
                                        (quote #t)
                                        (#%top . r))))))

  (test-case "what a macro introduces is its own binding"
    (define stx (expand-source
                 (string-append
                  "(defmacro (swap a b)\n"
                  "  (list 'begin (list 'define 'tmp a) (list 'set! a b) (list 'set! b 'tmp)))\n"
                  "(define tmp 1)\n"
                  "(swap tmp other)\n")))
    (define tmps (reverse (expanded-ids stx 'tmp)))
    (check-equal? (length tmps) 3)
    ;; the user's tmp is the first; the two the macro introduced are not it
    (check-true (bound-identifier=? (car tmps) (car tmps)))
    (check-false (bound-identifier=? (car tmps) (cadr tmps)))
    (check-false (bound-identifier=? (car tmps) (caddr tmps)))
    (check-true (bound-identifier=? (cadr tmps) (caddr tmps))))

  (test-case "a program body takes statements"
    (check-equal? (expanded "(begin (define x 1) (print x))")
                  '(begin (begin (define-values (x) (quote 1)) (#%app (#%top . print) (#%top . x)))))))
