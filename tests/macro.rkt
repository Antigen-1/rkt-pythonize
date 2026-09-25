#lang racket/base

;; Tests for the macro level: the Python-side runtime functions macros are built
;; on, `defmacro` itself, and the LM -> LB pass that lowers it.

(require rackunit
         racket/string
         "../main.rkt"
         "utilities.rkt")

(module+ test
  (test-case "the runtime functions"
    (check-python-output "(print (list 1 2 3))" "[1, 2, 3]\n")
    (check-python-output "(print (list))" "[]\n")
    (check-python-output "(print (+ (quote (1 2)) (list 3)))" "[1, 2, 3]\n")
    (check-python-output "(print (apply list (quote (1 2))))" "[1, 2]\n")
    (check-python-output "(print (apply print (quote (7 8))))" "7 8\nNone\n")
    (check-python-output
     "(print (keyword-apply print #hash((sep . \"-\")) (list 1 2)))"
     "1-2\nNone\n")
    (check-python-output "(print (gensym))" "g1\n")
    (check-python-output "(begin (print (gensym \"x\")) (print (gensym \"x\")))" "x1\nx2\n")
    ;; gensym gives an interned symbol, so it behaves like any other symbol
    (check-python-output "(print (eq? (gensym \"same\") (gensym \"same\")))" "False\n"))

  (test-case "eval compiles a form at run time"
    (check-python-output "(print (eval (quote (+ 20 22))))" "42\n")
    (check-python-output "(print (eval (quote (if (< 1 2) \"yes\" \"no\"))))" "yes\n")
    (check-python-output "(print (eval (quote (quote (a 1)))))" "['a', 1]\n")
    (check-python-output "(print (eval (quote (begin (define n 5) (* n n)))))" "25\n")
    (check-python-output
     "(print (eval (quote (begin (define (double n) (* n 2)) (double 21)))))"
     "42\n")
    (check-python-output
     "(begin (define xs (list 1 2 3)) (eval (quote (begin ((getattr xs \"append\") 4)))) (print xs))"
     "[1, 2, 3, 4]\n")
    ;; a program that defines its own function still reaches it from eval
    (check-python-output
     "(begin (define (add-one n) (+ n 1)) (print (eval (quote (add-one 41)))))"
     "42\n")
    (check-python-failure "(eval (quote ()))" #rx"eval: \\(\\) is not a form"))

  (test-case "a macro is a procedure over unevaluated forms"
    (check-python-output
     #<<SRC
(defmacro (twice x) (list '+ x x))
(print (twice 21))
SRC
     "42\n")
    ;; the argument form arrives as data, so a macro can quote it back
    (check-python-output
     #<<SRC
(defmacro (quoted x) (list 'quote x))
(print (quoted (1 2 3)))
SRC
     "[1, 2, 3]\n")
    ;; and it can look at the form it was given
    (check-python-output
     #<<SRC
(defmacro (first-form x) (list 'quote ((getattr x "__getitem__") 0)))
(print (first-form (alpha beta)))
SRC
     "alpha\n"))

  (test-case "a macro's rest parameter is the rest of the forms"
    ;; the macro procedure is defined with the same dotted signature the source
    ;; writes, so the Python it becomes has *rest
    (check-python-contains
     "(defmacro (unless c . body) c)\n(unless 1 2)"
     "def unless(c, *body):")
    (check-python-contains
     "(defmacro (unless c . body) c)\n(unless 1 2)"
     "body = [*body]")

    (check-python-output
     #<<SRC
(defmacro (unless c . body) (list 'if c #f (+ '(begin) body)))
(unless #f (print 1) (print 2) (print 3))
(print (unless #t 5))
SRC
     "1\n2\n3\nFalse\n")
    (check-python-output
     #<<SRC
(defmacro (join . items)
  (list 'keyword-apply 'print #hash((sep . "-")) (list 'quote items)))
(join 1 2 3)
SRC
     "1-2-3\n"))

  (test-case "a macro can define something"
    (check-python-output
     #<<SRC
(defmacro (defn signature . body) (+ (list 'define signature) body))
(defn (double n) (* n 2))
(print (double 21))
SRC
     "42\n"))

  (test-case "a macro can expand into a call of another macro"
    (check-python-output
     #<<SRC
(defmacro (unless c . body) (list 'if c #f (+ '(begin) body)))
(defmacro (my-when c . body) (list 'unless (list 'not c) (+ '(begin) body)))
(my-when #t (print "yes"))
(print (my-when #f 1))
SRC
     "yes\nFalse\n"))

  (test-case "gensym names what an expansion introduces"
    (check-python-output
     #<<SRC
(defmacro (swap! a b)
  (begin
    (define tmp (gensym "tmp"))
    (list 'begin (list 'define tmp a) (list 'set! a b) (list 'set! b tmp))))
(begin
  (define x 1)
  (define y 2)
  (swap! x y)
  (print x)
  (print y))
SRC
     "2\n1\n"))

  (test-case "a macro works in a value position too"
    (check-python-output
     #<<SRC
(defmacro (plus1 x) (list '+ x 1))
(print (* 2 (plus1 20)))
SRC
     "42\n"))

  (test-case "the pass lowers macros into eval of the call form"
    (check-equal? (unparse-LB (expand-macros (parse-LM '(print (+ 1 2)))))
                  '(print (+ 1 2)))
    (check-equal?
     (unparse-LB (expand-macros (parse-LM '(defmacro (twice x) (list '+ x x)))))
     '(begin (define _macros (quote (twice)))
             (define (twice x) (list '+ x x))))
    (check-equal?
     (unparse-LB (expand-macros (parse-LM '(begin (defmacro (twice x) x) (print (twice 21))))))
     '(begin (define _macros (quote (twice)))
             (begin (define (twice x) x) (print (eval (quote (twice 21)))))))
    ;; a dotted signature stays dotted: it is the signature of the procedure
    (check-equal?
     (unparse-LB (expand-macros (parse-LM '(defmacro (m a b . rest) b))))
     '(begin (define _macros (quote (m)))
             (define (m a b . rest) b)))
    ;; a quoted form is data, so a macro call inside it stays untouched
    (check-equal?
     (unparse-LB (expand-macros (parse-LM '(begin (defmacro (m x) x) (print (quote (m 1)))))))
     '(begin (define _macros (quote (m)))
             (begin (define (m x) x) (print (quote (m 1)))))))

  (test-case "the generated program carries the macro table"
    (check-python-contains
     "(defmacro (twice x) (list '+ x x))\n(print (twice 21))"
     "_macros = [Symbol(\"twice\")]")
    (check-python-contains
     "(defmacro (twice x) (list '+ x x))\n(print (twice 21))"
     "print(eval([Symbol(\"twice\"), 21]))")
    (check-python-contains
     "(defmacro (unless c . body) c)\n(unless 1 2)"
     "_macros = [Symbol(\"unless\")]")
    ;; a program without macros is left exactly as it was
    (check-python-source "(print (+ 1 2))\n"
                         "# generated by rkt-pythonize\nprint((1 + 2))\n")
    (check-false (string-contains? (transpile "(print (+ 1 2))\n") "_macros")
                 "no macro table without macros"))

  (test-case "LM is LB plus defmacro"
    ;; outside LM, a defmacro is an ordinary application of the variable
    ;; `defmacro`, like any other form LB does not know
    (check-equal? (unparse-LB (parse-LB '(defmacro (m x) x))) '(defmacro (m x) x))
    ;; in LM it is a form of its own, and its signature is checked
    (check-equal? (unparse-LM (parse-LM '(defmacro (m x) x))) '(defmacro (m x) x))
    (check-exn exn:fail? (lambda () (parse-LM '(defmacro (1 x) x))))
    (check-true (macro-signature? '(m)))
    (check-true (macro-signature? '(m x y)))
    (check-true (macro-signature? '(m x . rest)))
    (check-false (macro-signature? '(1 x)))
    (check-false (macro-signature? 'm))
    (check-false (macro-signature? '(m 1))))

  (test-case "macro programs fail loudly"
    ;; too few argument forms for the macro
    (check-python-failure "(defmacro (twice x) x)\n(twice)\n" #rx"TypeError")
    ;; defmacro belongs at the top level
    (check-exn exn:fail? (lambda () (transpile "(define (f) (defmacro (m) 1))\n")))
    ;; and the signature has to be a macro signature
    (check-exn exn:fail? (lambda () (transpile "(defmacro (1 x) x)\n")))))
