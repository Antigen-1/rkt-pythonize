#lang racket/base

;; End-to-end tests: LB source text goes through `transpile` and the resulting
;; Python program is run by a real interpreter, so these check behaviour (and,
;; where readability is the point, the exact generated text).
;;
;; The interpreter is `$TEST_PYTHON_EXE` when that is set, and otherwise
;; `python3` from PATH.

(require rackunit
         racket/string
         "utilities.rkt")

(module+ test
  (test-case "literals and printing"
    (check-python-output "(print 42)" "42\n")
    (check-python-output "(print 4.5)" "4.5\n")
    (check-python-output "(print \"hi\")" "hi\n")
    (check-python-output "(print #t)" "True\n")
    (check-python-output "(print #f)" "False\n")
    (check-python-output "(print #(1 2 3))" "(1, 2, 3)\n")
    (check-python-output "(print #hash((a . 1)))" "{'a': 1}\n"))

  (test-case "arithmetic and the operator table"
    (check-python-output "(print (+ 1 (* 2 3)))" "7\n")
    (check-python-output "(print (- 10 3 2))" "5\n")
    (check-python-output "(print (/ 7 2))" "3.5\n")
    (check-python-output "(print (quotient 7 2))" "3\n")
    (check-python-output "(print (modulo 7 2))" "1\n")
    (check-python-output "(print (expt 2 10))" "1024\n")
    (check-python-output "(print (< 1 2))" "True\n")
    (check-python-output "(print (= 1 2))" "False\n")
    (check-python-output "(print (equal? #(1 2) #(1 2)))" "True\n")
    (check-python-output "(print (and 1 2 3))" "3\n")
    (check-python-output "(print (or #f 3))" "3\n")
    (check-python-output "(print (not #f))" "True\n")
    (check-python-output "(print (+ \"a\" \"b\"))" "ab\n"))

  (test-case "a free variable is a Python global"
    (check-python-output "(print (len #(1 2 3)))" "3\n")
    (check-python-output "(print (str 42))" "42\n")
    (check-python-output "(print (abs -5))" "5\n"))

  (test-case "definitions and assignment"
    (check-python-output "(define x 1)\n(set! x (+ x 41))\n(print x)" "42\n")
    (check-python-output "(define (double x) (* x 2))\n(print (double 21))" "42\n")
    (check-python-output "(print (begin 1 2 3))" "3\n"))

  (test-case "conditionals"
    (check-python-output "(print (if (< 1 2) \"yes\" \"no\"))" "yes\n")
    (check-python-output "(define x 5)\n(if (= x 5) (print \"five\") (print \"other\"))" "five\n"))

  (test-case "recursion"
    (check-python-output
     #<<SRC
(define (fact n)
  (if (= n 0) 1 (* n (fact (- n 1)))))
(print (fact 5))
SRC
     "120\n"))

  (test-case "a dotted parameter list collects the remaining arguments"
    (check-python-output
     #<<SRC
(define (f a . rest) (+ a ((object-get-attr rest "__len__"))))
(print (f 1 2 3))
SRC
     "3\n")
    (check-python-output
     #<<SRC
(define (all . args) args)
(print (all))
(print (all 1 2 3))
SRC
     "[]\n[1, 2, 3]\n")
    ;; the rest parameter is a list, so the list operations work on it
    (check-python-output
     #<<SRC
(define (total-of . numbers) (sum numbers))
(print (total-of 1 2 3 4))
SRC
     "10\n")
    (check-python-output
     #<<SRC
(define (add-marker . xs) (begin ((object-get-attr xs "append") 99) xs))
(print (add-marker 1 2))
SRC
     "[1, 2, 99]\n")
    ;; and it can be passed on with apply
    (check-python-output
     #<<SRC
(define (f a . rest) (apply list a rest))
(print (f 1 2 3))
SRC
     "[1, 2, 3]\n"))

  (test-case "closures and set! of an enclosing local"
    (check-python-output
     #<<SRC
(define (make-counter)
  (begin
    (define n 0)
    (define (tick) (begin (set! n (+ n 1)) n))
    tick))
(define c (make-counter))
(print (c))
(print (c))
(print ((make-counter)))
SRC
     "1\n2\n1\n")
    ;; the bounces of a trampolined closure assign an enclosing local
    (check-python-output
     #<<SRC
(define (make-counter)
  (begin
    (define n 0)
    (define (tick) (trampoline (begin (set! n (+ n 1)) n)))
    tick))
(define c (make-counter))
(print (c))
(print (c))
SRC
     "1\n2\n"))

  (test-case "trampoline keeps tail calls flat"
    (check-python-output
     #<<SRC
(define (count n acc)
  (trampoline (if (= n 0) acc (count (- n 1) (+ acc 1)))))
(print (count 100000 0))
SRC
     "100000\n")
    (check-python-output
     #<<SRC
(define (even? n) (trampoline (if (= n 0) #t (odd? (- n 1)))))
(define (odd? n) (trampoline (if (= n 0) #f (even? (- n 1)))))
(print (even? 100000))
SRC
     "True\n"))

  (test-case "a trampoline calls the procedure its body returns"
    ;; the body's value is a procedure, so it is called
    (check-python-output
     #<<SRC
(define (answer) 42)
(print (trampoline answer))
SRC
     "42\n")
    ;; and it keeps calling while the values are procedures
    (check-python-output
     #<<SRC
(define (last) 42)
(define (middle) last)
(define (first-thing) middle)
(print (trampoline first-thing))
SRC
     "42\n")
    ;; a value that is not a procedure is the value
    (check-python-output "(print (trampoline 7))" "7\n")
    (check-python-output "(print (trampoline))" "None\n")
    (check-python-output "(print (trampoline (list 1 2)))" "[1, 2]\n")
    ;; a tail call of the body is what the trampoline calls
    (check-python-source
     "(define (count n acc) (trampoline (count 1 2)))\n"
     #<<PY
# generated by rkt-pythonize

def _trampoline(value):
    """(trampoline e ...): call the procedure the body returns, and keep
    calling while the value is one."""
    while callable(value):
        value = value()
    return value

def count(n, acc):
    return _trampoline(count_body(n, acc))

def count_body(n, acc):
    return lambda: count_body(1, 2)


PY
     )
    ;; the driver is the program's, not the language's, so its name stays
    ;; reserved: a source that writes _trampoline gets a name of its own
    (check-python-output
     #<<SRC
(define (_trampoline x) (+ x 1))
(print (_trampoline 41))
SRC
     "42\n"))

  (test-case "a trampolined procedure drives itself wherever it is called"
    ;; a non-tail call returns a value, not a thunk
    (check-python-output
     #<<SRC
(define (count n acc) (trampoline (if (= n 0) acc (count (- n 1) (+ acc 1)))))
(define (add-one n) (+ 1 (count n 0)))
(print (add-one 100000))
SRC
     "100001\n")
    ;; a call through a variable is driven by the callee
    (check-python-output
     #<<SRC
(define (count n acc) (trampoline (if (= n 0) acc (count (- n 1) (+ acc 1)))))
(define (run f) (f 100000 0))
(print (run count))
SRC
     "100000\n")
    ;; a trampolined call inside the arguments of another bounce
    (check-python-output
     #<<SRC
(define (count n acc) (trampoline (if (= n 0) acc (count (- n 1) (+ acc 1)))))
(define (outer n) (trampoline (if (= n 0) 0 (outer (- n (count 5 0))))))
(print (outer 100000))
SRC
     "0\n"))

  (test-case "raise and with-handler"
    (check-python-output "(with-handler print (raise \"boom\"))" "boom\n")
    (check-python-output
     #<<SRC
(define (boom) (raise 41))
(define (recover e) (+ e 1))
(print (with-handler recover (boom)))
SRC
     "42\n")
    (check-python-output
     #<<SRC
(define (boom) (raise 1))
(define (recover e) (quote recovered))
(define (safe thunk) (with-handler recover (thunk)))
(print (safe boom))
SRC
     "recovered\n")
    (check-python-output
     #<<SRC
(define (recover e) (print "caught"))
(with-handler recover (int "x"))
SRC
     "caught\n")
    (check-python-output
     #<<SRC
(define (bump e) (raise (+ e 1)))
(define (inner e) (bump e))
(with-handler print (with-handler inner (raise 1)))
SRC
     "2\n"))

  (test-case "an uncaught raise fails the program"
    (check-python-failure "(raise \"boom\")" #rx"_Raised: boom"))

  (test-case "symbols are interned strings"
    (check-python-output "(print 'a)" "a\n")
    (check-python-output "(print (eq? 'a 'a))" "True\n")
    (check-python-output "(print (= (id 'a) (id 'a)))" "True\n")
    (check-python-output "(print ((object-get-attr 'a \"upper\")))" "A\n")
    (check-python-output "(print ((object-get-attr #hash((a . 1)) \"get\") \"a\"))" "1\n"))

  (test-case "quoted data"
    (check-python-output "(print '(1 2 3))" "[1, 2, 3]\n")
    (check-python-output "(print '(a \"b\"))" "['a', 'b']\n")
    (check-python-output "(print '(1 (2 (3))))" "[1, [2, [3]]]\n")
    (check-python-output "(print '#(1 (2)))" "(1, [2])\n"))

  (test-case "a quoted list is a Python list, not a linked list"
    ;; it is a Python list, so the Python operations on lists are the operations
    ;; on forms: there is no cons, car or cdr anywhere in the runtime
    (check-python-output
     #<<SRC
(print ((object-get-attr '(1 2 3) "__len__")))
(print (object-ref '(1 2 3) 0))
(print (+ '(1 2) '(3)))
(begin (define xs '(1 2 3)) ((object-get-attr xs "append") 4) (print xs))
SRC
     "3\n1\n[1, 2, 3]\n[1, 2, 3, 4]\n")
    (check-python-failure "(print (car '(1 2)))" #rx"NameError: name 'car' is not defined"))

  (test-case "if counts only False as false"
    (check-python-output "(print (if #f \"no\" \"false-is-false\"))" "false-is-false\n")
    (check-python-output "(print (if #t \"true-is-true\" \"no\"))" "true-is-true\n")
    ;; 0, 0.0, "", '() and None are all true
    (check-python-output
     #<<SRC
(print (if 0 "zero" "no"))
(print (if 0.0 "zero-float" "no"))
(print (if "" "empty-string" "no"))
(print (if '() "empty-list" "no"))
(print (if (list) "empty-list" "no"))
(print (if (if #t #f #f) "no" "the-inner-if-was-false"))
SRC
     "zero\nzero-float\nempty-string\nempty-list\nempty-list\nthe-inner-if-was-false\n")
    (check-python-output
     "(print (if (print \"side\") \"none-is-true\" \"no\"))"
     "side\nnone-is-true\n")
    (check-python-output
     "(begin (define n 0) (print (if n \"n-is-true\" \"no\")))"
     "n-is-true\n"))

  (test-case "objects, attributes and modules"
    (check-python-output
     #<<SRC
(print (object-ref '(10 20 30) 1))
(begin (define xs (list 1 2 3)) (object-set! xs 1 99) (print xs))
(begin (define table #hash((a . 1))) (object-set! table "b" 2) (print (object-ref table "a")) (print table))
SRC
     "20\n[1, 99, 3]\n1\n{'a': 1, 'b': 2}\n")
    (check-python-output
     #<<SRC
(print ((object-get-attr "abc" "upper")))
(print (object-has-attr? "abc" "upper"))
(print (object-has-attr? "abc" "nope"))
SRC
     "ABC\nTrue\nFalse\n")
    (check-python-output
     #<<SRC
(import types)
(define point ((object-get-attr types "SimpleNamespace")))
(object-set-attr! point "x" 1)
(object-set-attr! point "y" 2)
(print (+ (object-get-attr point "x") (object-get-attr point "y")))
SRC
     "3\n"))

  (test-case "import is an import statement"
    (check-python-output
     #<<SRC
(import math)
(print ((object-get-attr math "sqrt") 16))
SRC
     "4.0\n")
    ;; it is a top-level import wherever the source writes it, and once only
    (check-equal?
     (for/list ([line (in-list (string-split
                                (transpile "(define (f) (begin (import math) 1))\n(import math)\n")
                                "\n"))]
                #:when (string=? line "import math"))
       line)
     '("import math"))
    (check-python-output "(import)\n(print 1)" "1\n")
    (check-python-output
     #<<SRC
(import (ref math sqrt))
(print (sqrt 16))
SRC
     "4.0\n")
    (check-python-output
     #<<SRC
(import (as os.path path))
(print ((object-get-attr path "basename") "/a/b"))
SRC
     "b\n")
    ;; the three kinds of spec, each written once
    (check-equal?
     (for/list ([line (in-list (string-split
                                (transpile "(import math (ref math sqrt pi) (as os.path path))\n")
                                "\n"))]
                #:when (or (string=? line "import math")
                           (string=? line "from math import sqrt, pi")
                           (string=? line "import os.path as path")))
       line)
     '("import math" "from math import sqrt, pi" "import os.path as path"))
    ;; a macro can import what the code it writes needs
    (check-python-output
     #<<SRC
(defmacro (use mod name) (list 'import (list 'ref mod name)))
(use math sqrt)
(print (sqrt 9))
SRC
     "3.0\n")
    (check-python-output
     "(print (eval (quote (begin (import (ref math sqrt)) (sqrt 25)))))"
     "5.0\n")
    ;; a form the program builds at run time can import too
    (check-python-output
     "(print (eval (quote (begin (import math) ((object-get-attr math \"ceil\") 1.2)))))"
     "2\n"))

  (test-case "Scheme names become readable Python names"
    (check-python-output "(define (zero? n) (= n 0))\n(print (zero? 0))" "True\n")
    (check-python-output "(define (add-one! n) (+ n 1))\n(print (add-one! 41))" "42\n")
    (check-python-output "(define (pass x) x)\n(print (pass 1))" "1\n"))

  (test-case "the reader is Racket's read"
    (check-python-output "; a comment\n(print 1) ; and another\n" "1\n")
    (check-python-output "(print\n  1)\n" "1\n"))

  (test-case "nothing to do"
    (check-python-source "" "# generated by rkt-pythonize\n")
    (check-python-source "(begin)\n" "# generated by rkt-pythonize\n")
    (check-python-source "(trampoline)\n" "# generated by rkt-pythonize\n")
    (check-python-output "" ""))

  (test-case "malformed programs are rejected"
    (check-exn exn:fail? (lambda () (transpile "()")))
    (check-exn exn:fail? (lambda () (transpile "(define 1 2)"))))

  (test-case "generated Python stays readable"
    (check-python-source
     "(define x 1)\n(print (+ x 2))\n"
     #<<PY
# generated by rkt-pythonize
x = 1
print((x + 2))

PY
     )
    (check-python-source
     "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))\n(print (fact 5))\n"
     #<<PY
# generated by rkt-pythonize
def fact(n):
    if ((n == 0) is not False):
        return 1
    else:
        return (n * fact((n - 1)))

print(fact(5))

PY
     ))

  (test-case "the prelude is emitted only where it is used"
    (check-true (string-contains? (transpile "(print 1)") "print(1)")
                "no prelude for a program that needs none")
    (check-false (string-contains? (transpile "(print 1)") "_trampoline")
                 "a program without trampoline gets no trampoline prelude")
    (check-false (string-contains? (transpile "(print 1)") "Symbol")
                 "a program without symbols gets no Symbol class")
    (check-python-contains "(print 'a)" "class Symbol(str):")
    (check-python-contains "(trampoline (f))" "def _trampoline(value):"))

  (test-case "a trampoline is never added automatically"
    (check-python-source
     "(define (f n) (if (= n 0) 0 (f (- n 1))))\n"
     #<<PY
# generated by rkt-pythonize
def f(n):
    if ((n == 0) is not False):
        return 0
    else:
        return f((n - 1))


PY
     )
    (check-python-contains
     #<<SRC
(define (count n acc)
  (trampoline (if (= n 0) acc (count (- n 1) (+ acc 1)))))
SRC
     #<<PY
def count(n, acc):
    return _trampoline(count_body(n, acc))

def count_body(n, acc):
    return (acc if ((n == 0) is not False) else lambda: count_body((n - 1), (acc + 1)))

PY
     )))
