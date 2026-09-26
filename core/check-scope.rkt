#lang racket/base

;; LE -> LE, checking the lexical scope and leaving the program as it is.
;;
;; A name is in scope when a parameter binds it, when a define in the body it
;; stands in binds it, or when the program defines it at the top level.
;; Anything else is a Python global, which is the point of the language, but a
;; name the program never binds is worth saying out loud: the compiler logs it
;; at warning level, for a reference and for a set! alike.
;;
;; render owns what a Python program can call without the source defining it
;; (the pieces it carries, its operators, its builtins), so this pass asks it.

(require "render.rkt")

(provide check-scope-program)

;; Python names a program may lean on without defining them
(define builtins
  '("abs" "all" "any" "bin" "bool" "bytes" "callable" "chr" "dict" "dir"
    "divmod" "enumerate" "filter" "float" "format" "frozenset" "getattr"
    "hasattr" "hash" "hex" "id" "input" "int" "isinstance" "issubclass" "iter"
    "len" "list" "locals" "map" "max" "min" "next" "object" "oct" "open" "ord"
    "pow" "print" "range" "repr" "reversed" "round" "set" "setattr" "slice"
    "sorted" "str" "sum" "super" "tuple" "type" "vars" "zip"
    "ArithmeticError" "AssertionError" "AttributeError" "Exception"
    "IndexError" "KeyError" "NameError" "NotImplementedError" "OSError"
    "RuntimeError" "StopIteration" "TypeError" "ValueError" "ZeroDivisionError"))

(define (head stx) (and (pair? (syntax->list stx)) (syntax-e (car (syntax->list stx)))))
(define (define? stx) (eq? (head stx) 'define))

(define (target-name target)
  (if (identifier? target) (syntax-e target) (syntax-e (car (syntax-e target)))))

(define (params-of sig)
  (define (walk rest acc)
    (cond [(null? rest) (reverse acc)]
          [(pair? rest) (walk (cdr rest) (cons (syntax-e (car rest)) acc))]
          [else (reverse (cons (syntax-e rest) acc))]))
  (walk sig '()))

(define (define-params target) (params-of (cdr (syntax-e target))))
(define (lambda-params target) (params-of (syntax-e target)))
(define (body-defines forms)
  (for/list ([f (in-list forms)] #:when (define? f))
    (target-name (cadr (syntax->list f)))))

;; a Python name that needs no definition: a piece, an operator, a builtin
(define (known-python? sym)
  (or (hash-ref runtime-pieces sym #f)
      (hash-ref infix sym #f)
      (eq? sym 'not)
      (and (member (munged sym) builtins) #t)))

(define module-names '())
(define warned '())

(define (warn-once! message)
  (unless (member message warned)
    (set! warned (cons message warned))
    (log-warning "rkt-pythonize: ~a" message)))

(define (in-scope? env sym)
  (or (and (member sym env) #t) (and (member sym module-names) #t)))

(define (check-scope-program forms)
  (set! module-names
        (for/list ([f (in-list forms)] #:when (define? f))
          (target-name (cadr (syntax->list f)))))
  (set! warned '())
  (for ([f (in-list forms)]) (check f '()))
  forms)

(define (check stx env)
  (cond [(identifier? stx)
         (define sym (syntax-e stx))
         (unless (or (in-scope? env sym) (known-python? sym))
           (warn-once!
            (format "no definition of ~a in this program: it becomes the Python name ~a"
                    sym (munged sym))))]
        [(not (syntax->list stx)) (void)]
        [else
         (define parts (syntax->list stx))
         (case (head stx)
           [(quote) (void)]
           [(lambda) (check-body (cddr parts) (append (lambda-params (cadr parts)) env))]
           [(define)
            (define target (cadr parts))
            (if (identifier? target)
                (check (caddr parts) env)
                (check-body (cddr parts) (append (define-params target) env)))]
           [(set!)
            (check-set! (cadr parts) env)
            (check (caddr parts) env)]
           [else (for ([p (in-list parts)]) (check p env))])]))

;; a body binds everything it defines, so its own names are in scope
(define (check-body forms env)
  (define env* (append (body-defines forms) env))
  (for ([f (in-list forms)]) (check f env*)))

(define (check-set! target env)
  (when (identifier? target)
    (define sym (syntax-e target))
    (unless (or (in-scope? env sym) (known-python? sym))
      (warn-once!
       (format "this program never defines ~a: this set! becomes a plain Python assignment"
               sym)))))
