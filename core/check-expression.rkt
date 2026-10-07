#lang racket/base

;; LE -> LE, checking that statements and expressions stay in their own places,
;; and leaving the program as it is.
;;
;; A statement is what a body, a begin in statement position and the branch of a
;; statement if are made of; an expression is what an if, a call argument and a
;; begin in expression position are made of.  Nothing in an expression position
;; is a statement, so a definition cannot hide inside one and mean something
;; else there, and a body ends with the expression that is its value.
;;
;; The parameter list of a define or a lambda and the arguments of a call are
;; checked here too, because both are places a keyword argument may stand and
;; nowhere else is: core/params.rkt says what one is.

(require racket/list
         "names.rkt"
         "params.rkt")

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
  (check-module-names forms)
  (for ([f (in-list forms)]) (check-statement f))
  forms)

;; Two top-level names that are one Python name are one name in the module --
;; the second definition would be the first -- so a program that exports its
;; names is told rather than left to wonder which one it got.  core/names.rkt
;; says what a name is in Python, and nothing here reads a name any other way.
(define (check-module-names forms)
  (define seen (make-hash))
  (for ([f (in-list forms)] #:when (eq? (head f) 'define))
    (define target (cadr (syntax->list f)))
    (define name (and (or (identifier? target) (pair? (syntax-e target)))
                      (target-name target)))
    (when name
      (define python (python-name name))
      (define first (hash-ref seen python #f))
      (when (and first (not (eq? first name)))
        (not-le f (format "~a and ~a are both ~a in Python: rename one of them"
                          first name python)))
      (hash-set! seen python name))))

;; the name a top-level define binds
(define (target-name target)
  (if (identifier? target) (syntax-e target) (syntax-e (car (syntax-e target)))))

;; a statement: an expression, a definition, an assignment, a begin or an if
(define (check-statement stx)
  (cond [(keyword? (syntax-e stx))
         (not-le stx "a keyword argument belongs in a call: (f #:k value)")]
        [(not (syntax->list stx)) (void)]
        [else
         (define parts (syntax->list stx))
         (case (head stx)
           [(define)
            (define target (cadr parts))
            (when (null? (cddr parts)) (not-le stx "a definition needs a body"))
            (check-not-constant stx target)
            (cond [(identifier? target)
                   (when (not (null? (cdr (cddr parts))))
                     (not-le stx "a value definition takes one expression"))
                   (check-expression (caddr parts))]
                  [(pair? (syntax-e target))
                   (check-signature stx (cdr (syntax->datum target)))
                   (check-body (cddr parts))]
                  [else (not-le stx "a definition of what?")])]
           [(set!)
            (when (not (= 3 (length parts))) (not-le stx "an assignment takes one expression"))
            ;; a keyword is syntax, not a name to assign to
            (unless (identifier? (cadr parts))
              (not-le stx "an assignment takes a name to assign"))
            (check-not-constant stx (cadr parts))
            (check-expression (caddr parts))]
           [(begin) (for ([f (in-list (cdr parts))]) (check-statement f))]
           [(if)
            (when (not (= 4 (length parts))) (not-le stx "an if takes three parts"))
            (check-expression (cadr parts))
            (check-statement (caddr parts))
            (check-statement (cadddr parts))]
           [else (check-expression stx)])]))

;; Python's constants are values a program reads: None is what the threading
;; operators interrupt on, and True and False are what #t and #f are
(define (check-constant stx name)
  (when (python-constant? name)
    (not-le stx (format "~a is one of Python's constants: it is a value, not a name to bind"
                        name))))

(define (check-not-constant stx target)
  (when (identifier? target) (check-constant stx (syntax-e target))))

;; a body: statements, and the value of its last form is the value of the body
(define (check-body forms)
  (when (null? forms) (error 'check-expression "a body needs at least one form"))
  (for ([f (in-list (drop-right forms 1))]) (check-statement f))
  (when (statement-form? (last forms))
    (not-le (last forms) "a body ends with the expression that is its value"))
  (check-expression (last forms)))

;; an expression: nothing inside it is a statement
(define (check-expression stx)
  (cond [(keyword? (syntax-e stx))
         (not-le stx "a keyword argument belongs in a call: (f #:k value)")]
        [(identifier? stx)
         (when (memq (syntax-e stx) operators)
           (not-le stx "an operator is syntax, not a value: define a procedure instead"))]
        ;; a literal is data, and data has no symbols and no keywords -- and a
        ;; keyword is syntax, so it is never one
        [(not (syntax->list stx)) (check-datum stx (syntax->datum stx))]
        [else
         (define parts (syntax->list stx))
         (case (head stx)
           [(quote)
            (when (= 2 (length parts)) (check-datum stx (syntax->datum (cadr parts))))]
           [(define set!) (not-le stx "a statement where an expression belongs")]
           [(import)
            (when (not (= 2 (length parts))) (not-le stx "an import takes one module"))
            (check-expression (cadr parts))]
           [(if)
            (when (not (= 4 (length parts))) (not-le stx "an if takes three parts"))
            (for ([p (in-list (cdr parts))]) (check-expression p))]
           [(begin)
            ;; a begin sequences expressions.  That a definition cannot stand in
            ;; one is begin's semantics, not a limit of the compiler: a begin is
            ;; not a place where a name is defined, and if a definition stood in
            ;; one it would mean something other than it says where it stands.
            (when (null? (cdr parts)) (not-le stx "an empty begin has no value"))
            (for ([p (in-list (cdr parts))])
              (when (statement-form? p)
                (not-le p
                        (string-append
                         "a begin sequences expressions: a definition belongs in a body, "
                         "where a name is defined")))
              (check-expression p))]
           [(lambda)
            (when (null? (cddr parts)) (not-le stx "a lambda needs a body"))
            (define sig (syntax->datum (cadr parts)))
            (when (not (or (null? sig) (pair? sig))) (not-le stx "a lambda takes a parameter list"))
            (check-signature stx sig)
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
            ;; the head of a call is a name or a form, not a place for an operand
            (when (keyword? (syntax-e (car parts)))
              (not-le stx "a call starts with a name, not a keyword"))
            (unless (identifier? (car parts)) (check-expression (car parts)))
            (check-call-arguments stx (cdr parts) (head stx))])]))

;; an operator is syntax, so it has no keyword arguments; any other call takes
;; them, and what a keyword argument is -- and where it may stand -- is
;; core/params.rkt's
(define (check-call-arguments stx args name)
  (when (and name (memq name operators)
             (for/or ([a (in-list args)]) (keyword? (syntax-e a))))
    (not-le stx "an operator takes no keyword arguments: an operator is syntax, not a procedure"))
  (define-values (positional keywords)
    (argument-parts args (lambda (message) (not-le stx message))))
  (for ([p (in-list positional)]) (check-expression p))
  (for ([keyword (in-list keywords)]) (check-expression (cadr keyword))))

;; quoted data and a literal are data: a symbol is not (there is no symbol
;; type), and a keyword is the syntax of an application or a parameter list
(define (check-datum stx datum)
  (cond [(symbol? datum) (not-le stx (format "a symbol is not data: ~a" datum))]
        [(keyword? datum) (not-le stx (format "a keyword is not data: ~a" datum))]
        [(list? datum) (for ([d (in-list datum)]) (check-datum stx d))]
        [(vector? datum) (for ([d (in-list (vector->list datum))]) (check-datum stx d))]
        [(hash? datum)
         (for ([key (in-list (hash-keys datum))])
           (check-datum stx key)
           (check-datum stx (hash-ref datum key)))]
        [else (void)]))

;; a parameter list: core/params.rkt says what one is, and a form to point at
;; makes its errors read as that form
(define (check-signature stx sig)
  (define-values (params keywords rest)
    (signature-parts sig (lambda (message) (not-le stx message))))
  (for ([p (in-list params)]) (check-constant stx p))
  (for ([k (in-list keywords)]) (check-constant stx (keyword-param-name k)))
  (when rest (check-constant stx rest))
  (values params keywords rest))
