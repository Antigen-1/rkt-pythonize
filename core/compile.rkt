#lang racket/base

;; LB -> Python, at expansion time: the expanded body of a module in, Python
;; source out.  Names go to Python by name (a free name is a Python global);
;; nothing here needs a runtime library beyond the few prelude lines a program
;; actually uses.

(require racket/list
         racket/string)

(provide compile-body)

(define infix
  (for/hash ([p (in-list '((+ . "+") (- . "-") (* . "*") (/ . "/")
                           (= . "==") (< . "<") (> . ">") (<= . "<=") (>= . ">=")
                           (equal? . "==")) )])
    (values (car p) (cdr p))))

(define reserved
  '("False" "None" "True" "and" "as" "assert" "async" "await" "break" "class"
    "continue" "def" "del" "elif" "else" "except" "finally" "for" "from"
    "global" "if" "import" "in" "is" "lambda" "nonlocal" "not" "or" "pass"
    "raise" "return" "try" "while" "with" "yield"))

(define (munged sym)
  (define text
    (list->string
     (append*
      (for/list ([ch (in-string (symbol->string sym))])
        (case ch
          [(-) (string->list "_")] [(?) (string->list "_p")] [(!) (string->list "_b")]
          [else (list ch)])))))
  (define base (if (and (positive? (string-length text)) (char-numeric? (string-ref text 0)))
                   (string-append "_" text)
                   text))
  (if (member base reserved) (string-append base "_") base))

(define (py-string text)
  (string-append "\"" (string-join
                       (for/list ([ch (in-string text)])
                         (case ch
                           [(#\\) "\\\\"] [(#\") "\\\""] [(#\newline) "\\n"]
                           [(#\tab) "\\t"] [else (string ch)]))
                       "") "\""))

(define (py-datum value)
  (cond [(symbol? value) (error 'compile "a symbol is not data: ~a" value)]
        [(integer? value) (format "~a" value)]
        [(flonum? value) (format "~a" value)]
        [(string? value) (py-string value)]
        [(boolean? value) (if value "True" "False")]
        [(list? value) (format "[~a]" (string-join (map py-datum value) ", "))]
        [(vector? value) (format "(~a)" (string-join (map py-datum (vector->list value)) ", "))]
        [(hash? value) (format "{~a}"
                               (string-join (for/list ([k (in-list (hash-keys value))])
                                              (format "~a: ~a" (py-datum k) (py-datum (hash-ref value k))))
                                            ", "))]
        [else (error 'compile "not data: ~a" value)]))

(define (head stx) (and (pair? (syntax->list stx)) (syntax-e (car (syntax->list stx)))))

;; an expression
(define (expr stx)
  (cond
    [(identifier? stx) (munged (syntax-e stx))]
    [(not (syntax->list stx)) (py-datum (syntax-e stx))]
    [else
     (define parts (syntax->list stx))
     (case (head stx)
       [(quote) (py-datum (syntax-e (cadr parts)))]
       [(#%lb-global) (munged (syntax-e (cadr (syntax->list (cadr parts)))))]
       [(if) (format "(~a if ~a else ~a)" (expr (caddr parts))
                     (expr (cadr parts)) (expr (cadddr parts)))]
       [(begin) (format "_begin(~a)" (string-join (map expr (cdr parts)) ", "))]
       [(lambda) (error 'compile "a lambda has to be a definition's value")]
       [(#%lb-raise) (need "raise") (format "_raise(~a)" (expr (cadr parts)))]
       [(#%app)
        (define f (cadr parts))
        (define args (cddr parts))
        (define op (and (identifier? f) (hash-ref infix (syntax-e f) #f)))
        (cond [(and op (>= (length args) 2))
               (format "(~a)" (string-join (map expr args) (format " ~a " op)))]
              [else (format "~a(~a)" (expr f) (string-join (map expr args) ", "))])]
       [else (error 'compile "cannot compile: ~a" (syntax->datum stx))])]))

(define (statement s)
  (define parts (and (syntax->list s) (syntax->list s)))
  (case (head s)
    [(define-values)
     (define name (syntax-e (car (syntax->list (cadr parts)))))
     (define rhs (caddr parts))
     (cond [(and (head rhs) (eq? (head rhs) 'lambda))
            (define lam (syntax->list rhs))
            (define params (map (lambda (p) (munged (syntax-e p)))
                                (syntax->list (cadr lam))))
            (define body (caddr lam))
            (format "def ~a(~a):\n    return ~a" (munged name)
                    (string-join params ", ") (expr body))]
           [else (format "~a = ~a" (munged name) (expr rhs))])]
    [(set!) (format "~a = ~a" (munged (syntax-e (cadr parts))) (expr (caddr parts)))]
    [(begin) (string-join (map statement (cdr parts)) "\n")]
    [else (expr s)]))

(define pieces
  (hasheq 'begin (list "def _begin(*values):" "    return values[-1]")
          'raise (list "class _Raised(Exception):" "    def __init__(self, value):"
                       "        super().__init__(value)" "        self.value = value" ""
                       "def _raise(value):" "    raise _Raised(value)")))

(define needed (make-hash))
(define (need piece) (hash-set! needed piece #t))

(define (compile-body body)
  (hash-clear! needed)
  (define lines (map statement body))
  (define prelude
    (append* (for/list ([p (in-list '(begin raise))] #:when (hash-ref needed p #f))
               (append (hash-ref pieces p) (list "")))))
  (string-append "# generated by rkt-pythonize\n"
                 (if (null? prelude) "" (string-append "\n" (string-join prelude "\n") "\n"))
                 (string-join lines "\n") "\n"))
