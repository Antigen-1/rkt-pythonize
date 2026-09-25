#lang racket/base

;; LB -> Python, at expansion time: the expanded body of a module in, Python
;; source out.  Names go to Python by name; a free name is a Python global, and
;; the few names the runtime provides bring their prelude lines along.

(require racket/list
         racket/string)

(provide compile-body)

(define infix
  (for/hash ([p (in-list '((+ . "+") (- . "-") (* . "*") (/ . "/")
                           (= . "==") (< . "<") (> . ">") (<= . "<=") (>= . ">=")
                           (equal? . "==")))])
    (values (car p) (cdr p))))

(define runtime-pieces
  (for/hash ([p (in-list '((list . list) (apply . apply) (keyword-apply . keyword-apply)
                           (object-ref . object-ref) (object-set! . object-set!)
                           (object-get-attr . object-get-attr)
                           (object-set-attr! . object-set-attr!)
                           (object-has-attr? . object-has-attr?)))])
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
          [(#\-) (string->list "_")] [(#\?) (string->list "_p")] [(#\!) (string->list "_b")]
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
        [(hash? value)
         (format "{~a}" (string-join (for/list ([k (in-list (hash-keys value))])
                                       (format "~a: ~a" (py-datum k) (py-datum (hash-ref value k))))
                                     ", "))]
        [else (error 'compile "not data: ~a" value)]))

(define (head stx) (and (pair? (syntax->list stx)) (syntax-e (car (syntax->list stx)))))

(define (mentions? stx sym)
  (cond [(identifier? stx) (eq? (syntax-e stx) sym)]
        [(not (syntax->list stx)) #f]
        ;; a nested procedure drives its own
        [(eq? (head stx) 'lambda) #f]
        [else (ormap (lambda (p) (mentions? p sym)) (syntax->list stx))]))

(define trampolined (make-hash))
(define (body-name sym) (string-append (munged sym) "_body"))

;; what a bounce calls: the body of a trampolined procedure, so that the bounce
;; does not re-enter its driver
(define (bounce-callee f)
  (define sym (callee-symbol f))
  (if (and sym (hash-ref trampolined sym #f)) (body-name sym) (expr f)))

;; the value a body function ends with: inside a trampoline body the trampoline
;; is the bounce itself, not a driver
(define (trampoline-arg stx)
  (define parts (syntax->list stx))
  (cond [(not parts) #f]
        [(eq? (head stx) '#%lb-trampoline) (cadr parts)]
        [(and (eq? (head stx) '#%app) (= 3 (length parts))
              (eq? (callee-symbol (cadr parts)) '#%lb-trampoline))
         (caddr parts)]
        [else #f]))

(define (tail-expr stx driver?)
  (define arg (and (not driver?) (trampoline-arg stx)))
  (if arg (expr arg #t) (expr stx)))

(define (callee-symbol stx) (and (identifier? stx) (syntax-e stx)))

(define (global-expr sym)
  (when (hash-ref runtime-pieces sym #f) (need (hash-ref runtime-pieces sym)))
  (munged sym))

(define (global-name stx)
  (define datum (syntax->datum stx))
  (cond [(symbol? datum) datum]
        [(and (pair? datum) (memq (car datum) '(quote #%quote)) (symbol? (cadr datum)))
         (cadr datum)]
        [else (error 'compile "not a global name: ~a" datum)]))

;; an expression; in bounce position an application is the 0-arity procedure a
;; trampoline calls instead of a call
(define (expr stx [bounce? #f])
  (cond
    [(identifier? stx) (global-expr (syntax-e stx))]
    [(not (syntax->list stx)) (py-datum (syntax->datum stx))]
    [else
     (define parts (syntax->list stx))
     (case (head stx)
       [(quote) (py-datum (syntax->datum (cadr parts)))]
       [(if) (format "(~a if ~a else ~a)"
                     (expr (caddr parts) bounce?) (expr (cadr parts)) (expr (cadddr parts) bounce?))]
       [(begin) (format "_begin(~a)"
                        (string-join (for/list ([e (in-list (cdr parts))] [i (in-naturals)])
                                       (expr e (and bounce? (= i (- (length parts) 2)))))
                                     ", "))]
       [(#%lb-global)
        (define name (global-expr (global-name (cadr parts))))
        (define more (cddr parts))
        (if (null? more) name (format "~a(~a)" name (string-join (map expr more) ", ")))]
       [(#%lb-raise) (need 'raise) (format "_raise(~a)" (expr (cadr parts)))]
       [(#%lb-trampoline) (need 'trampoline) (format "_trampoline(~a)" (expr (cadr parts) #t))]
       [(#%lb-with-handler) (need 'with-handler)
                             (format "_with_handler(~a, lambda: ~a)"
                                     (expr (cadr parts)) (expr (caddr parts)))]
       [(lambda) (error 'compile "a lambda has to be a definition's value")]
       [(#%app)
        (define f (cadr parts))
        (define args (cddr parts))
        (define core (callee-symbol f))
        (define f-name (and (identifier? f) (munged (syntax-e f))))
        (cond [(eq? core '#%lb-global)
               (define name (global-expr (global-name (car args))))
               (if (null? (cdr args)) name
                   (format "~a(~a)" name (string-join (map expr (cdr args)) ", ")))]
              [(eq? core '#%lb-raise) (need 'raise) (format "_raise(~a)" (expr (car args)))]
              [(eq? core '#%lb-trampoline) (need 'trampoline) (format "_trampoline(~a)" (expr (car args) #t))]
              [(eq? core '#%lb-with-handler) (need 'with-handler)
                                            (format "_with_handler(~a, lambda: ~a)"
                                                    (expr (car args)) (expr (cadr args)))]
              [(and f-name (hash-ref infix (syntax-e f) #f) (>= (length args) 2))
               (format "(~a)" (string-join (map expr args) (format " ~a " (hash-ref infix (syntax-e f)))))]
              [else
               (define call (format "~a(~a)" (if bounce? (bounce-callee f) (expr f))
                                        (string-join (map expr args) ", ")))
               (if bounce? (begin (need 'trampoline) (format "lambda: ~a" call)) call)])]
       [else (error 'compile "cannot compile: ~a" (syntax->datum stx))])]))

(define (statement s)
  (define parts (syntax->list s))
  (case (head s)
    ;; compile-time only: the expander has done its work
    [(define-syntaxes define-syntax begin-for-syntax) ""]
    [(define-values)
     (define name (syntax-e (car (syntax->list (cadr parts)))))
     (define rhs (caddr parts))
     (cond [(eq? (head rhs) 'lambda)
            (define lam (syntax->list rhs))
            (define params (map (lambda (p) (munged (syntax-e p))) (syntax->list (cadr lam))))
            (define body-forms (cddr lam))
            (define earlier (drop-right body-forms 1))
            (define (def def-name tail)
              (string-append
               (format "def ~a(~a):\n" def-name (string-join params ", "))
               (if (null? earlier)
                   ""
                   (string-append (string-join (for/list ([l (in-list (map statement earlier))])
                                                 (string-append "    " l))
                                               "\n")
                                  "\n"))
               "    return " tail "\n"))
            (cond [(hash-ref trampolined name #f)
                   ;; `name` drives, `name_body` holds the bounces
                   (need 'trampoline)
                   (define bn (body-name name))
                   (string-append
                    (def (munged name) (format "_trampoline(~a(~a))" bn (string-join params ", ")))
                    "\n"
                    (def bn (tail-expr (last body-forms) #f)))]
                  [else (def (munged name) (expr (last body-forms)))])]
           [else (format "~a = ~a" (munged name) (expr rhs))])]
    [(set!) (format "~a = ~a" (munged (syntax-e (cadr parts))) (expr (caddr parts)))]
    [(begin) (string-join (map statement (cdr parts)) "\n")]
    [else (expr s)]))

(define pieces
  (hasheq
   'begin (list "def _begin(*values):" "    return values[-1]")
   'raise (list "class _Raised(Exception):" "    def __init__(self, value):"
                "        super().__init__(value)" "        self.value = value" ""
                "def _raise(value):" "    raise _Raised(value)")
   'trampoline (list "def _trampoline(value):"
                     "    \"\"\"(lb:trampoline e): call what the body returns while it is a procedure.\"\"\""
                     "    while callable(value):" "        value = value()" "    return value")
   'with-handler (list "def _with_handler(handler, body):"
                       "    \"\"\"(lb:with-handler h e): the body, with h handling what it raises.\"\"\""
                       "    try:" "        return body()"
                       "    except _Raised as error:" "        return handler(error.value)"
                       "    except Exception as error:" "        return handler(error)")
   'list (list "_builtin_list = list" ""
               "def list(*items):" "    return _builtin_list(items)")
   'apply (list "def apply(function, *arguments):"
                "    return function(*arguments[:-1], *arguments[-1])")
   'keyword-apply (list "def keyword_apply(function, keyword_arguments, arguments):"
                        "    return function(*arguments, **keyword_arguments)")
   'object-ref (list "def object_ref(obj, key):" "    return obj[key]")
   'object-set! (list "def object_set_b(obj, key, value):" "    obj[key] = value")
   'object-get-attr (list "def object_get_attr(obj, name):" "    return getattr(obj, name)")
   'object-set-attr! (list "def object_set_attr_b(obj, name, value):" "    setattr(obj, name, value)")
   'object-has-attr? (list "def object_has_attr_p(obj, name):" "    return hasattr(obj, name)")))

(define order
  '(begin raise trampoline with-handler list apply keyword-apply
    object-ref object-set! object-get-attr object-set-attr! object-has-attr?))

(define needed (make-hash))
(define (need piece) (hash-set! needed piece #t))

(define (compile-body body)
  (hash-clear! needed)
  (hash-clear! trampolined)
  (for ([s (in-list body)])
    (define parts (syntax->list s))
    (when (and parts (eq? (head s) 'define-values))
      (define rhs (caddr parts))
      (when (and (eq? (head rhs) 'lambda) (ormap (lambda (b) (mentions? b '#%lb-trampoline)) (cddr (syntax->list rhs))))
        (hash-set! trampolined (syntax-e (car (syntax->list (cadr parts)))) #t))))
  (define lines (filter (lambda (line) (not (string=? line ""))) (map statement body)))
  (define prelude
    (append* (for/list ([p (in-list order)] #:when (hash-ref needed p #f))
               (append (hash-ref pieces p) (list "")))))
  (string-append "# generated by rkt-pythonize\n"
                 (if (null? prelude) "" (string-append "\n" (string-join prelude "\n") "\n"))
                 (string-join lines "\n") "\n"))
