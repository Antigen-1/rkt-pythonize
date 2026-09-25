#lang racket/base

;; LB -> Python.
;;
;; The generated program is self-contained: it carries only the prelude pieces
;; it actually uses, and nothing else.  There is no Python runtime library.
;;
;;   (define x e)            x = e                       (module level)
;;   (define (f x* ...) e)   def f(x* ...): ...          (module level)
;;   (trampoline e ...)      tail calls inside become _Tail thunks, driven by
;;                           _trampoline(...) -- only where the source says
;;                           `trampoline`, never automatically
;;   (set! x e)              x = e                       (nonlocal when needed)
;;   (raise e)               raise _Raised(e)
;;   (with-handler h e)      try: ... except ...: h(value)
;;   (begin e ...)           statements, or _begin(...) as a value
;;   (if e1 e2 e3)           e2 if e1 else e3, or an if/else statement
;;   (e0 e* ...)             e0(e* ...)
;;   'd / l                  a literal Python value
;;
;; A free variable is a Python global, so `(print "hi")` is print("hi") and the
;; Python standard library is reachable through those names.

(require "base.rkt"
         nanopass/base
         racket/format
         racket/list
         racket/string)

(provide compile-LB
         python-name
         transpile)

;; ---------------------------------------------------------------------------
;; Names
;; ---------------------------------------------------------------------------

(define python-keywords
  '("False" "None" "True" "and" "as" "assert" "async" "await" "break" "class"
    "continue" "def" "del" "elif" "else" "except" "finally" "for" "from"
    "global" "if" "import" "in" "is" "lambda" "nonlocal" "not" "or" "pass"
    "raise" "return" "try" "while" "with" "yield"))

;; Names the generated prelude defines: a user variable with one of these names
;; gets a suffix.
(define prelude-names '("Symbol" "_Raised" "_Tail" "_trampoline" "_begin"))

(define character-names
  (hasheq #\- "_" #\? "_p" #\! "_b" #\< "_lt" #\> "_gt" #\= "_eq" #\/ "_slash"
          #\* "_star" #\+ "_plus" #\% "_pct" #\^ "_hat" #\& "_amp" #\| "_bar"
          #\~ "_tilde" #\@ "_at" #\$ "_dollar" #\: "_colon" #\. "_dot" #\# "_hash"))

;; A Scheme name as a readable Python identifier: `even?` -> `even_p`,
;; `set-car!` -> `set_car_b`, `if` -> `if_`.  Stable, so a name always compiles
;; to the same identifier.
(define (python-name sym)
  (define text (symbol->string sym))
  (define munged
    (list->string
     (append*
      (for/list ([ch (in-string text)])
        (cond [(or (char-alphabetic? ch) (char-numeric? ch) (char=? ch #\_)) (list ch)]
              [(hash-ref character-names ch #f) => string->list]
              [else (string->list (format "_u~a" (char->integer ch)))])))))
  (define base
    (cond [(string=? munged "") "_"]
          [(char-numeric? (string-ref munged 0)) (string-append "_" munged)]
          [else munged]))
  (cond [(member base python-keywords) (string-append base "_")]
        [(member base prelude-names) (string-append base "_")]
        [else base]))

;; ---------------------------------------------------------------------------
;; Literals and datums
;; ---------------------------------------------------------------------------

(define (python-string text)
  (define body
    (list->string
     (append*
      (for/list ([ch (in-string text)])
        (case ch
          [(#\\) (string->list "\\\\")]
          [(#\") (string->list "\\\"")]
          [(#\newline) (string->list "\\n")]
          [(#\tab) (string->list "\\t")]
          [(#\return) (string->list "\\r")]
          [else (if (char<? ch #\space)
                    (string->list (format "\\x~2,'0x" (char->integer ch)))
                    (list ch))])))))
  (string-append "\"" body "\""))

(define (python-tuple items)
  (define parts (map python-datum items))
  (string-append "(" (string-join parts ", ") (if (= 1 (length parts)) "," "") ")"))

(define (python-list items)
  (string-append "[" (string-join (map python-datum items) ", ") "]"))

(define (python-dict table)
  (define entries
    (for/list ([key (in-list (hash-keys table))])
      (format "~a: ~a" (python-datum key) (python-datum (hash-ref table key)))))
  (string-append "{" (string-join entries ", ") "}"))

;; A datum as Python source: symbols become interned `Symbol`s, lists become
;; Python lists, tuples become tuples, dicts become dicts.
(define (python-datum value)
  (cond [(integer? value) (format "~a" value)]
        [(flonum? value) (format "~a" value)]
        [(string? value) (python-string value)]
        [(boolean? value) (if value "True" "False")]
        [(symbol? value) (format "Symbol(~a)" (python-string (symbol->string value)))]
        [(list? value) (python-list value)]
        [(vector? value) (python-tuple (vector->list value))]
        [(hash? value) (python-dict value)]
        [else (error 'compile-LB "not an LB datum: ~a" value)]))

(define (datum-has-symbol? value)
  (cond [(symbol? value) #t]
        [(vector? value) (ormap datum-has-symbol? (vector->list value))]
        [(hash? value) (or (ormap datum-has-symbol? (hash-keys value))
                           (ormap datum-has-symbol? (hash-values value)))]
        [(list? value) (ormap datum-has-symbol? value)]
        [(pair? value) (or (datum-has-symbol? (car value)) (datum-has-symbol? (cdr value)))]
        [else #f]))

;; ---------------------------------------------------------------------------
;; Prelude
;; ---------------------------------------------------------------------------

(define prelude-pieces
  (hasheq
   'symbol
   '("class Symbol(str):"
     "    \"\"\"An interned Lisp symbol: a str subclass, so it compares and hashes"
     "    like its own name.\"\"\""
     "    __slots__ = ()"
     "    _pool = {}"
     "    def __new__(cls, name):"
     "        symbol = cls._pool.get(name)"
     "        if symbol is None:"
     "            symbol = str.__new__(cls, name)"
     "            cls._pool[name] = symbol"
     "        return symbol")
   'raised
   '("class _Raised(Exception):"
     "    \"\"\"The value of an explicit (raise v); it can be any LB value.\"\"\""
     "    def __init__(self, value):"
     "        super().__init__(value)"
     "        self.value = value")
   'trampoline
   '("class _Tail:"
     "    \"\"\"The thunk of a tail call inside an explicit (trampoline ...).\"\"\""
     "    __slots__ = (\"call\",)"
     "    def __init__(self, call):"
     "        self.call = call"
     ""
     "def _trampoline(value):"
     "    while isinstance(value, _Tail):"
     "        value = value.call()"
     "    return value")
   'begin
   '("def _begin(*values):"
     "    return values[-1]")))

(define prelude-order '(symbol raised trampoline begin))

;; ---------------------------------------------------------------------------
;; Emission context
;; ---------------------------------------------------------------------------

;; lines:    reversed list of (indent . text) pairs
;; indent:   the indentation of lines emitted next
;; features: prelude pieces used so far
;; temps:    counter for temporaries
;; scopes:   innermost first; each scope is a mutable list of the names it
;;           introduces.  A function scope carries its name, the module scope #f.
;; trampolines: the procedures of the program whose body uses `trampoline`
;; driving?: #f while emitting such a body: the enclosing driver forces the
;;           bounces, so the body itself must not start one
(struct context (lines indent features temps scopes trampolines driving?) #:mutable)

;; One lexical scope: its name (#f for the module scope) and the names it
;; introduces.  Mutable, because names are added while the body is emitted.
(struct scope (name names) #:mutable)

(define (make-context trampolines)
  (context '() 0 '() 0 (list (scope #f '())) trampolines #t))

(define (emit! ctx text)
  (set-context-lines! ctx (cons (cons (context-indent ctx) text) (context-lines ctx))))

(define (emit-lines! ctx lines)
  (for ([line (in-list lines)])
    (set-context-lines! ctx (cons line (context-lines ctx)))))

(define (need! ctx feature)
  (unless (memq feature (context-features ctx))
    (set-context-features! ctx (cons feature (context-features ctx)))))

(define (temp! ctx)
  (set-context-temps! ctx (add1 (context-temps ctx)))
  (format "_t~a" (context-temps ctx)))

;; Run `thunk` one level deeper and return the lines it emitted (they carry
;; their own indentation, so they can be re-emitted as they are).
(define (block ctx thunk)
  (define before (context-lines ctx))
  (set-context-indent! ctx (add1 (context-indent ctx)))
  (thunk)
  (set-context-indent! ctx (sub1 (context-indent ctx)))
  (define after (context-lines ctx))
  (set-context-lines! ctx before)
  (reverse (take after (- (length after) (length before)))))

(define (emit-block! ctx thunk)
  (define lines (block ctx thunk))
  (if (null? lines)
      (emit! ctx "pass")
      (emit-lines! ctx lines)))

;; scopes
(define (scope-push! ctx name)
  (set-context-scopes! ctx (cons (scope name '()) (context-scopes ctx))))

(define (scope-pop! ctx)
  (set-context-scopes! ctx (cdr (context-scopes ctx))))

(define (scope-define! ctx name)
  (define current (car (context-scopes ctx)))
  (set-scope-names! current (cons name (scope-names current))))

(define (outer-function-local? ctx name)
  (for/or ([s (in-list (cdr (context-scopes ctx)))])
    (and (scope-name s) (memq name (scope-names s)))))

;; ---------------------------------------------------------------------------
;; Scope analysis: what does a body assign, and what does it define?
;; ---------------------------------------------------------------------------

(define (assigned-names e)
  (nanopass-case (LB Expr) e
    ((set! ,x ,e1) (cons x (assigned-names e1)))
    ((define ,x ,e1) (assigned-names e1))
    ((define (,x ,x* ...) ,e1) (assigned-names e1))
    ((raise ,e1) (assigned-names e1))
    ((with-handler ,e1 ,e2) (append (assigned-names e1) (assigned-names e2)))
    ((begin ,e* ...) (append* (map assigned-names e*)))
    ((trampoline ,body ...) (append* (map assigned-names body)))
    ((if ,e1 ,e2 ,e3) (append (assigned-names e1) (assigned-names e2) (assigned-names e3)))
    ((,e0 ,e* ...) (append (assigned-names e0) (append* (map assigned-names e*))))
    (else '())))

(define (defined-names e)
  (nanopass-case (LB Expr) e
    ((define (,x ,x* ...) ,e1) (list x))
    ((define ,x ,e1) (list x))
    ((with-handler ,e1 ,e2) (defined-names e2))
    ((begin ,e* ...) (append* (map defined-names e*)))
    ((trampoline ,body ...) (append* (map defined-names body)))
    ((if ,e1 ,e2 ,e3) (append (defined-names e2) (defined-names e3)))
    (else '())))

;; ---------------------------------------------------------------------------
;; Trampolines
;; ---------------------------------------------------------------------------

;; Does this body use `trampoline` itself?  A nested `define` is a procedure of
;; its own and is analysed on its own.
(define (uses-trampoline? e)
  (nanopass-case (LB Expr) e
    ((trampoline ,body ...) #t)
    ((define ,x ,e1) #f)
    ((define (,x ,x* ...) ,e1) #f)
    ((raise ,e1) (uses-trampoline? e1))
    ((with-handler ,e1 ,e2) (uses-trampoline? e2))
    ((begin ,e* ...) (ormap uses-trampoline? e*))
    ((set! ,x ,e1) (uses-trampoline? e1))
    ((if ,e1 ,e2 ,e3) (or (uses-trampoline? e1) (uses-trampoline? e2) (uses-trampoline? e3)))
    ((,e0 ,e* ...) (or (uses-trampoline? e0) (ormap uses-trampoline? e*)))
    (else #f)))

;; Every procedure of the program whose body uses `trampoline`, so that a bounce
;; can call its body instead of its public entry.
(define (trampoline-procedures e)
  (nanopass-case (LB Expr) e
    ((define (,x ,x* ...) ,e1)
     (append (if (uses-trampoline? e1) (list x) '()) (trampoline-procedures e1)))
    ((define ,x ,e1) (trampoline-procedures e1))
    ((trampoline ,body ...) (append* (map trampoline-procedures body)))
    ((raise ,e1) (trampoline-procedures e1))
    ((with-handler ,e1 ,e2) (append (trampoline-procedures e1) (trampoline-procedures e2)))
    ((begin ,e* ...) (append* (map trampoline-procedures e*)))
    ((set! ,x ,e1) (trampoline-procedures e1))
    ((if ,e1 ,e2 ,e3)
     (append (trampoline-procedures e1) (trampoline-procedures e2) (trampoline-procedures e3)))
    ((,e0 ,e* ...) (append (trampoline-procedures e0) (append* (map trampoline-procedures e*))))
    (else '())))

;; A procedure that uses `trampoline` is emitted twice: `f` drives the
;; trampoline and `f_body` holds the bounces.  A bounce then calls a body, never
;; a driver, which is what keeps a deep loop flat.
(define (trampoline-body-name sym)
  (string->symbol (format "~a_body" (python-name sym))))

(define (bounce-name ctx sym)
  (if (memq sym (context-trampolines ctx))
      (trampoline-body-name sym)
      (python-name sym)))

;; The `nonlocal` names of a function: names it assigns that belong to an
;; enclosing function, minus the ones it defines itself.
(define (nonlocal-names ctx body)
  (define defined (defined-names body))
  (for/list ([name (in-list (remove-duplicates (assigned-names body)))]
             #:when (and (outer-function-local? ctx name)
                         (not (memq name defined))))
    name))

;; ---------------------------------------------------------------------------
;; Operators
;; ---------------------------------------------------------------------------

;; Names that read better as infix operators.  Without this `(+ 1 2)` would be a
;; call to an undefined `+`; with it the generated code stays close to the
;; source.
(define infix-operators
  (hasheq '+ "+" '- "-" '* "*" '/ "/" 'quotient "//" 'modulo "%" 'expt "**"
          '< "<" '> ">" '<= "<=" '>= ">=" '= "==" 'equal? "==" 'eq? "is"))

;; The arguments are never rendered in bounce position: an operator is a strict
;; primitive, so `(+ (f x) (g y))` has to compute both calls, not build thunks
;; for them (that would add two `_Tail` objects together).
(define (operator-expression ctx name args)
  (define op (hash-ref infix-operators name #f))
  (define (arg a) (python-expr ctx a))
  (cond
    [(and op (>= (length args) 2))
     (format "(~a)" (string-join (map arg args) (format " ~a " op)))]
    [(and (eq? name 'and) (pair? args))
     (format "(~a)" (string-join (map arg args) " and "))]
    [(eq? name 'and) "True"]
    [(and (eq? name 'or) (pair? args))
     (format "(~a)" (string-join (map arg args) " or "))]
    [(eq? name 'or) "False"]
    [(and (eq? name 'not) (= 1 (length args)))
     (format "(not ~a)" (arg (car args)))]
    [(and (eq? name 'negate) (= 1 (length args)))
     (format "(- ~a)" (arg (car args)))]
    [else #f]))

;; ---------------------------------------------------------------------------
;; Value positions
;; ---------------------------------------------------------------------------

(define (python-expr ctx e [bounce? #f])
  (nanopass-case (LB Expr) e
    (,x (python-name x))
    (,l (python-constant ctx l))
    (',d (python-constant ctx d))
    ((,e0 ,e* ...)
     (or (and (symbol? e0) (operator-expression ctx e0 e*))
         (let* ([callee (if (and bounce? (symbol? e0))
                            (bounce-name ctx e0)
                            (python-expr ctx e0))]
                [call (format "~a(~a)"
                              callee
                              (string-join (map (lambda (a) (python-expr ctx a)) e*) ", "))])
           (cond [bounce?
                  (need! ctx 'trampoline)
                  (format "_Tail(lambda: ~a)" call)]
                 [else call]))))
    ((if ,e1 ,e2 ,e3)
     (format "(~a if ~a else ~a)"
             (python-expr ctx e2 bounce?)
             (python-expr ctx e1)
             (python-expr ctx e3 bounce?)))
    ((begin ,e* ...)
     (cond [(null? e*) (need! ctx 'begin) "_begin()"]
           [(null? (cdr e*)) (python-expr ctx (car e*) bounce?)]
           [else
            ;; only the last expression is in tail position, so only it bounces
            (need! ctx 'begin)
            (format "_begin(~a)"
                    (string-join
                     (append (map (lambda (e) (python-expr ctx e)) (drop-right e* 1))
                             (list (python-expr ctx (last e*) bounce?)))
                     ", "))]))
    ((trampoline ,body ...)
     (cond
       [(null? body) "None"]
       [else
        (for ([e (in-list (drop-right body 1))]) (emit-stmt! ctx e))
        (define bounced (python-expr ctx (last body) #t))
        (need! ctx 'trampoline)
        ;; in bounce position the enclosing driver does the driving
        (if bounce? bounced (format "_trampoline(~a)" bounced))]))
    ((set! ,x ,e1)
     (emit! ctx (format "~a = ~a" (python-name x) (python-expr ctx e1)))
     "None")
    ((define (,x ,x* ...) ,e1)
     (emit-stmt! ctx e)
     "None")
    ((define ,x ,e1)
     (emit-stmt! ctx e)
     "None")
    ((raise ,e1)
     (need! ctx 'raised)
     (emit! ctx (format "raise _Raised(~a)" (python-expr ctx e1)))
     "None")
    ((with-handler ,e1 ,e2)
     (need! ctx 'raised)
     (define tmp (temp! ctx))
     (emit! ctx (format "~a = None" tmp))
     (emit! ctx "try:")
     (emit-block! ctx (lambda () (emit! ctx (format "~a = ~a" tmp (python-expr ctx e2)))))
     (emit! ctx "except _Raised as _e:")
     (emit-block! ctx (lambda () (emit! ctx (format "~a = ~a(_e.value)" tmp (python-expr ctx e1)))))
     (emit! ctx "except Exception as _e:")
     (emit-block! ctx (lambda () (emit! ctx (format "~a = ~a(_e)" tmp (python-expr ctx e1)))))
     tmp)))

(define (python-constant ctx value)
  (when (datum-has-symbol? value) (need! ctx 'symbol))
  (python-datum value))

;; ---------------------------------------------------------------------------
;; Statement positions
;; ---------------------------------------------------------------------------

(define (emit-stmt! ctx e [tail? #f])
  (nanopass-case (LB Expr) e
    ((define (,x ,x* ...) ,e1)
     (emit-function! ctx x x* e1))
    ((define ,x ,e1)
     (scope-define! ctx x)
     (emit! ctx (format "~a = ~a" (python-name x) (python-expr ctx e1))))
    ((set! ,x ,e1)
     (emit! ctx (format "~a = ~a" (python-name x) (python-expr ctx e1))))
    ((raise ,e1)
     (need! ctx 'raised)
     (emit! ctx (format "raise _Raised(~a)" (python-expr ctx e1))))
    ((with-handler ,e1 ,e2)
     (need! ctx 'raised)
     (define keyword (if tail? "return " ""))
     (define handler-text (python-expr ctx e1))
     (emit! ctx "try:")
     (emit-block! ctx (lambda () (emit-stmt! ctx e2 tail?)))
     (emit! ctx "except _Raised as _e:")
     (emit-block! ctx (lambda () (emit! ctx (format "~a~a(_e.value)" keyword handler-text))))
     (emit! ctx "except Exception as _e:")
     (emit-block! ctx (lambda () (emit! ctx (format "~a~a(_e)" keyword handler-text)))))
    ((begin ,e* ...)
     (emit-sequence! ctx e* tail?))
    ((trampoline ,body ...)
     (unless (null? body)
       (for ([e (in-list (drop-right body 1))]) (emit-stmt! ctx e))
       (define bounced (python-expr ctx (last body) #t))
       (need! ctx 'trampoline)
       ;; the tail of a trampoline body hands its bounce to the enclosing
       ;; driver; anywhere else a driver is needed right here
       (define driven (if (and tail? (not (context-driving? ctx)))
                          bounced
                          (format "_trampoline(~a)" bounced)))
       (emit! ctx (format "~a~a" (if tail? "return " "") driven))))
    ((if ,e1 ,e2 ,e3)
     (emit! ctx (format "if ~a:" (python-expr ctx e1)))
     (emit-block! ctx (lambda () (emit-stmt! ctx e2 tail?)))
     (emit! ctx "else:")
     (emit-block! ctx (lambda () (emit-stmt! ctx e3 tail?))))
    (else
     (cond [tail? (emit! ctx (format "return ~a" (python-expr ctx e)))]
           [(pure-value? e) (void)]
           [else (emit! ctx (python-expr ctx e))]))))

(define (pure-value? e)
  (nanopass-case (LB Expr) e
    (,x #t)
    (,l #t)
    (',d #t)
    (else #f)))

(define (emit-sequence! ctx body tail?)
  (define count (length body))
  (for ([e (in-list body)] [i (in-naturals)])
    (emit-stmt! ctx e (and tail? (= i (sub1 count))))))

;; Emit `def name(params):` followed by whatever `body-thunk` emits.
(define (emit-def! ctx name params body-thunk)
  (scope-push! ctx name)
  (for ([p (in-list params)]) (scope-define! ctx p))
  (define body-lines (block ctx body-thunk))
  (scope-pop! ctx)
  (emit! ctx (format "def ~a(~a):" (python-name name) (string-join (map python-name params) ", ")))
  (if (null? body-lines) (emit! ctx "pass") (emit-lines! ctx body-lines))
  (emit! ctx ""))

(define (emit-function! ctx name params body)
  (define params-text (string-join (map python-name params) ", "))
  (define (emit-body! trampoline-body?)
    (define saved (context-driving? ctx))
    (set-context-driving?! ctx (not trampoline-body?))
    (define nonlocals (nonlocal-names ctx body))
    (when (pair? nonlocals)
      (emit! ctx (format "nonlocal ~a" (string-join (map python-name nonlocals) ", "))))
    (emit-stmt! ctx body #t)
    (set-context-driving?! ctx saved))
  (scope-define! ctx name)
  (cond
    [(uses-trampoline? body)
     ;; `name` drives the trampoline and `name_body` holds its bounces, so that
     ;; a bounce never re-enters a driver and a deep loop stays flat.
     (define body-name (trampoline-body-name name))
     (scope-define! ctx body-name)
     (emit-def! ctx name params
                (lambda ()
                  (need! ctx 'trampoline)
                  (emit! ctx (format "return _trampoline(~a(~a))"
                                     (python-name body-name) params-text))))
     (emit-def! ctx body-name params (lambda () (emit-body! #t)))]
    [else (emit-def! ctx name params (lambda () (emit-body! #f)))]))

;; ---------------------------------------------------------------------------
;; Program
;; ---------------------------------------------------------------------------

(define (render-lines lines)
  (for/list ([line (in-list lines)])
    (if (zero? (car line))
        (cdr line)
        (string-append (make-string (* 4 (car line)) #\space) (cdr line)))))

;; LB source text -> Python source text.  Reading is Racket's own `read`, so
;; there is no lexer to maintain; a source file with several top-level forms
;; becomes one `(begin form ...)`, and an empty file an empty `(begin)`.
(define (transpile source)
  (define in (open-input-string source))
  (define (read-forms)
    (let loop ([forms '()])
      (define form (read in))
      (if (eof-object? form) (reverse forms) (loop (cons form forms)))))
  (define forms (read-forms))
  (compile-LB
   (parse-LB (cond [(null? forms) '(begin)]
                   [(null? (cdr forms)) (car forms)]
                   [else (cons 'begin forms)]))))

(define (compile-LB program)
  (define ctx (make-context (trampoline-procedures program)))
  (emit-stmt! ctx program #f)
  (define program-lines (render-lines (reverse (context-lines ctx))))
  (define prelude
    (append*
     (for/list ([feature (in-list prelude-order)]
                #:when (memq feature (context-features ctx)))
       (append (hash-ref prelude-pieces feature) (list "")))))
  (string-append
   (string-join (append (list "# generated by rkt-pythonize")
                        (if (null? prelude) '() (cons "" prelude))
                        program-lines)
                "\n")
   "\n"))
