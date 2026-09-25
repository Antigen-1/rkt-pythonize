#lang racket/base

;; LB -> Python.
;;
;; The generated program is self-contained: it carries only the prelude pieces
;; it actually uses, and nothing else.  There is no Python runtime library.
;;
;; A statement is what a body, a `begin` in statement position, and the branch
;; of a statement `if` are made of; an expression is everything else, and an
;; expression is always a Python expression.  Nothing in a value position
;; emits a statement of its own, so a `define`, a `set!` or a `with-handler`
;; cannot hide inside one and mean something else there.
;;
;;   (define x e)            x = e                       (module level)
;;   (define (f x* ...) e)   def f(x* ...): ...          (module level)
;;   (trampoline e ...)      the body returns a 0-arity procedure, and
;;                           _trampoline(...) calls it while the value is one;
;;                           a tail call inside the body is what returns the
;;                           procedure.  Only where the source says
;;                           `trampoline`, never automatically
;;   (set! x e)              x = e                       (nonlocal when needed)
;;   (raise e)               raise _Raised(e)
;;   (with-handler h e)      try: ... except ...: h(value)
;;   (begin s ...)           statements, or _begin(...) as a value
;;   (if e1 s1 s2)           an if/else statement, of statements
;;   (if e1 e2 e3)           e2 if e1 else e3
;;   (with-handler h e)      _with_handler(h, lambda: e)
;;   (raise e)               _raise(e), or a raise statement
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
         python-name)

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
(define prelude-names '("Symbol" "_Raised" "_trampoline" "_begin" "_raise"
                        "_with_handler"))

(define character-names
  (hasheq #\- "_" #\? "_p" #\! "_b" #\< "_lt" #\> "_gt" #\= "_eq" #\/ "_slash"
          #\* "_star" #\+ "_plus" #\% "_pct" #\^ "_hat" #\& "_amp" #\| "_bar"
          #\~ "_tilde" #\@ "_at" #\$ "_dollar" #\: "_colon" #\. "_dot" #\# "_hash"))

;; Names that read better as infix operators.  Without this `(+ 1 2)` would be a
;; call to an undefined `+`; with it the generated code stays close to the
;; source, and the Python-side `eval` renders them the same way.
(define infix-operators
  (hasheq '+ "+" '- "-" '* "*" '/ "/" 'quotient "//" 'modulo "%" 'expt "**"
          '< "<" '> ">" '<= "<=" '>= ">=" '= "==" 'equal? "==" 'eq? "is"))

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

;; The entries of the tables the Python-side `eval` renders forms with.  They
;; come from the tables the emitter itself uses, so that both agree, and they
;; are sorted so that a program always compiles to the same text.
(define (name-character-entries)
  (for/list ([entry (in-list (sort (hash->list character-names) char<? #:key car))])
    (cons (python-string (string (car entry))) (python-string (cdr entry)))))

(define (name-reserved-entries)
  (for/list ([name (in-list (sort (remove-duplicates (append python-keywords prelude-names))
                                  string<?))])
    (python-string name)))

(define (infix-operator-entries)
  (for/list ([entry (in-list (sort (hash->list infix-operators)
                                   string<?
                                   #:key (lambda (entry) (symbol->string (car entry)))))])
    (cons (python-string (symbol->string (car entry))) (python-string (cdr entry)))))

(define (python-table entries)
  (string-append "{"
                 (string-join (for/list ([entry (in-list entries)])
                                (format "~a: ~a" (car entry) (cdr entry)))
                              ", ")
                 "}"))

(define (python-set entries)
  (string-append "{" (string-join entries ", ") "}"))

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
   '("def _trampoline(value):"
     "    \"\"\"(trampoline e ...): call the procedure the body returns, and keep"
     "    calling while the value is one.\"\"\""
     "    while callable(value):"
     "        value = value()"
     "    return value")
   'begin
   '("def _begin(*values):"
     "    return values[-1]")
   'object-ref
   '("def object_ref(obj, key):"
     "    \"\"\"(object-ref o k): the element at `k`, that is o[k].\"\"\""
     "    return obj[key]")
   'object-set!
   '("def object_set_b(obj, key, value):"
     "    \"\"\"(object-set! o k v): store `v` at `k`, that is o[k] = v.\"\"\""
     "    obj[key] = value")
   'object-get-attr
   '("def object_get_attr(obj, name):"
     "    \"\"\"(object-get-attr o name): the attribute, that is getattr(o, name).\"\"\""
     "    return getattr(obj, name)")
   'object-set-attr!
   '("def object_set_attr_b(obj, name, value):"
     "    \"\"\"(object-set-attr! o name v): set the attribute, that is setattr(o, name, v).\"\"\""
     "    setattr(obj, name, value)")
   'object-has-attr?
   '("def object_has_attr_p(obj, name):"
     "    \"\"\"(object-has-attr? o name): is the attribute there, that is hasattr(o, name).\"\"\""
     "    return hasattr(obj, name)")
   'raise
   '("def _raise(value):"
     "    \"\"\"(raise v) where a value is wanted: it never comes back.\"\"\""
     "    raise _Raised(value)")
   'with-handler
   '("def _with_handler(handler, body):"
     "    \"\"\"(with-handler h e): the body, with h handling what it raises.\"\"\""
     "    try:"
     "        return body()"
     "    except _Raised as error:"
     "        return handler(error.value)"
     "    except Exception as error:"
     "        return handler(error)")
   'import-module
   '("import importlib"
     ""
     "def import_module(name):"
     "    \"\"\"(import-module \"name\"): the module, like importlib.import_module.\"\"\""
     "    return importlib.import_module(name)")
   'list
   '("_builtin_list = list"
     ""
     "def list(*items):"
     "    \"\"\"(list x* ...): a list, which is what a quoted LB list already is.\"\"\""
     "    return _builtin_list(items)")
   'apply
   '("def apply(function, *arguments):"
     "    \"\"\"(apply f a* args): call f with `args` spread over the end.\"\"\""
     "    return function(*arguments[:-1], *arguments[-1])")
   'keyword-apply
   '("def keyword_apply(function, keyword_arguments, arguments):"
     "    \"\"\"(keyword-apply f kws args): call f with keyword arguments.\"\"\""
     "    return function(*arguments, **keyword_arguments)")
   'gensym
   '("_gensym_counter = 0"
     ""
     "def gensym(prefix=\"g\"):"
     "    \"\"\"(gensym) or (gensym prefix): a fresh symbol, interned like any other.\"\"\""
     "    global _gensym_counter"
     "    _gensym_counter = _gensym_counter + 1"
     "    return Symbol(\"%s%d\" % (prefix, _gensym_counter))")
   'eval
   (append
    (list
     ;; The tables below are generated from the tables `python-name` and the
     ;; operator emitter use, so that a form compiled here means the same thing
     ;; as the same form compiled by the transpiler.
     (format "_name_characters = ~a" (python-table (name-character-entries)))
     (format "_name_reserved = ~a" (python-set (name-reserved-entries)))
     (format "_infix_operators = ~a" (python-table (infix-operator-entries)))
     ;; The macro table the macro pass fills in; `{}` when a program has none.
     "_macros = []"
     ""
     "def _eval_name(value):"
     "    \"\"\"The Python name of a symbol: the munging `python-name` does.\"\"\""
     "    text = str(value)"
     "    parts = []"
     "    for character in text:"
     "        if character.isalnum() or character == \"_\":"
     "            parts.append(character)"
     "        elif character in _name_characters:"
     "            parts.append(_name_characters[character])"
     "        else:"
     "            parts.append(\"_u%d\" % ord(character))"
     "    name = \"\".join(parts)"
     "    if name == \"\":"
     "        name = \"_\""
     "    elif name[0].isdigit():"
     "        name = \"_\" + name"
     "    if name in _name_reserved:"
     "        name = name + \"_\""
     "    return name"
     ""
     "def _eval_literal(value):"
     "    \"\"\"Python source for a datum: a symbol becomes an interned Symbol.\"\"\""
     "    if isinstance(value, Symbol):"
     "        return \"Symbol(%r)\" % str(value)"
     "    if isinstance(value, _builtin_list):"
     "        return \"[%s]\" % \", \".join(_eval_literal(item) for item in value)"
     "    if isinstance(value, tuple):"
     "        if len(value) == 1:"
     "            return \"(%s,)\" % _eval_literal(value[0])"
     "        return \"(%s)\" % \", \".join(_eval_literal(item) for item in value)"
     "    if isinstance(value, dict):"
     "        return \"{%s}\" % \", \".join(\"%s: %s\" % (_eval_literal(key), _eval_literal(item))"
     "                                  for key, item in value.items())"
     "    if value is True:"
     "        return \"True\""
     "    if value is False:"
     "        return \"False\""
     "    if isinstance(value, float):"
     "        if value != value:"
     "            return \"float('nan')\""
     "        if value == float(\"inf\"):"
     "            return \"float('inf')\""
     "        if value == float(\"-inf\"):"
     "            return \"float('-inf')\""
     "    return repr(value)"
     ""
     "def _eval_condition(form):"
     "    \"\"\"LB's truth: only False is false.\"\"\""
     "    if isinstance(form, Symbol) or isinstance(form, _builtin_list):"
     "        return \"(%s is not False)\" % _eval_value(form)"
     "    return \"False\" if form is False else \"True\""
     ""
     "def _eval_import_line(spec):"
     "    \"\"\"The import statement an import spec asks for.\"\"\""
     "    if isinstance(spec, Symbol):"
     "        return \"import %s\" % spec"
     "    if spec and spec[0] == Symbol(\"as\"):"
     "        return \"import %s as %s\" % (spec[1], _eval_name(spec[2]))"
     "    if spec and spec[0] == Symbol(\"ref\"):"
     "        return \"from %s import %s\" % (spec[1], \", \".join(str(name) for name in spec[2:]))"
     "    raise SyntaxError(\"eval: not an import spec: %r\" % (spec,))"
     ""
     "def _eval_value(form):"
     "    \"\"\"Python source for `form` where its value is wanted.\"\"\""
     "    if isinstance(form, Symbol):"
     "        return _eval_name(form)"
     "    if not isinstance(form, _builtin_list):"
     "        return _eval_literal(form)"
     "    if not form:"
     "        raise Exception(\"eval: () is not a form\")"
     "    head = form[0]"
     "    if isinstance(head, Symbol):"
     "        if len(form) == 2 and head == Symbol(\"quote\"):"
     "            return _eval_literal(form[1])"
     "        if len(form) == 4 and head == Symbol(\"if\"):"
     "            return \"(%s if %s else %s)\" % (_eval_value(form[2]), _eval_condition(form[1]),"
     "                                            _eval_value(form[3]))"
     "        if len(form) >= 2 and head == Symbol(\"begin\"):"
     "            return \"_begin(%s)\" % \", \".join(_eval_value(item) for item in form[1:])"
     "        if len(form) >= 3 and head in _infix_operators:"
     "            return \"(%s)\" % (\" %s \" % _infix_operators[head]).join("
     "                _eval_value(item) for item in form[1:])"
     "        if head == Symbol(\"and\"):"
     "            if len(form) == 1:"
     "                return \"(True)\""
     "            return \"(%s)\" % \" and \".join(_eval_value(item) for item in form[1:])"
     "        if head == Symbol(\"or\"):"
     "            if len(form) == 1:"
     "                return \"(False)\""
     "            return \"(%s)\" % \" or \".join(_eval_value(item) for item in form[1:])"
     "        if len(form) == 2 and head == Symbol(\"not\"):"
     "            return \"(not %s)\" % _eval_value(form[1])"
     "        if len(form) == 2 and head == Symbol(\"negate\"):"
     "            return \"(- %s)\" % _eval_value(form[1])"
     "        if head in _macros:"
     "            # a macro call: hand the argument forms to the macro, then compile"
     "            # whatever form it returns"
     "            arguments = [_eval_literal(item) for item in form[1:]]"
     "            return \"eval(apply(%s, [%s]))\" % (_eval_name(head), \", \".join(arguments))"
     "        if len(form) == 3 and head == Symbol(\"set!\"):"
     "            return \"(%s := %s)\" % (_eval_name(form[1]), _eval_value(form[2]))"
     "        if head == Symbol(\"import\"):"
     "            raise SyntaxError(\"eval: import needs a statement position\")"
     "    return \"%s(%s)\" % (_eval_value(head), \", \".join(_eval_value(item) for item in form[1:]))"
     ""
     "def _eval_statements(form, lines, target, indent):"
     "    \"\"\"Append statements that leave the value of `form` in `target`.\"\"\""
     "    pad = \"    \" * indent"
     "    if isinstance(form, _builtin_list) and form and isinstance(form[0], Symbol):"
     "        head = form[0]"
     "        if head == Symbol(\"set!\") and len(form) == 3:"
     "            lines.append(\"%s%s = %s\" % (pad, _eval_name(form[1]), _eval_value(form[2])))"
     "            lines.append(\"%s%s = None\" % (pad, target))"
     "            return"
     "        if head == Symbol(\"define\") and len(form) == 3 and isinstance(form[1], Symbol):"
     "            lines.append(\"%s%s = %s\" % (pad, _eval_name(form[1]), _eval_value(form[2])))"
     "            lines.append(\"%s%s = None\" % (pad, target))"
     "            return"
     "        if head == Symbol(\"define\") and len(form) == 3 and isinstance(form[1], _builtin_list):"
     "            name = _eval_name(form[1][0])"
     "            parameters = \", \".join(_eval_name(item) for item in form[1][1:])"
     "            lines.append(\"%sdef %s(%s):\" % (pad, name, parameters))"
     "            body = []"
     "            _eval_statements(form[2], body, \"_eval_result\", indent + 1)"
     "            lines.extend(body)"
     "            lines.append(\"%s    return _eval_result\" % pad)"
     "            lines.append(\"%s%s = None\" % (pad, target))"
     "            return"
     "        if head == Symbol(\"begin\"):"
     "            parts = form[1:]"
     "            if not parts:"
     "                lines.append(\"%s%s = None\" % (pad, target))"
     "                return"
     "            for item in parts[:-1]:"
     "                _eval_statements(item, lines, \"_eval_discard\", indent)"
     "            _eval_statements(parts[-1], lines, target, indent)"
     "            return"
     "        if head == Symbol(\"if\") and len(form) == 4:"
     "            lines.append(\"%sif %s:\" % (pad, _eval_condition(form[1])))"
     "            _eval_statements(form[2], lines, target, indent + 1)"
     "            lines.append(\"%selse:\" % pad)"
     "            _eval_statements(form[3], lines, target, indent + 1)"
     "            return"
     "        if head == Symbol(\"raise\") and len(form) == 2:"
     "            lines.append(\"%sraise _Raised(%s)\" % (pad, _eval_value(form[1])))"
     "            lines.append(\"%s%s = None\" % (pad, target))"
     "            return"
     "        if head == Symbol(\"import\"):"
     "            for spec in form[1:]:"
     "                lines.append(pad + _eval_import_line(spec))"
     "            lines.append(\"%s%s = None\" % (pad, target))"
     "            return"
     "    lines.append(\"%s%s = %s\" % (pad, target, _eval_value(form)))"
     ""
     "def eval(form):"
     "    \"\"\"Evaluate a form built at run time, in the program's globals.\"\"\""
     "    lines = []"
     "    _eval_statements(form, lines, \"_eval_result\", 0)"
     "    globals()[\"_eval_result\"] = None"
     "    exec(\"\\n\".join(lines), globals())"
     "    result = globals()[\"_eval_result\"]"
     "    del globals()[\"_eval_result\"]"
     "    return result"))))

;; Pieces that cannot be emitted without others.
(define prelude-dependencies
  (hasheq 'gensym '(symbol)
          'raise '(raised)
          'with-handler '(raised)
          ;; a form a program builds at run time may call any of the runtime
          ;; functions, so eval brings them all along
          'eval '(symbol raised begin raise with-handler import-module
                  object-ref object-set! object-get-attr object-set-attr! object-has-attr?
                  list apply keyword-apply gensym)))

(define prelude-order
  '(symbol raised trampoline begin
    object-ref object-set! object-get-attr object-set-attr! object-has-attr?
    list apply keyword-apply gensym raise with-handler import-module eval))

;; The Python-side runtime functions.  A program reaches them as free variables,
;; and each one brings its prelude piece along; a program that defines one of
;; these names itself simply replaces the runtime function.
(define runtime-functions
  '(object-ref object-set! object-get-attr object-set-attr! object-has-attr?
    list apply keyword-apply gensym import-module eval))



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
(struct context (lines indent features temps scopes trampolines driving?)
  #:mutable)

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
    (set-context-features! ctx (cons feature (context-features ctx)))
    (for ([dependency (in-list (hash-ref prelude-dependencies feature '()))])
      (need! ctx dependency))))

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

;; An empty block is a Python syntax error, so it gets a `pass` -- and the
;; `pass` belongs to the block, at the indentation its lines would have had.
(define (emit-block! ctx thunk)
  (define lines (block ctx thunk))
  (emit-lines! ctx (if (null? lines)
                       (block ctx (lambda () (emit! ctx "pass")))
                       lines)))

;; scopes
(define (scope-push! ctx name)
  (set-context-scopes! ctx (cons (scope name '()) (context-scopes ctx))))

(define (scope-pop! ctx)
  (set-context-scopes! ctx (cdr (context-scopes ctx))))

(define (scope-define! ctx name)
  (define current (car (context-scopes ctx)))
  (set-scope-names! current (cons name (scope-names current))))

;; Is this name bound by a scope of the program itself?
(define (bound-name? ctx name)
  (for/or ([scope (in-list (context-scopes ctx))])
    (memq name (scope-names scope))))

;; A variable is a reference to a Python global unless a scope of the program
;; binds it.  Naming one of the runtime functions brings in its prelude piece.
(define (variable-expression ctx name)
  (unless (bound-name? ctx name)
    (when (memq name runtime-functions) (need! ctx name)))
  (python-name name))

(define (outer-function-local? ctx name)
  (for/or ([s (in-list (cdr (context-scopes ctx)))])
    (and (scope-name s) (memq name (scope-names s)))))

;; ---------------------------------------------------------------------------
;; Scope analysis: what does a body assign, and what does it define?
;; ---------------------------------------------------------------------------

(define (assigned-names s)
  (nanopass-case (LB Stmt) s
    ((define ,b ,body) (assigned-names-expr body))
    ((set! ,x ,e1) (cons x (assigned-names-expr e1)))
    ((begin ,s* ...) (append* (map assigned-names s*)))
    ((if ,e1 ,s1 ,s2)
     (append (assigned-names-expr e1) (assigned-names s1) (assigned-names s2)))
    (else (assigned-names-expr s))))

(define (assigned-names-expr e)
  (nanopass-case (LB Expr) e
    ((if ,e1 ,e2 ,e3)
     (append (assigned-names-expr e1)
             (assigned-names-expr e2)
             (assigned-names-expr e3)))
    ((begin ,e1 ,e* ...)
     (append (assigned-names-expr e1) (append* (map assigned-names-expr e*))))
    ((with-handler ,e1 ,e2) (append (assigned-names-expr e1) (assigned-names-expr e2)))
    ((trampoline ,e1) (assigned-names-expr e1))
    ((raise ,e1) (assigned-names-expr e1))
    ((,e0 ,e* ...) (append (assigned-names-expr e0) (append* (map assigned-names-expr e*))))
    (else '())))

(define (defined-names s)
  (nanopass-case (LB Stmt) s
    ;; a nested define is a procedure of its own: its body belongs to it
    ((define ,b ,body) (list (binding-name b)))
    ((begin ,s* ...) (append* (map defined-names s*)))
    ((if ,e1 ,s1 ,s2) (append (defined-names s1) (defined-names s2)))
    (else '())))

;; ---------------------------------------------------------------------------
;; Trampolines
;; ---------------------------------------------------------------------------

;; Does this body use `trampoline` itself?  A nested `define` is a procedure of
;; its own and is analysed on its own.
(define (uses-trampoline? s)
  (nanopass-case (LB Stmt) s
    ((define ,b ,body) #f)
    ((set! ,x ,e1) (uses-trampoline?-expr e1))
    ((begin ,s* ...) (ormap uses-trampoline? s*))
    ((if ,e1 ,s1 ,s2)
     (or (uses-trampoline?-expr e1) (uses-trampoline? s1) (uses-trampoline? s2)))
    (else (uses-trampoline?-expr s))))

(define (uses-trampoline?-expr e)
  (nanopass-case (LB Expr) e
    ((trampoline ,e1) #t)
    ((if ,e1 ,e2 ,e3)
     (or (uses-trampoline?-expr e1) (uses-trampoline?-expr e2) (uses-trampoline?-expr e3)))
    ((begin ,e1 ,e* ...)
     (or (uses-trampoline?-expr e1) (ormap uses-trampoline?-expr e*)))
    ((with-handler ,e1 ,e2) (or (uses-trampoline?-expr e1) (uses-trampoline?-expr e2)))
    ((raise ,e1) (uses-trampoline?-expr e1))
    ((,e0 ,e* ...) (or (uses-trampoline?-expr e0) (ormap uses-trampoline?-expr e*)))
    (else #f)))

;; Every procedure of the program whose body uses `trampoline`, so that a bounce
;; can call its body instead of its public entry.
(define (trampoline-procedures s)
  (nanopass-case (LB Stmt) s
    ((define ,b ,body)
     (append (if (and (pair? b) (uses-trampoline? body)) (list (binding-name b)) '())
             (trampoline-procedures body)))
    ((set! ,x ,e1) (trampoline-procedures-expr e1))
    ((begin ,s* ...) (append* (map trampoline-procedures s*)))
    ((if ,e1 ,s1 ,s2)
     (append (trampoline-procedures-expr e1)
             (trampoline-procedures s1)
             (trampoline-procedures s2)))
    (else (trampoline-procedures-expr s))))

(define (trampoline-procedures-expr e)
  (nanopass-case (LB Expr) e
    ((if ,e1 ,e2 ,e3)
     (append (trampoline-procedures-expr e1)
             (trampoline-procedures-expr e2)
             (trampoline-procedures-expr e3)))
    ((begin ,e1 ,e* ...)
     (append (trampoline-procedures-expr e1)
             (append* (map trampoline-procedures-expr e*))))
    ((with-handler ,e1 ,e2)
     (append (trampoline-procedures-expr e1) (trampoline-procedures-expr e2)))
    ((trampoline ,e1) (trampoline-procedures-expr e1))
    ((raise ,e1) (trampoline-procedures-expr e1))
    ((,e0 ,e* ...)
     (append (trampoline-procedures-expr e0)
             (append* (map trampoline-procedures-expr e*))))
    (else '())))

;; A procedure that uses `trampoline` is emitted twice: `f` drives the
;; trampoline and `f_body` holds the bounces.  A bounce then calls a body, never
;; a driver, which is what keeps a deep loop flat.
(define (trampoline-body-name sym)
  (string->symbol (format "~a_body" (python-name sym))))

;; What a bounce calls: the body of a trampolined procedure, so that the bounce
;; does not re-enter its driver, and in every other case the name itself -- as a
;; reference, so that a runtime function among them brings its prelude piece.
(define (bounce-name ctx sym)
  (if (memq sym (context-trampolines ctx))
      (trampoline-body-name sym)
      (variable-expression ctx sym)))

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


;; The arguments are never rendered in bounce position: an operator is a strict
;; primitive, so `(+ (f x) (g y))` has to compute both calls, not build the
;; procedures a trampoline would call for them (that would try to add two
;; functions together).
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
    (,x (variable-expression ctx x))
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
                  ;; a tail call inside a trampoline is the 0-arity procedure
                  ;; the trampoline will call
                  (need! ctx 'trampoline)
                  (format "lambda: ~a" call)]
                 [else call]))))
    ((if ,e1 ,e2 ,e3)
     (format "(~a if ~a else ~a)"
             (python-expr ctx e2 bounce?)
             (python-condition ctx e1)
             (python-expr ctx e3 bounce?)))
    ((begin ,e1 ,e* ...)
     (cond [(null? e*) (python-expr ctx e1 bounce?)]
           [else
            ;; only the last expression is in tail position, so only it bounces
            (need! ctx 'begin)
            (format "_begin(~a)"
                    (string-join
                     (append (map (lambda (e) (python-expr ctx e))
                                  (cons e1 (drop-right e* 1)))
                             (list (python-expr ctx (last e*) bounce?)))
                     ", "))]))
    ((with-handler ,e1 ,e2)
     ;; the handler is a value, so it is evaluated before the body runs; the body
     ;; is a procedure so that it runs where it stands, and inside a Python try
     (need! ctx 'with-handler)
     (format "_with_handler(~a, lambda: ~a)"
             (python-expr ctx e1)
             (python-expr ctx e2)))
    ((trampoline ,e1)
     (need! ctx 'trampoline)
     (define bounced (python-expr ctx e1 #t))
     ;; in bounce position the enclosing driver does the driving
     (if bounce? bounced (format "_trampoline(~a)" bounced)))
    ((raise ,e1)
     (need! ctx 'raise)
     (format "_raise(~a)" (python-expr ctx e1)))
    (else
     (error 'compile-LB "a statement cannot be used as an expression: ~a" e))))

(define (python-condition ctx e)
  (nanopass-case (LB Expr) e
    (,l (if (eq? l #f) "False" "True"))
    (',d (if (eq? d #f) "False" "True"))
    (else (format "(~a is not False)" (python-expr ctx e)))))

(define (python-constant ctx value)
  (when (datum-has-symbol? value) (need! ctx 'symbol))
  (python-datum value))

;; ---------------------------------------------------------------------------
;; Statement positions
;; ---------------------------------------------------------------------------

(define (emit-stmt! ctx s [tail? #f])
  (nanopass-case (LB Stmt) s
    ((define ,b ,body)
     (cond [(pair? b) (emit-function! ctx b body)]
           [else
            (scope-define! ctx b)
            (emit! ctx (format "~a = ~a" (python-name b) (python-expr ctx body)))]))
    ((set! ,x ,e1)
     (emit! ctx (format "~a = ~a" (python-name x) (python-expr ctx e1))))
    ((begin ,s* ...)
     (emit-sequence! ctx s* tail?))
    ((if ,e1 ,s1 ,s2)
     (emit! ctx (format "if ~a:" (python-condition ctx e1)))
     (emit-block! ctx (lambda () (emit-stmt! ctx s1 tail?)))
     (emit! ctx "else:")
     (emit-block! ctx (lambda () (emit-stmt! ctx s2 tail?))))
    (else
     ;; an expression in statement position: its value if this is a tail, its
     ;; effect otherwise
     (cond [tail? (emit! ctx (format "return ~a" (tail-expression ctx s)))]
           [(pure-value? s) (void)]
           [else (emit! ctx (python-expr ctx s))]))))

;; The value a body ends with.  Inside a trampoline body the trampoline's own
;; expression is not driven here: it is the bounce the enclosing driver calls.
(define (tail-expression ctx e)
  (cond [(and (not (context-driving? ctx)) (trampoline-expression? e))
         (python-expr ctx (trampoline-expression-body e) #t)]
        [else (python-expr ctx e)]))

(define (trampoline-expression? e)
  (nanopass-case (LB Expr) e
    ((trampoline ,e1) #t)
    (else #f)))

(define (trampoline-expression-body e)
  (nanopass-case (LB Expr) e
    ((trampoline ,e1) e1)
    (else e)))

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
(define (emit-def! ctx name params-text body-thunk)
  (scope-push! ctx name)
  (define body-lines (block ctx body-thunk))
  (scope-pop! ctx)
  (emit! ctx (format "def ~a(~a):" (python-name name) params-text))
  (emit-lines! ctx (if (null? body-lines)
                       (block ctx (lambda () (emit! ctx "pass")))
                       body-lines))
  (emit! ctx ""))

(define (emit-function! ctx signature body)
  (define name (binding-name signature))
  (define-values (params rest) (binding-parts signature))
  ;; `def f(a, *rest)` and the call that forwards to a trampoline body are the
  ;; same text, so one string does for both.
  (define params-text
    (string-append (string-join (map python-name params) ", ")
                   (if rest
                       (format "~a*~a" (if (null? params) "" ", ") (python-name rest))
                       "")))
  (define (emit-body! trampoline-body?)
    (define saved (context-driving? ctx))
    (set-context-driving?! ctx (not trampoline-body?))
    (define nonlocals (nonlocal-names ctx body))
    (when (pair? nonlocals)
      (emit! ctx (format "nonlocal ~a" (string-join (map python-name nonlocals) ", "))))
    ;; a rest parameter is a Python tuple: make it the list an LB list is
    (when rest
      (emit! ctx (format "~a = [*~a]" (python-name rest) (python-name rest))))
    (emit-stmt! ctx body #t)
    (set-context-driving?! ctx saved))
  (scope-define! ctx name)
  (for ([param (in-list params)]) (scope-define! ctx param))
  (when rest (scope-define! ctx rest))
  (cond
    [(uses-trampoline? body)
     ;; `name` drives the trampoline and `name_body` holds its bounces, so that
     ;; a bounce never re-enters a driver and a deep loop stays flat.
     (define body-name (trampoline-body-name name))
     (scope-define! ctx body-name)
     (emit-def! ctx name params-text
                (lambda ()
                  (need! ctx 'trampoline)
                  (emit! ctx (format "return _trampoline(~a(~a))"
                                     (python-name body-name) params-text))))
     (emit-def! ctx body-name params-text (lambda () (emit-body! #t)))]
    [else (emit-def! ctx name params-text (lambda () (emit-body! #f)))]))

;; ---------------------------------------------------------------------------
;; Program
;; ---------------------------------------------------------------------------

(define (render-lines lines)
  (for/list ([line (in-list lines)])
    (if (zero? (car line))
        (cdr line)
        (string-append (make-string (* 4 (car line)) #\space) (cdr line)))))

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
