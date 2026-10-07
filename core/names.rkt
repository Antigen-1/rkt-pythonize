#lang racket/base

;; What a Scheme name is in Python, and the name tables the passes share.
;;
;; This is the one module that turns a name into its Python name, and no other
;; pass spells a Python name out:
;;
;;   (python-name 'even?)      "even_p"      'camel is "isEven", the predicate
;;   (python-name 'set-car!)   "set_car_b"   "setCarB"
;;   (python-name 'object-ref) "object_ref"  "objectRef"
;;   (python-name 'class)      "class_"      a Python keyword gets a trailing _
;;   (python-name '1st)        "_1st"        a leading digit gets a leading _
;;   (python-keyword-name #:foo-bar)         the same, for a keyword argument
;;   (python-name 'list #:prefix "_lift_3f9a_")  "_lift_3f9a_list"
;;
;; The conversion reads the name and `python-name-style` and nothing else, so
;; the Python name of a top-level define is knowable from the source alone --
;; which is what a program that exports its names needs.  A prefix and a suffix
;; are Python text put around the conversion, not converted themselves.  Two
;; top-level names that come out as one Python name are a compile error, and a
;; name the compiler makes up is made of a UUID, so the name a program defines
;; at the top level is its own.
;;
;; `python-name-style` is 'snake (the default) or 'camel:
;;
;;   name          'snake      'camel
;;   even?         even_p      isEven      a trailing ? is the predicate
;;   a?b           a_pb        aPb         a ? that is not the end is P
;;   set-car!      set_car_b   setCarB
;;   object-ref    object_ref  objectRef
;;   class         class_      class_      a Python keyword stays a keyword
;;   1st           _1st        _1st        a name cannot start with a digit
;;
;; A program sets it where it sets the other parameters, before the forms it
;; means it for: (begin-for-syntax (python-name-style 'camel)).
;;
;; `runtime-names` are the LE symbols the runtime already carries -- the pieces
;; and the operators -- and `python-builtins` are the Python names a lifted
;; program may call.  Both are parameters, so a program that knows its Python
;; world can widen them:
;;
;;   (begin-for-syntax (runtime-names (cons 'sys (runtime-names))))
;;
;; and the scope check stops warning about what it names.

(require uuid)

(provide python-name python-keyword-name python-name-style
         generated-name generated-prefix prelude-prefix temporary-name
         piece-name piece-call called-piece
         infix prefix operators runtime-pieces pieces piece-order
         runtime-names python-builtins known-runtime-name? python-constant?)

;; ---- what a name is in Python

;; the styles a name is spelled in: - ends a word of a name, and the next one
;; is joined with _ or capitalized
(define styles '(snake camel))

(define python-name-style
  (make-parameter 'snake
                  (lambda (style)
                    (unless (memq style styles)
                      (error 'python-name-style "a style is ~s or ~s, and this is ~s"
                             (car styles) (cadr styles) style))
                    style)))

;; the Python name of a Scheme name.  The prefix and the suffix are Python
;; text around it -- a name the compiler makes up says so with one -- and what
;; they are put around is the conversion of the name itself
(define (python-name sym #:prefix [prefix ""] #:suffix [suffix ""])
  (string-append prefix (name-text (symbol->string sym)) suffix))

;; a keyword argument's name is a name: #:foo-bar is foo_bar, #:even? is even_p
(define (python-keyword-name keyword)
  (python-name (string->symbol (keyword->string keyword))))

(define (name-text text)
  (define last (sub1 (string-length text)))
  ;; with 'camel a name that ends with ? is the predicate it says it is: the ?
  ;; comes off, the first letter is capitalized, and is goes in front
  (define predicate? (and (eq? (python-name-style) 'camel)
                          (positive? (string-length text))
                          (char=? (string-ref text last) #\?)))
  (define joined (joined-words (if predicate? (substring text 0 last) text)))
  (define named (if predicate? (string-append "is" (capitalized joined)) joined))
  ;; a Python name cannot start with a digit, and a Python keyword cannot be a
  ;; name at all
  (define digits (if (and (positive? (string-length named))
                          (char-numeric? (string-ref named 0)))
                     (string-append "_" named)
                     named))
  (if (member digits reserved) (string-append digits "_") digits))

(define (capitalized text)
  (if (positive? (string-length text))
      (string-append (string (char-upcase (string-ref text 0))) (substring text 1))
      text))

;; - ? and ! say where one word of a name ends: a ? is a word of its own -- p for
;; "predicate", P where it is not the end of a name -- and ! is the bang, b or
;; B.  With 'camel the letter after a - is capitalized rather than joined, and a
;; - with no letter after it -- at the end of a name, or before punctuation --
;; is joined instead, so no name is lost: - is _, a- is a_, a--b is a_B, and -a
;; is A.
(define (joined-words text)
  (define camel? (eq? (python-name-style) 'camel))
  (define last (sub1 (string-length text)))
  (define words '())
  (define capitalize? #f)
  (for ([ch (in-string text)] [i (in-naturals)])
    (define emitted (if capitalize? (char-upcase ch) ch))
    (set! capitalize? #f)
    (cond [(and (char=? ch #\-) camel?
                (< i last)
                (char-alphabetic? (string-ref text (add1 i))))
           (set! capitalize? #t)]
          [(char=? ch #\-) (set! words (cons "_" words))]
          [(char=? ch #\?) (set! words (cons (if camel? "P" "_p") words))]
          [(char=? ch #\!) (set! words (cons (if camel? "B" "_b") words))]
          [else (set! words (cons (string emitted) words))]))
  (apply string-append (reverse words)))

;; ---- the names the compiler makes up

;; a procedure the compiler lifts is _lift_3f9a1b2c, or _lift_3f9a1b2c_inner
;; when the program called it inner: the prefix says the compiler made it up,
;; a UUID tells one from another, and the base says where the name came from.
;; Nothing has to be kept out of the way, since a name a program writes is not
;; a UUID it did not write down.
(define generated-prefix "_lift_")

(define (generated-name base)
  (string->symbol
   (if base
       (python-name base #:prefix (format "~a~a_" generated-prefix (uuid-part)))
       (format "~a~a" generated-prefix (uuid-part)))))

;; ---- the tables

;; operators are syntax, not values: with two or more operands they are infix
(define infix
  (for/hash ([p (in-list '((+ . "+") (- . "-") (* . "*") (/ . "/")
                           (quotient . "//") (modulo . "%") (expt . "**")
                           (& . "&") (\| . "|") (^ . "^") (<< . "<<") (>> . ">>")
                           (= . "==") (not= . "!=") (< . "<") (> . ">")
                           (<= . "<=") (>= . ">=") (equal? . "==") (eq? . "is")
                           (in . "in") (and . "and") (or . "or")))])
    (values (car p) (cdr p))))

;; with one operand these are prefix, and + is the operand itself
(define prefix (hash 'not "not" '- "-" '~ "~" '+ "+"))

;; every name that is an operator, so the passes can tell syntax from a name
(define operators (append (hash-keys infix) (hash-keys prefix) '(+) ))

(define runtime-pieces
  (for/hash ([p (in-list '((list . list) (apply . apply) (keyword-apply . keyword-apply)
                           (object-ref . object-ref) (object-set! . object-set!)
                           (object-get-attr . object-get-attr)
                           (object-set-attr! . object-set-attr!)
                           (object-has-attr? . object-has-attr?)))])
    (values (car p) (cdr p))))

;; Python's own keywords, which a name of a program may not be: one wears a
;; trailing _ instead.  False, None and True are not here: they are the
;; constants a program may read -- None is what the threading operators
;; interrupt on -- and binding one is what the checks refuse.
(define reserved
  '("and" "as" "assert" "async" "await" "break" "class"
    "continue" "def" "del" "elif" "else" "except" "finally" "for" "from"
    "global" "if" "import" "in" "is" "lambda" "nonlocal" "not" "or" "pass"
    "raise" "return" "try" "while" "with" "yield"))

;; begin is not here: a begin where a value belongs is lifted into a procedure
;; of its own, and one that stands as a statement is statements, so the runtime
;; never sees one
(define piece-order
  '(import raise trampoline with-handler list apply keyword-apply
    object-ref object-set! object-get-attr object-set-attr! object-has-attr?))

;; ---- the prelude
;;
;; The names the prelude's pieces are defined under carry a prefix the compiler
;; makes up for the run, so a name a program writes is never one of them: a
;; program may define list, and its own list is what (list ...) means where it
;; writes one, while the piece the compiler needs is _pz..._list and out of
;; reach.  Pin the prefix to have the same Python every time, or to nothing to
;; read the prelude as it is written here.

;; an identifier's worth of a UUID: the compiler's own names are made of these,
;; and a name a program writes is not one
(define (uuid-part)
  (substring (regexp-replace* #rx"-" (uuid-string) "") 0 8))

;; a prefix the run can call its own
(define (random-prefix)
  (format "_~a_" (uuid-part)))

(define prelude-prefix
  (make-parameter
   (random-prefix)
   (lambda (prefix)
     (unless (string? prefix)
       (error 'prelude-prefix "a prefix is a string, and this is ~s" prefix))
     prefix)))

;; the Python name of a piece, which is what a call to it renders
(define (piece-name piece)
  (string-append (prelude-prefix) (piece-own-name piece)))

;; what a piece is called inside the prelude: a piece that answers to an LE name
;; wears the Python name of that one, and the runtime's own are the table below
(define (piece-own-name piece)
  (cond [(hash-ref runtime-pieces piece #f) (python-name piece)]
        [(hash-ref own-piece-names piece #f)]
        [else (error 'piece-name "no Python name for the piece ~a" piece)]))

;; the pieces the runtime names itself: the names are Python names like any
;; other, so the one that is a Python keyword wears the underscore the
;; conversion gives it
(define own-piece-names
  (hasheq 'import "import_module" 'raise "raise_"
          'trampoline "trampoline" 'with-handler "with_handler"))

;; a name a generated form binds, where the program did not write one: the base
;; name when the forms around it do not use it, and one the compiler's own UUID
;; keeps apart when they do -- so a generated form cannot capture a name the
;; program wrote, and the common case still reads as the base name
(define (temporary-name base taken)
  (if (and (memq base taken) #t)
      (string->symbol (format "~a_~a" base (uuid-part)))
      base))

;; the LE name a pass writes where the compiler calls a piece itself: a program
;; writes the piece's own name, and the compiler writes this one, so a program
;; that defines apply still gets its own apply for the call it wrote, and the
;; compiler still gets the runtime's
(define (piece-call piece)
  (string->symbol (format "prelude-~a" piece)))

(define piece-calls
  (for/hash ([piece (in-list (hash-keys runtime-pieces))])
    (values (piece-call piece) piece)))

(define (called-piece sym) (hash-ref piece-calls sym #f))

;; The Python a piece carries: ~a is where the piece's own name goes, which is
;; what keeps a def and the call to it the one name, and ~t is where the
;; prelude's prefix goes, for the names the prelude keeps inside itself: the
;; exception the raise piece makes and the with-handler piece catches, and the
;; list the list piece keeps out of its own way.
(define pieces
  (hasheq
   'import (list "import importlib" ""
                  "def ~a(name):"
                  "    \"\"\"(import x): the Python module the string x names.\"\"\""
                  "    return importlib.import_module(name)")
   'raise (list "class ~tRaised(Exception):" "    def __init__(self, value):"
                "        super().__init__(value)" "        self.value = value" ""
                "def ~a(value):" "    raise ~tRaised(value)")
   'trampoline (list "def ~a(value):"
                     "    \"\"\"(trampoline e): call what the body returns while it is a procedure.\"\"\""
                     "    while callable(value):" "        value = value()" "    return value")
   'with-handler (list "def ~a(handler, body):"
                       "    \"\"\"(with-handler h e): the body, with h handling what it raises.\"\"\""
                       "    try:" "        return body()"
                       "    except ~tRaised as error:" "        return handler(error.value)"
                       "    except Exception as error:" "        return handler(error)")
   'list (list "~tbuiltin_list = list" ""
               "def ~a(*items):" "    return ~tbuiltin_list(items)")
   'apply (list "def ~a(function, *arguments, **keywords):"
                "    return function(*arguments[:-1], *arguments[-1], **keywords)")
   'keyword-apply (list "def ~a(function, keyword_arguments, arguments):"
                        "    return function(*arguments, **keyword_arguments)")
   'object-ref (list "def ~a(obj, key):" "    return obj[key]")
   'object-set! (list "def ~a(obj, key, value):" "    obj[key] = value")
   'object-get-attr (list "def ~a(obj, name):" "    return getattr(obj, name)")
   'object-set-attr! (list "def ~a(obj, name, value):" "    setattr(obj, name, value)")
   'object-has-attr? (list "def ~a(obj, name):" "    return hasattr(obj, name)")))

;; what the runtime carries for LE: the pieces and the operators
(define runtime-names
  (make-parameter (append (hash-keys runtime-pieces) operators)))

;; the Python constants a program may read by name: Python's own three
(define python-constants '("False" "None" "True"))

;; is this name one of them?  A program reads them, and does not bind them: the
;; conversion leaves the name as it is, and the checks say no to a binder.
(define (python-constant? sym)
  (and (member (python-name sym) python-constants) #t))

;; Python names a lifted program may call without the program defining them
(define python-builtins
  (make-parameter
   '("False" "None" "True"
     "abs" "all" "any" "bin" "bool" "bytes" "callable" "chr" "dict" "dir"
     "divmod" "enumerate" "filter" "float" "format" "frozenset" "getattr"
     "hasattr" "hash" "hex" "id" "input" "int" "isinstance" "issubclass" "iter"
     "len" "list" "locals" "map" "max" "min" "next" "object" "oct" "open" "ord"
     "pow" "print" "range" "repr" "reversed" "round" "set" "setattr" "slice"
     "sorted" "str" "sum" "super" "tuple" "type" "vars" "zip"
     "ArithmeticError" "AssertionError" "AttributeError" "Exception"
     "IndexError" "KeyError" "NameError" "NotImplementedError" "OSError"
     "RuntimeError" "StopIteration" "TypeError" "ValueError" "ZeroDivisionError")))

;; a name neither the program nor this list has to define
(define (known-runtime-name? sym)
  (or (and (member sym (runtime-names)) #t)
      (and (member (python-name sym) (python-builtins)) #t)))
