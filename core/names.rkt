#lang racket/base

;; The name tables the passes share, and the two lists a program can adjust.
;;
;; `runtime-names` are the LE symbols the runtime already carries -- the pieces
;; and the operators -- and `python-builtins` are the Python names a lifted
;; program may call.  Both are parameters, so a program that knows its Python
;; world can widen them:
;;
;;   (begin-for-syntax (runtime-names (cons 'sys (runtime-names))))
;;
;; and the scope check stops warning about what it names.

(require racket/list
         racket/string)

(provide munged infix runtime-pieces pieces piece-order
         runtime-names python-builtins known-runtime-name?)

(define infix
  (for/hash ([p (in-list '((+ . "+") (- . "-") (* . "*") (/ . "/")
                           (= . "==") (< . "<") (> . ">") (<= . "<=") (>= . ">=")
                           (equal? . "==") (eq? . "is")
                           (quotient . "//") (modulo . "%") (expt . "**")
                           (and . "and") (or . "or")))])
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

(define piece-order
  '(import begin raise trampoline with-handler list apply keyword-apply
    object-ref object-set! object-get-attr object-set-attr! object-has-attr?))

(define pieces
  (hasheq
   'import (list "import importlib" ""
                  "def import_module(name):"
                  "    \"\"\"(import x): the Python module the string x names.\"\"\""
                  "    return importlib.import_module(name)")
   'begin (list "def _begin(*values):" "    return values[-1]")
   'raise (list "class _Raised(Exception):" "    def __init__(self, value):"
                "        super().__init__(value)" "        self.value = value" ""
                "def _raise(value):" "    raise _Raised(value)")
   'trampoline (list "def _trampoline(value):"
                     "    \"\"\"(trampoline e): call what the body returns while it is a procedure.\"\"\""
                     "    while callable(value):" "        value = value()" "    return value")
   'with-handler (list "def _with_handler(handler, body):"
                       "    \"\"\"(with-handler h e): the body, with h handling what it raises.\"\"\""
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

;; what the runtime carries for LE: the pieces, the operators, and not
(define runtime-names
  (make-parameter (append (hash-keys runtime-pieces) (hash-keys infix) '(not))))

;; Python names a lifted program may call without the program defining them
(define python-builtins
  (make-parameter
   '("abs" "all" "any" "bin" "bool" "bytes" "callable" "chr" "dict" "dir"
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
      (and (member (munged sym) (python-builtins)) #t)))
