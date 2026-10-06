#lang scribble/manual

@require[@for-label[rkt-pythonize
                    racket/base]]

@; the examples below are produced by running the library while this document
@; is built: an example that stops working fails the build
@(require rkt-pythonize
          racket/port
          racket/system)

@(define example-namespace (make-base-namespace))
@(parameterize ([current-namespace example-namespace])
   (eval '(require rkt-pythonize)))

@; the Macros section's example runs in that namespace, so its defmacro has to
@; be evaluated there: a macro is visible where its name is bound
@(parameterize ([current-namespace example-namespace])
   (eval '(defmacro (twice form) `(begin ,form ,form))))

@(define (python-of forms)
   (parameterize ([current-namespace example-namespace])
     (eval `(#%python-code ,@forms))))

@; python3 first, then python, and PYTHON_EXE overrides both
@(define (find-python)
   (define candidates
     (if (getenv "PYTHON_EXE")
         (list (getenv "PYTHON_EXE") "python3" "python")
         (list "python3" "python")))
   (or (for/or ([candidate (in-list candidates)])
         (and candidate (find-executable-path candidate)))
       (error 'rkt-pythonize-manual
              "no Python found: set PYTHON_EXE, or put python3 or python on PATH")))

@(define (output-of forms)
   (define out (open-output-string))
   (define err (open-output-string))
   (define status
     (parameterize ([current-output-port out] [current-error-port err])
       (system* (find-python) "-c" (python-of forms))))
   (unless status
     (error 'rkt-pythonize-manual "the example did not run: ~a" (get-output-string err)))
   (get-output-string out))

@title{rkt-pythonize}
@author{zhanghao}

@defmodule[rkt-pythonize]

@bold{rkt-pythonize} is an LE-to-Python compiler behind one macro.  The library
exports @racket[#%python-code], whose body is LE and whose value is the Python
it compiles to.  The compilation happens at expansion time, so the value is a
string.  @filepath{CHANGELOG.md} says what changed in each version.

@table-of-contents[]

@; the README and the changelog are chapters of this manual, and each is also a
@; Markdown file of its own: build-docs.sh renders readme.scrbl and
@; changelog.scrbl on their own into README.md and CHANGELOG.md
@include-section["readme.scrbl"]

@section[#:tag "quick"]{Quick start}

@codeblock|{
#lang racket/base
(require rkt-pythonize)

(define python
  (#%python-code
    (define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))
    (print (fact 5))))

(displayln python)
}|

The Python it renders, produced by running @racket[#%python-code] as this
manual is built:

@verbatim{@(python-of '((define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))
                        (print (fact 5))))}

and what running that Python prints:

@verbatim{@(output-of '((define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))
                        (print (fact 5))))}

@defform[(#%python-code form ...)]{The Python that the LE forms compile to, as a
string.  It is a macro: the rendering happens while the enclosing module is
compiled, and there is nothing left of it at run time.}

@section{Pipeline}

@itemlist[
@item{@filepath{core/expand-macro.rkt} -- LE to LE: a macro a @racket[defmacro]
bound is expanded where its name is bound.}
@item{@filepath{core/expand-cond.rkt} -- LE to LE: a @racket[cond] becomes the
nested @racket[if]s it means, each clause body a @racket[begin].}
@item{@filepath{core/check-expression.rkt} -- LE to LE: refuses a statement where
an expression belongs, and a Racket form that is not LE.}
@item{@filepath{core/check-scope.rkt} -- LE to LE: a name bound by a parameter, by
a define in its body or by the program at the top level is in scope, and any
other name is a Python global the compiler logs a warning about.}
@item{@filepath{core/explicit.rkt} -- LE to LL: a form that takes a thunk gets an
explicit @racket[lambda] for its body.}
@item{@filepath{core/lift.rkt} -- LL to LB: every @racket[lambda] and every
locally defined procedure becomes a top-level define that takes the variables it
captures as leading parameters.}
@item{@filepath{core/render.rkt} -- LB to Python: the source, with the pieces the
program asked for.}]

The names a Python program has without the source defining them are two
parameters in @filepath{core/names.rkt}: @racket[runtime-names] (the pieces and
operators LE may use) and @racket[python-builtins] (the Python names a lifted
program may call).  Widen either -- for instance
@code{(begin-for-syntax (runtime-names (cons 'sys (runtime-names))))} -- and the
scope check stops warning about what you added.

@section{LE}

A statement is what a body, a @racket[begin] in statement position and the
branch of a statement @racket[if] are made of; an expression is what an
@racket[if], a call argument and a @racket[begin] in expression position are
made of.  Nothing in an expression position is a statement, so a definition
cannot hide inside one and mean something else there.

@verbatim|{
s ::= e                                  an expression, for its value or its effect
    | (define x e)                       bind a value
    | (define (x x* ...) s* ...)         bind a procedure
    | (define (x x* ... . rest) s* ...)  bind a procedure; the rest parameter
    |                                    collects the arguments into a list
    | (set! x e)                         assign
    | (begin s ...)                      a sequence, of statements here
    | (if e1 s1 s2)                      a conditional of statements
    | (cond [e s* ...] ...)              the first clause whose test is true; else is last

e ::= x                                  variable, a Python global
    | l                                  self-evaluating literal
    | 'd                                 quoted datum
    | (import x)                          the module the name or string x names
    | (lambda (x* ...) s* ...)           a procedure, lifted like a define
    | (if e1 e2 e3)                      conditional
    | (cond [e e* ...] ...)              the first clause whose test is true; else is last
    | (begin e1 e* ...)                  a sequence, of expressions here
    | (raise e1)                         raise an exception
    | (with-handler e1 s* ...)           run the body with e1 handling what it raises
    | (trampoline s* ... e)              call what the last body returns while it is a procedure
    | (e0 e* ...)                        application

d ::= int | float | string | boolean | list | tuple | dict
l ::= int | float | string | boolean | tuple | dict
}|

A procedure body is a sequence of statements, and its value is the last
expression in it.  The body of a @racket[with-handler] or a @racket[trampoline]
is a statement sequence too: when it holds @racket[define] or @racket[set!] it
becomes a nested @racket[def] that runs where the form stands, and a lone
expression stays an inline @racket[lambda].

@racket[set!] of a name the program defines at the top level becomes
@racket[global]; @racket[set!] of a name the program never defines is a plain
Python assignment, and the compiler says so.  A procedure that @racket[set!]s a
variable it captures is a compile error: a lifted procedure receives what it
captures as parameters.  A capture that is shadowed where the procedure is used
is a compile error too.

@racket[lambda] takes the same parameter lists as @racket[define] -- a rest
parameter included, and it collects into a list -- and its body is a statement
sequence like a procedure body, with the value of its last expression.  Every
@racket[lambda], and every procedure defined where it stands, is lifted to a
top-level define that takes the variables it captures as leading parameters, so
a procedure value carries what it captured with it.

@racket[cond] runs the body of the first clause whose test is true, and a clause
body is an implicit @racket[begin], so it may hold several forms;
@racket[else] is the last clause, and a @racket[cond] with no @racket[else]
raises when no test is true.

@subsection{Data}

@racket[quote] is data: integers, floats, strings, booleans, lists, tuples and
dicts.  A list is a Python list, a tuple is @racket[#(1 2)] and becomes
@racket[(1, 2)], and a dict is @racket[#hash(("a" . 1))] and becomes
@racket[{"a": 1}], so dict keys are strings.  There is no symbol type, so
quoting a symbol is a compile error, and there is no @racket[eval] and no
@racket[gensym].

@subsection{Names}

@itemlist[
@item{A free identifier is a Python global: @racket[(print (len "abc"))]
becomes @racket[print(len("abc"))].}
@item{Names are munged into readable Python identifiers: @racket[even?] is
@racket[even_p], @racket[set-car!] is @racket[set_car_b], @racket[object-ref]
is @racket[object_ref], and a name that is a Python keyword gets a trailing
@racketid[_].}
@item{Operators are @bold{syntax}, not procedures.  With two or more operands
they are infix: @racket[+], @racket[-], @racket[*], @racket[/],
@racket[quotient], @racket[modulo], @racket[expt], @racket[&], @racket[\|],
@racket[^], @racket[\|], @racket[<<], @racket[>>], @racket[=], @racket[not=], @racket[<],
@racket[>], @racket[<=], @racket[>=] (@racket[equal?] is @racket[=],
@racket[eq?] is @racket[is]), @racket[in], @racket[and] and @racket[or].  With
one operand @racket[not], @racket[-] and @racket[~] are prefix, and
@racket[(+ x)] is @racket[x].  An operator is never a value:
@racket[(apply + xs)] is refused, so define a procedure when the procedure
itself is wanted.}
@item{A name the program never binds is a Python global, which is the point,
but the compiler logs a warning about it: a reference to a name no
@racket[define] or parameter binds, and a @racket[set!] of one.  Python builtins
(@racket[print], @racket[len], ...) and the pieces below are known, so they stay
quiet.  The warnings are logged at warning level, so @exec{PLTSTDERR=warning} is
how you see them.}
@item{Only @racket[#f] is false.  An @racket[if] compiles to
@racket[(then if test is not False else else)], so @racket[0], @racket[""] and
@racket[(quote ())] are true, as they are in Racket.}]

@subsection{Runtime pieces}

A procedure the program names comes with the piece it needs, and nothing else
does.

@(tabular
  (list (list @bold{source} @bold{Python})
        (list @racket[(raise e)] @elem{@racket[_raise] and @racket[_Raised]})
        (list @racket[(with-handler h e ...)] @racket[_with_handler])
        (list @racket[(trampoline s* ... e)] @racket[_trampoline])
        (list @racket[begin] @elem{in an expression: @racket[_begin]})
        (list @racket[(import x)] @racket[import_module])
        (list @racket[(list 1 2)] @racket[list])
        (list @racket[(apply f xs)] @racket[apply])
        (list @racket[(keyword-apply f kw xs)] @racket[keyword_apply])
        (list @racket[(object-ref xs 0)] @racket[object_ref])
        (list @racket[(object-set! xs 0 1)] @racket[object_set_b])
        (list @racket[(object-get-attr xs "append")] @racket[object_get_attr])
        (list @racket[(object-set-attr! xs "a" 1)] @racket[object_set_attr_b])
        (list @racket[(object-has-attr? xs "append")] @racket[object_has_attr_p])))

@subsection{Tail calls}

Tail calls are explicit.  @racket[trampoline] calls what its last body returns,
and keeps calling while that value is a procedure, so a procedure that returns a
thunk for its tail call loops flat and one that returns anything else is done.
The example above runs while this manual is built, and prints:

@verbatim{@(output-of '((define (count n acc)
                          (if (= n 0) acc (lambda () (count (- n 1) (+ acc 1)))))
                        (print (trampoline (count 100000 0)))))}

Nothing is recognised for you: the compiler does not look for a tail call and
does not wrap one, so the thunk is yours to write.

@codeblock|{
(define python
  (#%python-code
    (define (count n acc)
      (if (= n 0) acc (lambda () (count (- n 1) (+ acc 1)))))
    (print (trampoline (count 100000 0)))))

(displayln python)     ; the Python source; running it prints 100000
}|

@subsection{Macros}

A macro is written outside the program, in Racket, with @racket[defmacro], and
used inside @racket[#%python-code]:

@defform[(defmacro (name params ...) body ...)]{Binds @racket[name] to a
transformer for the LE inside @racket[#%python-code]: the body runs while the
program is compiled, on the argument forms as data, and the form it returns
takes their place.  @racket[name] is a syntax binding, so it is visible after
this form in its own module, or in a module that requires the one that wrote
it -- and a macro has to be defined before the @racket[#%python-code] form that
uses it.  There is no hygiene: the expansion is re-syntaxed where it is used,
so @racket[gensym] a name of the macro's own.  @racket[(defmacro name
transformer)] binds @racket[name] to a transformer expression.}

@codeblock|{
(defmacro (twice form) `(begin ,form ,form))

(define python
  (#%python-code
    (twice (print "hi"))))
}|

The Python that renders, produced by running @racket[#%python-code] as this
manual is built:

@verbatim{@(python-of '((twice (print "hi"))))}

@defform[(defmacro #:space key (name params ...) body ...)]{Binds the macro in
the space @racket[key], under the name @racket[key]@tt{.}@racket[name] --
@racket[(defmacro #:space a (twice x) ...)] binds @tt{a.twice} -- so two macros
may share a name.  @racket[key] is a datum, and it is part of the binding's
name, so a module that provides @tt{a.twice} is how another module gets that
macro.  @racket[(defmacro #:space key name transformer)] binds it to a
transformer expression.}

@codeblock|{
(defmacro #:space math (twice form) `(print (* 2 ,form)))
(defmacro #:space text (twice form) `(print ,form ,form))

(define python
  (#%python-code
    (#:space math (twice 21))    ; the macro in space math
    (#:space text (twice "hi"))  ; the macro in space text
    (twice "plain")))            ; no space: the plain name, as ever
}|

A use is @racket[(#:space key name arg ...)]: the macro call
@racket[(name arg ...)] is expanded in that space, and its subforms with it,
whatever space the body around it is in.  An inner @racket[#:space] overrides
the one around it for its own form and subforms, which is why a macro call can
hand a form in another space to another macro.  The key is not one of the
macro's arguments.  @racket[#%python-code] takes the keyword too,
@racket[(#%python-code #:space key form ...)], to put a whole body in a space,
and @racket[(#:space #f name arg ...)] is the plain name inside a spaced body.
A name is looked up in the space around it, so a plain name that no macro in
that space matches is an ordinary Python call, as it always was; a
@racket[#:space] form that names no macro is a compile error.

A macro may expand into anything LE has, @racket[cond] included, and into
another macro.  A name the expansion writes is the program's name too, so a
macro that needs one of its own asks @racket[gensym] for it:

@codeblock|{
(defmacro (bind-tmp value body)
  (define name (gensym 'tmp))
  `(begin (define ,name ,value) ,body))
}|

A macro's name shadows an LE name of the same name: inside
@racket[#%python-code], @racket[(name arg ...)] is the macro wherever
@racket[name] is bound as one.  A macro of your own is not the only way to grow
sugar: a Racket macro can expand to @racket[#%python-code], or build the LE
forms as data and hand them to it.

@codeblock|{
(define-syntax-rule (python-twice e) (#%python-code (print e) (print e)))
(displayln (python-twice (quote (1 2))))
;; the source it renders is print([1, 2]) twice, so running it prints two lines
}|

@section{Tests}

@codeblock|{
$ PLTCOLLECTS="$PWD/..:" TMPDIR="$PWD/.tmp" raco test tests/
}|

The tests use the macro directly, look at the Python it renders, and run it with
@exec{python3}.  @envvar{PLTCOLLECTS} is only needed when an older copy of the
package is installed; with the package installed,
@exec{raco test -x -p rkt-pythonize} is enough.

@include-section["changelog.scrbl"]
