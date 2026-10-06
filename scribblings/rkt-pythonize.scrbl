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
@; the examples below run while this manual is built: the prelude's prefix is
@; pinned to nothing so the Python they show reads the way the manual says
@(parameterize ([current-namespace example-namespace])
   (eval '(begin-for-syntax (prelude-prefix ""))))

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
    | (define (x p* ...) s* ...)         bind a procedure
    | (define (x p* ... . rest) s* ...)  bind a procedure; the rest parameter
    |                                    collects the arguments into a list
    | (set! x e)                         assign
    | (begin s ...)                      a sequence, of statements here
    | (if e1 s1 s2)                      a conditional of statements
    | (cond [e s* ...] ...)              the first clause whose test is true; else is last

e ::= x                                  variable, a Python global
    | l                                  self-evaluating literal
    | 'd                                 quoted datum
    | (import x)                          the module the name or string x names
    | (lambda (p* ...) s* ...)           a procedure, lifted like a define
    | (if e1 e2 e3)                      conditional
    | (cond [e e* ...] ...)              the first clause whose test is true; else is last
    | (begin e1 e* ...)                  a sequence, of expressions here
    | (raise e1)                         raise an exception
    | (with-handler e1 s* ...)           run the body with e1 handling what it raises
    | (trampoline s* ... e)              call what the last body returns while it is a procedure
    | (e0 a* ...)                        application, with keyword arguments

p ::= x | #:k x                          a parameter: positional, or the parameter
                                         of the keyword k
a ::= e | #:k e                          an argument: positional, or the keyword k

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
@racket[gensym]: the one way a program makes a name of its own is
@racket[gensym] in a macro, and a macro is the one place to ask for it.  A
keyword is not data either: @racket[#:k] is the syntax of a
keyword argument, so @racket[(quote #:k)] and @racket[#(1 #:k)] are compile
errors too.

@subsection{Names}

@itemlist[
@item{A free identifier is a Python global: @racket[(print (len "abc"))]
becomes @racket[print(len("abc"))].}
@item{@filepath{core/names.rkt} owns what a name is in Python:
@racket[python-name] is the conversion, @racket[python-keyword-name] is a
keyword argument's name and @racket[piece-name] is what a piece is called, and
the conversion reads the name and @racket[python-name-style] and nothing else,
so the Python name of a top-level define is knowable from the source alone.
The rules are below.}
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

A @racket[-] ends a word of a name, @racket[?] and @racket[!] are words of their
own -- the predicate and the bang -- and a name that is a Python keyword takes a
trailing @tt{_} while one that starts with a digit takes a leading one:

@verbatim|{
name                'snake, the default   'camel
even?               even_p                 isEven
a?b                 a_pb                   aPb
set-car!            set_car_b              setCarB
object-ref          object_ref             objectRef
a--b                a__b                   a_B
a-                  a_                     a_
class               class_                 class_    a Python keyword
1st                 _1st                   _1st      a name cannot start with a digit
}|

With @racket['snake] the words are joined with @tt{_}; with
@racket['camel] the letter after a @racket[-] is capitalized instead, and a
@racket[-] that has no letter after it is joined, so no name is lost.

With @racket['camel] a name that ends with @racket[?] is the predicate it says
it is: the @racket[?] comes off, the first letter is capitalized, and @tt{is}
goes in front, so @tt{even?} is @tt{isEven} and @tt{my-func?} is @tt{isMyFunc}.
A @racket[?] that is not the end of a name is @tt{P} -- @tt{a?b} is @tt{aPb} --
where @racket['snake] has @tt{_p} in both places.

A name that already spells its Python name -- one with an @tt{_} in it --
is left as it is, so a Python keyword argument that is not one word can be
written exactly: @racket[#:format_spec] is @racket[format_spec] in either style.

@racket[python-name-style] is the parameter that chooses, and it is set where
the compiler runs, before the forms it is meant for:

@codeblock|{
(begin-for-syntax (python-name-style 'camel))
}|

@racket[python-name] is provided at that phase too, so
@racket[(begin-for-syntax (python-name 'object-ref))] is the name a form would
use; it takes a prefix and a suffix as well, which are Python text put around
the conversion and not converted themselves.

A keyword argument's name is converted as a name is -- @racket[#:foo-bar] is
@racket[foo_bar] in one style and @racket[fooBar] in the other -- and the
keyword a Python function takes is converted the same way, whether or not the
program defines the procedure it calls: @racket[(sorted xs #:reverse #t)] is
@tt{sorted(xs, reverse=True)}.

A name the compiler makes up for itself is @tt{_lift_3f9a1b2c}, or
@tt{_lift_3f9a1b2c_inner} for a procedure the program called @racket[inner], and
it is a UUID the program did not write down, so a name the program defines at
the top level is its own in the Python.  The prelude's own names carry
@racket[prelude-prefix] -- an identifier's worth of a UUID, which the compiler
asks for once a run -- so a name a program writes is never one of the
prelude's: a program may define
@racket[list] and get its own @racket[list], while the piece the compiler calls
is out of its reach under the prefix.  Pin the prefix to have the same Python
every time; this manual pins it to nothing, so the pieces below read as they are
written.  Two top-level names that are one Python name --
@racket[x-y] and @racket[x_y] -- are a compile error, so a name of the program
is one name in the module.  What is compared is the Python name in both
directions: a program that defines @racket[x-y] and writes @racket[x_y]
anywhere -- a call, a reference, a @racket[set!] -- is writing the name it
defined, and a parameter list that spells one Python name twice is refused as
well.

@subsection{Keyword arguments}

A parameter list takes keyword parameters, written @racket[#:k k], and a call
hands a procedure keyword arguments the same way:

@codeblock|{
(define (area w #:height height) (* w height))
(print (area 3 #:height 4))
}|

The Python that renders, produced while this manual is built:

@verbatim{@(python-of '((define (area w #:height height) (* w height))
                        (print (area 3 #:height 4))))}

and what running it prints:

@verbatim{@(output-of '((define (area w #:height height) (* w height))
                        (print (area 3 #:height 4))))}

The star is what makes @tt{height} keyword-only in Python, and it stands where
the rest parameter would: @tt{(define (f a #:k k . rest) ...)} is
@tt{def f(a, *rest, k)}, and @racket[lambda] takes keyword parameters too.  A
keyword parameter is keyword-only where the procedure is called, as it is in
Racket, and the parameter list of a procedure that captures is lifted with its
keywords, so a procedure value passes them on.

A keyword's name is a name, converted as a name is: @racket[#:foo-bar] is
@tt{foo_bar}, @racket[#:even?] is @tt{even_p}.  In Python a keyword and the
parameter it fills are one name, so a parameter list spells the keyword twice,
@racket[#:k k], and @racket[#:k v] is a compile error.

The same conversion names the keyword a Python function takes, whether or not
the program defines the procedure: a call reads as a Scheme name and renders as
the Python one.

@codeblock|{
(print (sorted (list 3 1 2) #:reverse #t))
(print (round 2.567 #:ndigits 2))
}|

@verbatim{@(python-of '((print (sorted (list 3 1 2) #:reverse #t))
                        (print (round 2.567 #:ndigits 2))))}

@verbatim{@(output-of '((print (sorted (list 3 1 2) #:reverse #t))
                        (print (round 2.567 #:ndigits 2))))}

A call writes its keyword arguments where it likes, Racket's way; the Python it
renders puts the positional arguments first and the keyword ones after, which is
also the order Python evaluates them in.

@verbatim{@(python-of '((define (tag x y #:label label) (list label x y))
                        (print (tag 1 #:label "a" 2))))}

A keyword is syntax, not a value: it names an argument where a call writes one
and a parameter where a parameter list takes one, and it is never a datum, so
@racket[(quote #:k)] is refused.  One keyword takes one value, and two
arguments that are one Python name after the conversion -- @racket[#:x-y] and
@racket[#:x_y] -- are the one keyword, which is a compile error rather than the
@tt{SyntaxError} Python would raise.

@racket[apply] carries a keyword argument to the function it calls, which is the
function the keyword is for: @racket[(apply f xs #:k v)] is @tt{f(*xs, k=v)}.

@subsection{Runtime pieces}

A procedure the program names comes with the piece it needs, and nothing else
does.

@(tabular
  (list (list @bold{source} @bold{Python})
        (list @racket[(raise e)] @elem{@tt{raise_} and @tt{Raised}})
        (list @racket[(with-handler h e ...)] @tt{with_handler})
        (list @racket[(trampoline s* ... e)] @tt{trampoline})
        (list @racket[begin] @elem{in an expression: @tt{begin}})
        (list @racket[(import x)] @tt{import_module})
        (list @racket[(list 1 2)] @tt{list})
        (list @racket[(apply f xs)] @tt{apply})
        (list @racket[(keyword-apply f kw xs)] @tt{keyword_apply})
        (list @racket[(object-ref xs 0)] @tt{object_ref})
        (list @racket[(object-set! xs 0 1)] @tt{object_set_b})
        (list @racket[(object-get-attr xs "append")] @tt{object_get_attr})
        (list @racket[(object-set-attr! xs "a" 1)] @tt{object_set_attr_b})
        (list @racket[(object-has-attr? xs "append")] @tt{object_has_attr_p})))


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
transformer)] binds @racket[name] to a transformer expression.

@racket[params] is a Racket parameter list, so a macro takes keyword arguments
too: @racket[(defmacro (m x #:k k) ...)] is called @racket[(m 1 #:k 2)], and a
keyword of a macro may carry a default, @racket[#:k [k 5]], which a procedure's
may not.  A transformer is a Racket procedure, so a macro's keywords are
Racket's: they are not converted, two of them are two keywords over there
whatever they would be in Python, and a call that names one the transformer does
not take, or leaves out one it needs, is a compile error.}

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
