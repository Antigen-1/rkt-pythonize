#lang scribble/manual

@; CHANGELOG.md is generated from this file: run ./build-docs.sh after editing.
@; The changelog is also the last chapter of the manual: rkt-pythonize.scrbl
@; pulls this file in with @include-section.

@require[@for-label[rkt-pythonize
                    racket/base]]

@title[#:style 'unnumbered]{Changelog}

Newest first.  Every version here is a state someone could have, so the small
ones are listed too.  Everything before 2.0.0 was a different design -- a
Lisp-to-Python transpiler (LB, then LE, then LM) driven by a nanopass pipeline,
with a runtime macro system of its own; the last of those was 1.3.3.

@section[#:style 'unnumbered]{3.12.6}

@itemlist[
@item{What interrupts @racket[->] and @racket[->>] is an exception, not
@racket[None]: a step whose value is an instance of @racket[Exception] or of a
subclass of it ends the chain there, the steps after it do not run, and the
chain's value is that instance.  @racket[None] is a value like any other, so a
step may hand it on, and a chain that runs to its end is told from one that was
interrupted by what comes back: the instance itself, or the value of the last
step.}]

@section[#:style 'unnumbered]{3.11.6}

@itemlist[
@item{A @racket[begin] where a value belongs is lifted into a procedure of its
own and called where it stands, so the Python is a @tt{def} of statements rather
than one call whose arguments are the program: @racket[(if (begin (print "t") #t)
1 2)] is a @tt{def} that prints and returns, and a call to it.  A begin that
stands as a statement, and the begin a body's forms make, are statements as
before.  The runtime has no @tt{begin} piece any more.}
@item{That a definition cannot stand in a begin where a value belongs is
@racket[begin]'s semantics, not a limit of the compiler -- a begin sequences
expressions, and a definition belongs in a body, where a name is defined -- and
the refusal now says so.}
@item{@filepath{syntax/} is the high-level syntax, which runs after the macros
and before the checks: @filepath{syntax/cond.rkt}, the @racket[cond] expansion
that was @filepath{core/expand-cond.rkt}, and @filepath{syntax/thread.rkt},
which is new.}
@item{@racket[(-> x step ...)] threads into the first argument of each step and
@racket[(->> x step ...)] into the last positional one.  @racket[None]
interrupts: a step whose value is None ends the chain there and the chain's value
is None, and the steps after it do not run.  The chain is run by
@racket[trampoline] -- each step answers the call that makes the next one -- so
it is flat however many steps it has, and the value of the last step comes back
in a one-element list the driver unwraps, so a value that is callable is not
mistaken for the next step.}
@item{@tt{None}, @tt{True} and @tt{False} are the Python constants a program
reads, and not names a Python keyword takes a trailing @tt{_} from:
@racket[None] is what the threading operators interrupt on.  Binding one is a
compile error.}]

@section[#:style 'unnumbered]{3.10.6}

@itemlist[
@item{The camel predicate rule is written down, and the conversion tables agree
with the compiler: with @racket['camel] a name that ends with @racket[?] is the
predicate it says it is -- the @racket[?] comes off, the first letter is
capitalized, and @tt{is} goes in front, so @racket[even?] is @tt{isEven} and
@racket[my-func?] is @tt{isMyFunc} -- while a @racket[?] that is not the end of
a name is @tt{P}, as in @tt{aPb} where @racket['snake] has @tt{a_pb}.  The
tables and the 3.10.5 note below said @tt{evenP}, which the compiler has not
done since the rule changed in 3.10.5; nothing in the compiler changed here.}]

@section[#:style 'unnumbered]{3.10.5}

@itemlist[
@item{Keyword arguments, Racket's way, on both sides of a call: a parameter list
takes @racket[#:k k] -- @racket[(define (f a #:k k) ...)], and
@racket[lambda] with it -- and a call writes @racket[(f 1 #:k 2)].  A keyword
parameter is keyword-only in the Python, @tt{def f(a, *, k)}, and the star
stands where the rest parameter would, @tt{def f(a, *rest, k)}, so it is
keyword-only where the procedure is called, as it is in Racket.}
@item{A call writes its keyword arguments where it likes: @racket[(f 1 #:k 2 3)]
is @tt{f(1, 3, k=2)}.  The Python puts the positional arguments first and the
keyword ones after, which is the order Python evaluates them in.}
@item{@tt{core/names.rkt} is the one module that says what a name is in Python.
@racket[python-name] is the conversion, @racket[piece-name] is what a piece of
the prelude is called, and nothing else spells a Python name out -- and the
conversion reads the name and @racket[python-name-style] and nothing else:
@racket[(python-name 'even?)] is @tt{even_p}, @racket[(python-name 'set-car!)]
is @tt{set_car_b}, a name that is a Python keyword takes a trailing @tt{_} and
one that starts with a digit takes a leading one.}
@item{@racket[python-name-style] is a parameter, @racket['snake] by default and
@racket['camel] otherwise, so a module says how its names are spelled:
@racket[(begin-for-syntax (python-name-style 'camel))] before its
@racket[#%python-code] forms compiles @racket[even?] to @tt{isEven} and
@racket[object-ref] to @tt{objectRef}.  With @racket['camel] the letter after a
@racket[-] is capitalized, a @racket[-] with no letter after it is joined
instead, so no name is lost, and a name that ends with @racket[?] is the
predicate it says it is: the @racket[?] comes off, the first letter is
capitalized, and @tt{is} goes in front.  @racket[python-name] takes a prefix and a suffix
too, Python text around the conversion, which is how the compiler names what it
lifts.  @racket[python-name], @racket[python-name-style],
@racket[runtime-names] and @racket[python-builtins] are provided at the phase
the compiler runs in, where they were only in @tt{core/names.rkt} before.}
@item{Every name of a program is @racket[python-name] of the name the source
wrote and nothing else, so the Python name of a top-level define is knowable
from the source alone -- which is what a program that exports its names reads.
A name the compiler makes up for itself -- @tt{_lift_3f9a1b2c} for a lifted
procedure, @tt{_lift_3f9a1b2c_inner} for one the program called @tt{inner} -- is
made of a UUID, so it is not one the program writes and a lifted procedure
cannot take a program's name.  What a check compares is the
Python name, in both directions: two top-level names that are one Python name
(@racket[x-y] and @racket[x_y]) are a compile error rather than one silently
overwriting the other, a parameter list that spells one Python name twice is
refused, a call that gives one keyword two values under two spellings of it is
refused rather than left to a Python @tt{SyntaxError}, and a name written as it
is in Python is the name the program defined -- @racket[(define x-y 1)] with
@racket[(set! x_y 2)] somewhere is that one variable, which the scope check
knows and does not warn about.}
@item{A keyword's name is converted as a name is -- @racket[#:foo-bar] is
@tt{foo_bar} in one style and @tt{fooBar} in the other -- and a call converts
the keyword it writes whether or not the program defines the procedure it calls,
so @racket[(sorted xs #:reverse #t)] is @tt{sorted(xs, reverse=True)}.  A piece
that answers to an LE name is named by the same conversion, so
@racket[(object-ref xs 0)] and its @tt{def} agree in either style.  In Python a
keyword and the parameter it fills are one name, so a parameter list spells it
twice, @racket[#:k k]; @racket[(define (f #:k v) v)] is a compile error.}
@item{The prelude's names carry @racket[prelude-prefix], an identifier's worth
of a UUID the compiler asks for once a run (@tt{uuid}, a new dependency), so no
name a program writes is one of the prelude's: a
program may define @racket[list], @racket[apply] or @tt{_raise}, and its own is
what the name means where it is bound, while the pieces the compiler calls
itself -- @racket[(raise e)], @racket[(with-handler ...)], the rest parameter of
a lifted procedure that captures -- are the prefixed ones and out of the
program's reach.  Pin the prefix to have the same Python every time, or to
nothing to read the prelude as it is written; the manual's examples pin it to
nothing.}
@item{A keyword is syntax, not data: it belongs in a call or in a parameter
list, and nowhere else.  A keyword in an expression position or a statement
position, a @racket[set!] of one, and a keyword inside quoted data or inside a
literal (@racket[(quote #:k)], @racket[#(1 #:k)]) are compile errors now, the
last two where the form is, rather than at render time.}
@item{@racket[defmacro] takes keyword arguments too: a transformer is a Racket
procedure, so @racket[(defmacro (m x #:k k) ...)] is called
@racket[(m 1 #:k 2)], a macro's keyword is not converted -- two of them are two
keywords there whatever they would be in Python -- and one may carry a default,
@racket[#:k [k 5]], which a procedure's may not.  A call that names a keyword
the transformer does not take, or leaves out one it needs, is a compile error
that says which.}
@item{@tt{core/params.rkt}: the parameter list @racket[define] and
@racket[lambda] share and the arguments a call hands a procedure.  A parameter
that is not a name, a keyword with no parameter and an argument that writes one
keyword twice are compile errors now, all of them Python's own rules.
@tt{core/lift.rkt} carries keyword parameters through a lift and the reference
it makes, and @tt{core/render.rkt} renders a signature keyword-only.}
@item{@racket[apply] carries a keyword argument to the function it calls, since
that is the function the keyword is for: @racket[(apply f xs #:k v)] is
@tt{f(*xs, k=v)}, and the piece takes @tt{**keywords}.  A lifted procedure that
captures, takes a rest parameter and takes keywords passes all three on, which
is what that piece is for.}
@item{@tt{tests/names.rkt} pins the conversion character by character in both
styles, and @tt{tests/camel.rkt} is a module that set the style and renders its
own forms.}]

@section[#:style 'unnumbered]{3.9.5}

@itemlist[
@item{@racket[defmacro]: a macro written outside @racket[#%python-code], in
Racket, whose body runs while the program is compiled on the argument forms as
data and returns the form that takes their place.  Each macro is a syntax
binding, so it is visible after its definition in its own module or where that
module is required, and nowhere else; there is no hygiene, so a macro asks
@racket[gensym] for a name of its own.}
@item{@racket[#:space] for @racket[defmacro]: two macros may share a name if they
live in different spaces.  The space is part of the binding's name
(@racket[(defmacro #:space a (twice x) ...)] binds @tt{a.twice}, which is what a
@racket[provide] and a @racket[require] carry), and @racket[(#:space a (twice
x))] expands that macro call and its subforms in space @tt{a}; an inner
@racket[#:space] overrides an outer one, @racket[(#:space #f ...)] is the plain
name, and @racket[#%python-code] takes the keyword too, for a whole body.  The
key is not one of the macro's arguments, and a @racket[#:space] form that names
no macro is a compile error.}
@item{@racket[(+ x)] is @racket[x], which the README and the manual have said
since 3.6.4 and @tt{core/names.rkt}'s tables did not do: @racket[+] is in the
prefix table now, and is still infix with two or more operands.}
@item{@tt{core/expand-macro.rkt}, the pass that expands them.  It runs first, so
a macro may expand into anything LE has, @racket[cond] included, and into
another macro; a macro that expands into itself stops after a bounded number of
expansions with an error, and so do a transformer given the wrong number of
arguments, one that raises, and one that returns something that is not a form.
Seven passes.}]

@section[#:style 'unnumbered]{3.8.5}

@itemlist[
@item{@racket[cond]: the body of the first clause whose test is true, with the
clauses after it in the else position.  A clause body is an implicit
@racket[begin], so it may hold several forms; @racket[else] is a keyword, and
legal only as the last clause's test; a @racket[cond] that runs out of clauses
raises when no test is true.}
@item{@tt{core/expand-cond.rkt}, the pass that expands it into nested
@racket[if]s and @racket[begin]s.  It runs first, so the checks after it only
see @racket[if] and @racket[begin], and the pipeline is six passes.}]

@section[#:style 'unnumbered]{3.7.5}

@itemlist[
@item{The README's two tables are hand-aligned boxed blocks: the Markdown
backend has no table, and the flattened one it made of them ran cells together
where a cell filled its column.}
@item{The README's layout listing names @tt{scribblings/readme.scrbl} and
@tt{scribblings/changelog.scrbl}, the manual's first and last chapters, and
@tt{./build-docs.sh}, which renders them into @tt{README.md} and
@tt{CHANGELOG.md}.}]

@section[#:style 'unnumbered]{3.7.4}

@itemlist[
@item{The prelude carries only the pieces a program asked for, and a piece
brings the pieces it needs: @racket[with-handler] catches @tt{_Raised}, so a
program that uses it carries @tt{raise} even when it never raises itself.  A
program with no @racket[import] has no @tt{importlib}, and one with no
@racket[trampoline] has no @tt{_trampoline}.}
@item{@tt{README.md} and this changelog are generated from
@tt{scribblings/readme.scrbl} and @tt{scribblings/changelog.scrbl} by
@tt{./build-docs.sh}; edit the sources.}
@item{The manual's examples are compiled and run with @tt{python3} while the
manual is built, so an example that stops working fails the build.}]

@section[#:style 'unnumbered]{3.6.4}

@itemlist[
@item{Operators are syntax, and the set is the one Python's builtins support:
@tt{+ - * / quotient modulo expt}, @tt{& | ^ << >>},
@tt{= not= < > <= >=} (@racket[equal?] is @tt{=}, @racket[eq?] is @tt{is}),
@racket[in], @racket[and], @racket[or] infix with two or more operands, and
@racket[not], @tt{-}, @tt{~} prefix with one (@racket[(+ x)] is @racket[x]).
An operator is never a value: @racket[(apply + xs)] and @racket[(print +)] are
refused, and one is checked in @tt{check-expression} instead of only at render
time.}]

@section[#:style 'unnumbered]{3.5.4}

@itemlist[
@item{@tt{CHANGELOG.md}, this file, with a pointer to it from the README and the
manual.}]

@section[#:style 'unnumbered]{3.5.3}

@itemlist[
@item{@racket[(import x)] is an expression: it is the module
@tt{importlib.import_module} returns, so a module is a value like any other.
@racket[x] is a name (which names its own module), a string, or an expression
the runtime evaluates, and the call carries the @tt{import_module} piece.}
@item{The manual had the pass list twice and the README repeated the pipeline in
the design notes; one copy of each is left.}]

@section[#:style 'unnumbered]{3.5.2}

@itemlist[
@item{@tt{core/names.rkt}: the tables the passes share -- @tt{munged}, the
operators, the pieces and their order.}
@item{@racket[runtime-names] and @racket[python-builtins] are parameters there,
so a program can widen what the scope check treats as known from a
@racket[begin-for-syntax].}
@item{README and manual show the five-pass pipeline.}]

@section[#:style 'unnumbered]{3.4.2}

@itemlist[
@item{@tt{core/check-expression.rkt} and @tt{core/check-scope.rkt}: the two
checks are passes of their own, and @tt{core/render.rkt} only renders.}
@item{Five passes: two checks (@tt{check-expression}, @tt{check-scope}) and
three rewrites (@tt{explicit}, @tt{lift}, @tt{render}).}]

@section[#:style 'unnumbered]{3.3.2}

@itemlist[
@item{Three passes, one file each: @tt{core/explicit.rkt} (make-explicit),
@tt{core/lift.rkt} (lift) and @tt{core/render.rkt} (render, formerly
@tt{core/compile.rkt}).}]

@section[#:style 'unnumbered]{3.3.1}

@itemlist[
@item{A lift pass: @tt{core/lift.rkt} turns LE into LL, lifting every
@racket[lambda] and every locally defined procedure to a top-level define that
takes the variables it captures from enclosing scopes as leading parameters.}
@item{@racket[lambda] takes the same parameter lists as @racket[define] -- a
rest parameter included, and it collects into a list -- and its body is a
statement sequence.}
@item{@racket[set!] of a captured variable is a compile error, and so is a
capture that is shadowed where the procedure is used.}]

@section[#:style 'unnumbered]{3.2.1}

@itemlist[
@item{No tail-call recognition: the last body of @racket[trampoline] is the
function it calls, and the compiler neither looks for a tail call nor wraps
one.}]

@section[#:style 'unnumbered]{3.1.1}

@itemlist[
@item{The trampoline example in the docs shows the Python it renders instead of
a number it never printed.}]

@section[#:style 'unnumbered]{3.1.0}

@itemlist[
@item{@racket[lambda], whose body was a single expression at the time.}
@item{A lexical scope check: a name the program never binds is logged at warning
level, for a reference and for a @racket[set!] alike.}]

@section[#:style 'unnumbered]{3.0.0}

@itemlist[
@item{rkt-pythonize is a library with one export, the macro
@racket[#%python-code], whose body is LE and whose value is the Python rendered
at expansion time.}
@item{No @tt{#lang}, no reader, no command line, and no Racket form is
replaced.}]

@section[#:style 'unnumbered]{2.0.0}

@itemlist[
@item{@tt{#lang rkt-pythonize}: a module is a Python program, compiled at
expansion time, and exports @tt{python-code}; the executable only writes it
out.}
@item{The data-based pipeline and the runtime macro machinery are gone, and with
them @tt{Symbol}, @racket[eval], @racket[gensym] and @tt{defmacro}.}]
