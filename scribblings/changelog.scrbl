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
