#lang scribble/manual

@require[@for-label[rkt-pythonize
                    racket/base]
         (only-in racket/pretty pretty-format)
         nanopass/base
         rkt-pythonize]

@title{rkt-pythonize}
@author{zhanghao}

@defmodule[rkt-pythonize]

@bold{rkt-pythonize} transpiles Lisp to Python.  The source language is
@bold{LB}, whose syntax is Racket's s-expression syntax: @racket[read] is the
whole front end, so there is no lexer, reader, or macro expander to maintain.
The output is a self-contained Python program -- it imports nothing and needs no
runtime library.

@table-of-contents[]

@section{Supported Features}

@itemlist[
@item{@racket[define] of values and of procedures, at the top level and inside
      bodies}
@item{Explicit tail calls: @racket[trampoline]}
@item{@racket[set!], including of a local of an enclosing procedure (Python
      @tt{nonlocal})}
@item{@racket[raise] and @racket[with-handler]}
@item{Self-evaluating literals, and quoted data with interned symbols}
@item{Free variables as Python globals, so the Python world stays reachable}
@item{Readable output: one Python function per LB procedure, no CPS conversion,
      and only the prelude pieces a program actually uses}
]

@section{Syntax}

The grammar of LB, straight from @racket[define-language]:

@codeblock[#:keep-lang-line? #f]{
#:lang nanopass
@(pretty-format #:mode 'write (language->s-expression LB))
}

Each form is compiled to the Python that reads best for it:

@itemlist[
@item{@racket[(define x e)] binds a value: @tt{x = e}}
@item{@racket[(define (f x* ...) e)] binds a procedure: @tt{def f(x* ...): ...}}
@item{@racket[(trampoline e ...)] marks the tail calls of @racket[e] as bounces
      of a trampoline; it is never added for you}
@item{@racket[(set! x e)] assigns, and becomes a Python @tt{nonlocal} when
      @racket[x] belongs to an enclosing procedure}
@item{@racket[(raise e)] raises @racket[e], which may be any LB value}
@item{@racket[(with-handler h e)] runs @racket[e] with @racket[h] as the handler
      of @racket[raise]; @racket[h] is called with the raised value}
@item{@racket[(begin e ...)] is a sequence, or a Python expression when it
      appears in a value position}
@item{@racket[(if e1 e2 e3)] is a conditional}
@item{@racket[(e0 e* ...)] calls @racket[e0]}
@item{A quoted datum @racket['d] becomes a Python value: a symbol becomes an
      interned @tt{Symbol}, a list a Python list, a tuple a tuple, a hash table a
      dict}
]

@section{Usage}

From the command line, where @tt{prog.lb} becomes @tt{prog.py}:

@commandline{raco rkt-pythonize [-o <output>] [<file>|-]}

Options come before the input file.  With no file, or with @tt{-}, the LB
program is read from stdin and the Python program goes to stdout, so the
transpiler composes:

@commandline{echo '(print (+ 1 2))' | raco rkt-pythonize}

From Racket, @racket[transpile] is the whole pipeline -- read, parse, compile:

@racketblock[
(require rkt-pythonize)
(transpile "(print (+ 1 2))")
]

@section{Examples}

A procedure, a conditional, and a call:

@racketblock[
(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))
(print (fact 5))
]

@(verbatim (transpile "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))\n(print (fact 5))\n"))

A tail call bounces only where the source says @racket[trampoline].  Such a
procedure is emitted twice -- the public name drives the trampoline and a
@tt{_body} name holds the bounces -- so a deep loop stays flat however the
procedure is called:

@racketblock[
(define (count n acc)
  (trampoline (if (= n 0) acc (count (- n 1) (+ acc 1)))))
(print (count 100000 0))
]

@(verbatim (transpile "(define (count n acc)\n  (trampoline (if (= n 0) acc (count (- n 1) (+ acc 1)))))\n(print (count 100000 0))\n"))

An argument is not a tail position, so @racket[count] is a plain call there:

@racketblock[
(define (add-one n) (+ 1 (count n 0)))
]

@(verbatim (transpile "(define (count n acc)\n  (trampoline (if (= n 0) acc (count (- n 1) (+ acc 1)))))\n(define (add-one n) (+ 1 (count n 0)))\n"))

A procedure whose bounces assign a local of an enclosing procedure gets a
@tt{nonlocal} declaration, worked out from the source:

@racketblock[
(define (make-counter)
  (begin
    (define n 0)
    (define (tick) (trampoline (begin (set! n (+ n 1)) n)))
    tick))
]

@(verbatim (transpile "(define (make-counter)\n  (begin\n    (define n 0)\n    (define (tick) (trampoline (begin (set! n (+ n 1)) n)))\n    tick))\n"))

@racket[raise] carries any LB value and the handler is a procedure that receives
it; @racket[with-handler] also catches Python exceptions, handing the exception
object to the handler:

@racketblock[
(define (boom) (raise 41))
(define (recover e) (+ e 1))
(print (with-handler recover (boom)))
]

@(verbatim (transpile "(define (boom) (raise 41))\n(define (recover e) (+ e 1))\n(print (with-handler recover (boom)))\n"))

A free variable is a Python global, so a program can reach the Python world
without any declaration:

@(verbatim (transpile "(print (len \"abc\"))\n"))

@section{Functions}

@defthing[#:kind "language" LB any/c]{
 The LB language: the forms above and the @racket[parse-LB] parser for them.
}

@defproc[(parse-LB [datum any/c]) any]{
 Parse one datum as an LB program.  A source file with several top-level forms
 is the single program @racket[(begin form ...)].
}

@defproc[(unparse-LB [program any]) any/c]{
 The inverse of @racket[parse-LB]: an LB program as a Racket datum.
}

@defproc[(compile-LB [program any]) string?]{
 Compile a parsed LB program to Python source text.
}

@defproc[(transpile [source string?]) string?]{
 Read, parse, and compile LB source text.  This is the whole transpiler.
}

@defproc[(python-name [sym symbol?]) string?]{
 The Python identifier an LB name compiles to: @racket[even?] becomes
 @tt{even_p}, @racket[set-car!] becomes @tt{set_car_b}, and a Python keyword gets
 a trailing underscore.
}

@defproc[(run-cli [args (listof string?)]) exact-integer?]{
 Run the command line described under @seclink["Usage"], and return the exit
 code.  The @tt{main} submodule of @tt{main.rkt} calls this with the command
 line arguments, which is what @tt{raco rkt-pythonize} runs.
}

@section{Changelog}

@itemlist[
@item{0.0.1 -- the rewrite: LB, defined with @seclink["Syntax"]{nanopass}, and a
      Python backend; a command line entry point.}
]
