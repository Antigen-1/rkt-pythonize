#lang scribble/manual

@require[@for-label[rkt-pythonize
                    racket/base]
         (only-in racket/pretty pretty-format)
         (only-in racket/port with-input-from-string)
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
@item{Macros: @racket[defmacro] over forms, in LM (see @seclink["Macros"])}
@item{Python modules with @racket[import], and Python objects with
      @racket[object-ref] and friends (see @seclink["Macros"])}
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
@item{@racket[(define (f x* ... . rest) e)] binds a procedure whose rest
      parameter collects the remaining arguments into a list:
      @tt{def f(x* ..., *rest):}}
@item{@racket[(trampoline e ...)] marks the tail calls of @racket[e] as bounces
      of a trampoline; it is never added for you}
@item{@racket[(set! x e)] assigns, and becomes a Python @tt{nonlocal} when
      @racket[x] belongs to an enclosing procedure}
@item{@racket[(raise e)] raises @racket[e], which may be any LB value}
@item{@racket[(with-handler h e)] runs @racket[e] with @racket[h] as the handler
      of @racket[raise]; @racket[h] is called with the raised value}
@item{@racket[(begin e ...)] is a sequence, or a Python expression when it
      appears in a value position}
@item{@racket[(if e1 e2 e3)] is a conditional.  LB's truth is Lisp's: only
      @racket[#f] is false, so @racket[0], @racket[0.0], @racket[""],
      @racket['()] and Python's @tt{None} are all true.  (@racket[and],
      @racket[or] and @racket[not] are the Python operators, and keep Python's
      truth.)}
@item{@racket[(e0 e* ...)] calls @racket[e0]}
@item{@racket[(import spec* ...)] imports Python modules: a top-level
      @tt{import} in the generated program, wherever the source wrote it, once
      per spec.  A name is a module, @racket[(as mod alias)] is a module under
      another name, and @racket[(ref mod name* ...)] is @tt{from mod import
      name, ...}}

@racketblock[
(import math)                  ; import math
(import (as os.path path))     ; import os.path as path
(import (ref math sqrt pi))    ; from math import sqrt, pi
]
@item{@racket[(defmacro (f x* ...) e)], and @racket[(defmacro (f x* ... . rest) e)],
      define a macro -- see @seclink["Macros"]}
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

From Racket, @racket[transpile] is the whole pipeline -- read, check the program
as LM, expand its macros into LB, and compile that to Python:

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

@section{Macros}

@bold{LM} is LB plus @racket[defmacro], and @filepath{passes/macro.rkt} is the
pass that expands it back into LB.  A macro is an ordinary procedure: its
parameters are bound to the @italic{unevaluated} argument forms, and it returns
the form to evaluate in its place.  A form is data -- a symbol is an interned
@tt{Symbol} and a list is a Python list -- so a macro body destructures and
builds forms with the Python operations it already has:

@racketblock[
(defmacro (unless c . body) (list 'if c #f (+ '(begin) body)))
]

A macro signature is a procedure signature, so @racket[defmacro] and
@racket[define] take the same two shapes.  A dotted parameter collects the
remaining forms into a list, so @racket[unless] above can take any number of
body forms, and the macro procedure it becomes has the same parameter list.  This one introduces a binding, and
asks @racket[gensym] for a name nothing else can capture:

@racketblock[
(defmacro (swap! a b)
  (begin
    (define tmp (gensym "tmp"))
    (list 'begin (list 'define tmp a) (list 'set! a b) (list 'set! b tmp))))
]

The pass lowers a macro definition to the @racket[define] of that procedure, and
a macro call to @racket[(eval '<the call form>)]:

@racketblock[
(defmacro (unless c . body) (list 'if c #f (+ '(begin) body)))
(unless #f (print 1))
]

@(verbatim
  (pretty-format
   #:mode 'write
   (unparse-LB
    (expand-macros
     (parse-LM
      (with-input-from-string
       "(begin (defmacro (unless c . body) (list 'if c #f (+ '(begin) body))) (unless #f (print 1)))"
       read))))))

So the expansion happens in the generated program, not in the transpiler: the
program carries the macro procedure, the table of macro names and arities that
tells its @racket[eval] what to expand, and the compiler that @racket[eval]
runs.  That keeps the transpiler small and the generated program self-contained.
A macro call can appear in a value position, can expand into a @racket[define],
and can expand into a call of another macro.

@subsection{Runtime functions}

The prelude writes these Python-side functions for a program that mentions
them, and they are there for any program.  The first five are what makes macros
work:

@itemlist[
@item{@racket[(list x* ...)] makes a list -- the same thing a quoted list is.}
@item{@racket[(apply f a* ... args)] calls @racket[f], spreading the last
      argument over the end of the argument list.}
@item{@racket[(keyword-apply f keywords args)] calls @racket[f] with a dict of
      keyword arguments called @racket[keywords].}
@item{@racket[(gensym)] and @racket[(gensym prefix)] give a fresh symbol,
      interned like any other.}
@item{@racket[(eval form)] compiles a form built at run time -- in the
      language of LB, through the same tables the transpiler uses -- and runs it
      in the program's globals.}
]

The rest reach into Python objects without spelling out reflection:
@racket[(object-ref o k)] is @tt{o[k]}, @racket[(object-set! o k v)] is
@tt{o[k] = v}, @racket[(object-get-attr o name)] is @tt{getattr(o, name)} --
methods included, so @racket[((object-get-attr "abc" "upper"))] is
@tt{"abc".upper()} -- @racket[(object-set-attr! o name v)] is
@tt{setattr(o, name, v)}, and @racket[(object-has-attr? o name)] is
@tt{hasattr(o, name)}.

Things worth knowing:
@itemlist[
@item{There is no automatic hygiene: a macro that introduces a binding uses
      @racket[gensym] for its name, and a name it does not make fresh can be
      captured.}
@item{@racket[defmacro] belongs at the top level, because the generated program
      finds the macro procedure by name.}
@item{@racket[eval] evaluates in the program's globals, so a @racket[define]
      inside a form it runs defines a global.}
@item{The runtime function names belong to the runtime: a program that defines
      its own @racket[list], @racket[apply], @racket[keyword-apply],
      @racket[gensym] or @racket[eval] replaces it.}
]

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
 Read, check as LM, expand macros, and compile to Python.  This is the whole
 transpiler.
}

@defproc[(expand-macros [program any]) any]{
 Expand the macros of an LM program, giving an LB program.
}

@defproc[(parse-LM [datum any/c]) any]{
 Parse one datum as an LM program: LB plus @racket[defmacro].
}

@defproc[(unparse-LM [program any]) any/c]{
 The inverse of @racket[parse-LM]: an LM program as a Racket datum.
}

@defproc[(macro-signature? [v any/c]) boolean?]{
 Is @racket[v] the signature of a macro, @racket[(name param ...)] or
 @racket[(name param ... . rest)]?
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
@item{1.2.0 -- @racket[if] is Lisp's truth: only @racket[#f] is false, so
      @racket[0], @racket[0.0], @racket[""] and @racket['()] are true; and
      @racket[(import spec* ...)] takes @racket[(as mod alias)] and
      @racket[(ref mod name* ...)] as well as a plain module name.}
@item{1.1.0 -- @racket[(import x* ...)] for Python modules, and
      @racket[object-ref], @racket[object-set!], @racket[object-get-attr],
      @racket[object-set-attr!] and @racket[object-has-attr?] for Python
      objects, so reaching into one no longer means spelling out @tt{getattr}
      and a dunder name.}
@item{1.0.0 -- a Lisp: definitions, with a rest parameter; @racket[defmacro] and
      quoted data to build forms with; @racket[trampoline], @racket[set!],
      @racket[raise] and @racket[with-handler]; interned symbols; free variables
      as Python globals; and the Python-side @racket[list], @racket[apply],
      @racket[keyword-apply], @racket[gensym] and @racket[eval].}
@item{0.0.1 -- the rewrite: LB, defined with @seclink["Syntax"]{nanopass}, and a
      Python backend; a command line entry point.}
]
