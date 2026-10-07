# Changelog

Newest first.  Every version here is a state someone could have, so the
small ones are listed too.  Everything before 2.0.0 was a different
design – a Lisp-to-Python transpiler (LB, then LE, then LM) driven by a
nanopass pipeline, with a runtime macro system of its own; the last of
those was 1.3.3.

## 4.0.1

* The documents are one text in two files: `"scribblings/readme.scrbl"`
  is the README and the manual’s chapters, and
  `"scribblings/changelog.scrbl"` is the changelog and the manual’s last
  chapter.  `"scribblings/manual.rkt"` assembles the manual from those
  two documents – the README’s chapters are read out of its document and
  reparented as they are, so the manual’s chapters and the changelog sit
  at one level, the README is the whole language rather than only its
  architecture, and no chapter is titled README.

* `"build-docs.rkt"` writes only the generated Markdown \(`"README.md"`
  and `"CHANGELOG.md"`), and `"installer.rkt"` runs it when the package
  is installed or updated; `"info.rkt"` names the installer and points
  its `scribblings` at the manual module.

## 4.0.0

The line from 3.0.0 to here, in one place.  4.0.0 is the language 3.12.6
describes: nothing in the compiler changed for it, and the version says
what the changes since 3.0.0 add up to.

* rkt-pythonize is a library with one export, `#%python-code`, whose
  value is the Python its forms compile to.  The compilation happens at
  expansion time: there is no `#lang`, no reader and no command line,
  and the data-based pipeline and runtime macro machinery of the 2.x
  line are gone.

* The pipeline is one file a stage: a macro expansion
  \(`"core/expand-macro.rkt"`), the high-level syntax
  \(`"syntax/cond.rkt"`, `"syntax/thread.rkt"`), two checks
  \(`"core/check-expression.rkt"`, `"core/check-scope.rkt"`), two
  lowerings (`"core/explicit.rkt"`, `"core/lift.rkt"`) and the renderer
  (`"core/render.rkt"`).  LE becomes LL becomes LB, the whole program is
  walked before a line of Python is written, and a name the program
  never binds is a logged warning rather than a guess.

* `"core/names.rkt"` is the one module that says what a name is in
  Python – `python-name`, `python-keyword-name`, `python-name-style`
  (`'snake`, or `'camel` where a trailing `?` is the predicate `is`) and
  `piece-name` – and nothing else spells a Python name out.  The
  conversion is idempotent, and the checks compare the Python name in
  both directions, so `x-y` and `x_y` are one name.

* Keyword arguments, Racket’s way: a parameter list takes `#:k k` and a
  call writes `(f 1 #:k 2)`, a keyword parameter is keyword-only in the
  Python, a call writes its keywords where it likes, and the compiler’s
  own call reaches the runtime’s `apply` whatever the program names
  things.  A keyword is syntax, never a datum.

* Macros: `defmacro` binds a transformer for the LE inside
  `#%python-code`, `#:space` lets two macros share a name, there is no
  hygiene – a macro asks `gensym` for a name of its own – and a macro
  takes keyword arguments, because a transformer is a Racket procedure.

* The runtime is a prelude of pieces, and it carries only the pieces the
  program asks for: `raise` and `with-handler`, `trampoline` \(tail
  calls are explicit: the compiler does not recognise one, so the thunk
  is yours), `import`, `list`, `apply`, `keyword-apply`, the `object-`
  family, and the operators, which are syntax and not procedures.  A
  begin where a value belongs is a procedure of its own rather than a
  piece, and the prelude’s names carry `prelude-prefix`, so no name a
  program writes is one of them.

* The high-level syntax lives in `"syntax/"`: `cond`, and the threading
  operators `(-> x step ...)` and `(->> x step ...)`, which thread the
  value into the first argument of a step or into the last positional
  one, stop at the first step whose value is an `Exception`, and are run
  by `trampoline`, so a chain is flat however many steps it has.

* Names the compiler makes up are made of a UUID – `_lift_3f9a1b2c` for
  a lifted procedure, `_lift_3f9a1b2c_inner` for one the program called
  `inner` – so a name a program defines at the top level is its own, and
  its Python name is knowable from the source alone, which is what a
  program that exports its names reads.

* The documents are each other’s tests: `README.md` and this changelog
  are generated from `"scribblings/"`, the manual’s examples are
  compiled and run with `python3` while the manual is built, and
  `"tests/"` renders programs, looks at the Python, and runs it.

## 3.12.6

* What interrupts `->` and `->>` is an exception, not `None`: a step
  whose value is an instance of `Exception` or of a subclass of it ends
  the chain there, the steps after it do not run, and the chain’s value
  is that instance.  `None` is a value like any other, so a step may
  hand it on, and a chain that runs to its end is told from one that was
  interrupted by what comes back: the instance itself, or the value of
  the last step.

## 3.11.6

* A `begin` where a value belongs is lifted into a procedure of its own
  and called where it stands, so the Python is a `def` of statements
  rather than one call whose arguments are the program: `(if (begin
  (print "t") #t) 1 2)` is a `def` that prints and returns, and a call
  to it.  A begin that stands as a statement, and the begin a body’s
  forms make, are statements as before.  The runtime has no `begin`
  piece any more.

* That a definition cannot stand in a begin where a value belongs is
  `begin`’s semantics, not a limit of the compiler – a begin sequences
  expressions, and a definition belongs in a body, where a name is
  defined – and the refusal now says so.

* `"syntax/"` is the high-level syntax, which runs after the macros and
  before the checks: `"syntax/cond.rkt"`, the `cond` expansion that was
  `"core/expand-cond.rkt"`, and `"syntax/thread.rkt"`, which is new.

* `(-> x step ...)` threads into the first argument of each step and
  `(->> x step ...)` into the last positional one.  `None` interrupts: a
  step whose value is None ends the chain there and the chain’s value is
  None, and the steps after it do not run.  The chain is run by
  `trampoline` – each step answers the call that makes the next one – so
  it is flat however many steps it has, and the value of the last step
  comes back in a one-element list the driver unwraps, so a value that
  is callable is not mistaken for the next step.

* `None`, `True` and `False` are the Python constants a program reads,
  and not names a Python keyword takes a trailing `_` from: `None` is
  what the threading operators interrupt on.  Binding one is a compile
  error.

## 3.10.6

* The camel predicate rule is written down, and the conversion tables
  agree with the compiler: with `'camel` a name that ends with `?` is
  the predicate it says it is – the `?` comes off, the first letter is
  capitalized, and `is` goes in front, so `even?` is `isEven` and
  `my-func?` is `isMyFunc` – while a `?` that is not the end of a name
  is `P`, as in `aPb` where `'snake` has `a_pb`.  The tables and the
  3.10.5 note below said `evenP`, which the compiler has not done since
  the rule changed in 3.10.5; nothing in the compiler changed here.

## 3.10.5

* Keyword arguments, Racket’s way, on both sides of a call: a parameter
  list takes `#:k k` – `(define (f a #:k k) ...)`, and `lambda` with it
  – and a call writes `(f 1 #:k 2)`.  A keyword parameter is
  keyword-only in the Python, `def f(a, *, k)`, and the star stands
  where the rest parameter would, `def f(a, *rest, k)`, so it is
  keyword-only where the procedure is called, as it is in Racket.

* A call writes its keyword arguments where it likes: `(f 1 #:k 2 3)` is
  `f(1, 3, k=2)`.  The Python puts the positional arguments first and
  the keyword ones after, which is the order Python evaluates them in.

* `core/names.rkt` is the one module that says what a name is in Python.
  `python-name` is the conversion, `piece-name` is what a piece of the
  prelude is called, and nothing else spells a Python name out – and the
  conversion reads the name and `python-name-style` and nothing else:
  `(python-name 'even?)` is `even_p`, `(python-name 'set-car!)` is
  `set_car_b`, a name that is a Python keyword takes a trailing `_` and
  one that starts with a digit takes a leading one.

* `python-name-style` is a parameter, `'snake` by default and `'camel`
  otherwise, so a module says how its names are spelled:
  `(begin-for-syntax (python-name-style 'camel))` before its
  `#%python-code` forms compiles `even?` to `isEven` and `object-ref` to
  `objectRef`.  With `'camel` the letter after a `-` is capitalized, a
  `-` with no letter after it is joined instead, so no name is lost, and
  a name that ends with `?` is the predicate it says it is: the `?`
  comes off, the first letter is capitalized, and `is` goes in front.
  `python-name` takes a prefix and a suffix too, Python text around the
  conversion, which is how the compiler names what it lifts.
  `python-name`, `python-name-style`, `runtime-names` and
  `python-builtins` are provided at the phase the compiler runs in,
  where they were only in `core/names.rkt` before.

* Every name of a program is `python-name` of the name the source wrote
  and nothing else, so the Python name of a top-level define is knowable
  from the source alone – which is what a program that exports its names
  reads. A name the compiler makes up for itself – `_lift_3f9a1b2c` for
  a lifted procedure, `_lift_3f9a1b2c_inner` for one the program called
  `inner` – is made of a UUID, so it is not one the program writes and a
  lifted procedure cannot take a program’s name.  What a check compares
  is the Python name, in both directions: two top-level names that are
  one Python name \(`x-y` and `x_y`) are a compile error rather than one
  silently overwriting the other, a parameter list that spells one
  Python name twice is refused, a call that gives one keyword two values
  under two spellings of it is refused rather than left to a Python
  `SyntaxError`, and a name written as it is in Python is the name the
  program defined – `(define x-y 1)` with `(set! x_y 2)` somewhere is
  that one variable, which the scope check knows and does not warn
  about.

* A keyword’s name is converted as a name is – `#:foo-bar` is `foo_bar`
  in one style and `fooBar` in the other – and a call converts the
  keyword it writes whether or not the program defines the procedure it
  calls, so `(sorted xs #:reverse #t)` is `sorted(xs, reverse=True)`.  A
  piece that answers to an LE name is named by the same conversion, so
  `(object-ref xs 0)` and its `def` agree in either style.  In Python a
  keyword and the parameter it fills are one name, so a parameter list
  spells it twice, `#:k k`; `(define (f #:k v) v)` is a compile error.

* The prelude’s names carry `prelude-prefix`, an identifier’s worth of a
  UUID the compiler asks for once a run (`uuid`, a new dependency), so
  no name a program writes is one of the prelude’s: a program may define
  `list`, `apply` or `_raise`, and its own is what the name means where
  it is bound, while the pieces the compiler calls itself – `(raise e)`,
  `(with-handler ...)`, the rest parameter of a lifted procedure that
  captures – are the prefixed ones and out of the program’s reach.  Pin
  the prefix to have the same Python every time, or to nothing to read
  the prelude as it is written; the manual’s examples pin it to nothing.

* A keyword is syntax, not data: it belongs in a call or in a parameter
  list, and nowhere else.  A keyword in an expression position or a
  statement position, a `set!` of one, and a keyword inside quoted data
  or inside a literal (`'#:k`, `#(1 #:k)`) are compile errors now, the
  last two where the form is, rather than at render time.

* `defmacro` takes keyword arguments too: a transformer is a Racket
  procedure, so `(defmacro (m x #:k k) ...)` is called `(m 1 #:k 2)`, a
  macro’s keyword is not converted – two of them are two keywords there
  whatever they would be in Python – and one may carry a default, `#:k
  [k 5]`, which a procedure’s may not.  A call that names a keyword the
  transformer does not take, or leaves out one it needs, is a compile
  error that says which.

* `core/params.rkt`: the parameter list `define` and `lambda` share and
  the arguments a call hands a procedure.  A parameter that is not a
  name, a keyword with no parameter and an argument that writes one
  keyword twice are compile errors now, all of them Python’s own rules.
  `core/lift.rkt` carries keyword parameters through a lift and the
  reference it makes, and `core/render.rkt` renders a signature
  keyword-only.

* `apply` carries a keyword argument to the function it calls, since
  that is the function the keyword is for: `(apply f xs #:k v)` is
  `f(*xs, k=v)`, and the piece takes `**keywords`.  A lifted procedure
  that captures, takes a rest parameter and takes keywords passes all
  three on, which is what that piece is for.

* `tests/names.rkt` pins the conversion character by character in both
  styles, and `tests/camel.rkt` is a module that set the style and
  renders its own forms.

## 3.9.5

* `defmacro`: a macro written outside `#%python-code`, in Racket, whose
  body runs while the program is compiled on the argument forms as data
  and returns the form that takes their place.  Each macro is a syntax
  binding, so it is visible after its definition in its own module or
  where that module is required, and nowhere else; there is no hygiene,
  so a macro asks `gensym` for a name of its own.

* `#:space` for `defmacro`: two macros may share a name if they live in
  different spaces.  The space is part of the binding’s name
  \(`(defmacro #:space a (twice x) ...)` binds `a.twice`, which is what
  a `provide` and a `require` carry), and `(#:space a (twice x))`
  expands that macro call and its subforms in space `a`; an inner
  `#:space` overrides an outer one, `(#:space #f ...)` is the plain
  name, and `#%python-code` takes the keyword too, for a whole body.
  The key is not one of the macro’s arguments, and a `#:space` form that
  names no macro is a compile error.

* `(+ x)` is `x`, which the README and the manual have said since 3.6.4
  and `core/names.rkt`’s tables did not do: `+` is in the prefix table
  now, and is still infix with two or more operands.

* `core/expand-macro.rkt`, the pass that expands them.  It runs first,
  so a macro may expand into anything LE has, `cond` included, and into
  another macro; a macro that expands into itself stops after a bounded
  number of expansions with an error, and so do a transformer given the
  wrong number of arguments, one that raises, and one that returns
  something that is not a form. Seven passes.

## 3.8.5

* `cond`: the body of the first clause whose test is true, with the
  clauses after it in the else position.  A clause body is an implicit
  `begin`, so it may hold several forms; `else` is a keyword, and legal
  only as the last clause’s test; a `cond` that runs out of clauses
  raises when no test is true.

* `core/expand-cond.rkt`, the pass that expands it into nested `if`s and
  `begin`s.  It runs first, so the checks after it only see `if` and
  `begin`, and the pipeline is six passes.

## 3.7.5

* The README’s two tables are hand-aligned boxed blocks: the Markdown
  backend has no table, and the flattened one it made of them ran cells
  together where a cell filled its column.

* The README’s layout listing names `scribblings/readme.scrbl` and
  `scribblings/changelog.scrbl`, the manual’s first and last chapters,
  and `./build-docs.sh`, which renders them into `README.md` and
  `CHANGELOG.md`.

## 3.7.4

* The prelude carries only the pieces a program asked for, and a piece
  brings the pieces it needs: `with-handler` catches `_Raised`, so a
  program that uses it carries `raise` even when it never raises itself.
  A program with no `import` has no `importlib`, and one with no
  `trampoline` has no `_trampoline`.

* `README.md` and this changelog are generated from
  `scribblings/readme.scrbl` and `scribblings/changelog.scrbl` by
  `./build-docs.sh`; edit the sources.

* The manual’s examples are compiled and run with `python3` while the
  manual is built, so an example that stops working fails the build.

## 3.6.4

* Operators are syntax, and the set is the one Python’s builtins
  support: `+ - * / quotient modulo expt`, `& | ^ << >>`, `= not= < > <=
  >=` (`equal?` is `=`, `eq?` is `is`), `in`, `and`, `or` infix with two
  or more operands, and `not`, `-`, `~` prefix with one (`(+ x)` is
  `x`). An operator is never a value: `(apply + xs)` and `(print +)` are
  refused, and one is checked in `check-expression` instead of only at
  render time.

## 3.5.4

* `CHANGELOG.md`, this file, with a pointer to it from the README and
  the manual.

## 3.5.3

* `(import x)` is an expression: it is the module
  `importlib.import_module` returns, so a module is a value like any
  other. `x` is a name (which names its own module), a string, or an
  expression the runtime evaluates, and the call carries the
  `import_module` piece.

* The manual had the pass list twice and the README repeated the
  pipeline in the design notes; one copy of each is left.

## 3.5.2

* `core/names.rkt`: the tables the passes share – `munged`, the
  operators, the pieces and their order.

* `runtime-names` and `python-builtins` are parameters there, so a
  program can widen what the scope check treats as known from a
  `begin-for-syntax`.

* README and manual show the five-pass pipeline.

## 3.4.2

* `core/check-expression.rkt` and `core/check-scope.rkt`: the two checks
  are passes of their own, and `core/render.rkt` only renders.

* Five passes: two checks (`check-expression`, `check-scope`) and three
  rewrites (`explicit`, `lift`, `render`).

## 3.3.2

* Three passes, one file each: `core/explicit.rkt` (make-explicit),
  `core/lift.rkt` (lift) and `core/render.rkt` (render, formerly
  `core/compile.rkt`).

## 3.3.1

* A lift pass: `core/lift.rkt` turns LE into LL, lifting every `lambda`
  and every locally defined procedure to a top-level define that takes
  the variables it captures from enclosing scopes as leading parameters.

* `lambda` takes the same parameter lists as `define` – a rest parameter
  included, and it collects into a list – and its body is a statement
  sequence.

* `set!` of a captured variable is a compile error, and so is a capture
  that is shadowed where the procedure is used.

## 3.2.1

* No tail-call recognition: the last body of `trampoline` is the
  function it calls, and the compiler neither looks for a tail call nor
  wraps one.

## 3.1.1

* The trampoline example in the docs shows the Python it renders instead
  of a number it never printed.

## 3.1.0

* `lambda`, whose body was a single expression at the time.

* A lexical scope check: a name the program never binds is logged at
  warning level, for a reference and for a `set!` alike.

## 3.0.0

* rkt-pythonize is a library with one export, the macro `#%python-code`,
  whose body is LE and whose value is the Python rendered at expansion
  time.

* No `#lang`, no reader, no command line, and no Racket form is
  replaced.

## 2.0.0

* `#lang rkt-pythonize`: a module is a Python program, compiled at
  expansion time, and exports `python-code`; the executable only writes
  it out.

* The data-based pipeline and the runtime macro machinery are gone, and
  with them `Symbol`, `eval`, `gensym` and `defmacro`.
