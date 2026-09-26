Changelog
=========

Newest first.  Every version here is a state someone could have, so the small
ones are listed too.  Everything before 2.0.0 was a different design -- a
Lisp-to-Python transpiler (LB, then LE, then LM) driven by a nanopass pipeline,
with a runtime macro system of its own; the last of those was 1.3.3.

3.5.4
-----

- `CHANGELOG.md`, this file, with a pointer to it from the README and the
  manual.

3.5.3
-----

- `(import x)` is an expression: it is the module `importlib.import_module`
  returns, so a module is a value like any other.  `x` is a name (which names
  its own module), a string, or an expression the runtime evaluates, and the
  call carries the `import_module` piece.
- The manual had the pass list twice and the README repeated the pipeline in
  the design notes; one copy of each is left.

3.5.2
-----

- `core/names.rkt`: the tables the passes share -- `munged`, the operators, the
  pieces and their order.
- `runtime-names` and `python-builtins` are parameters there, so a program can
  widen what the scope check treats as known from a `begin-for-syntax`.
- README and manual show the five-pass pipeline.

3.4.2
-----

- `core/check-expression.rkt` and `core/check-scope.rkt`: the two checks are
  passes of their own, and `core/render.rkt` only renders.
- Five passes: two checks (`check-expression`, `check-scope`) and three
  rewrites (`explicit`, `lift`, `render`).

3.3.2
-----

- Three passes, one file each: `core/explicit.rkt` (make-explicit),
  `core/lift.rkt` (lift) and `core/render.rkt` (render, formerly
  `core/compile.rkt`).

3.3.1
-----

- A lift pass: `core/lift.rkt` turns LE into LL, lifting every `lambda` and
  every locally defined procedure to a top-level define that takes the
  variables it captures from enclosing scopes as leading parameters.
- `lambda` takes the same parameter lists as `define` -- a rest parameter
  included, and it collects into a list -- and its body is a statement
  sequence.
- `set!` of a captured variable is a compile error, and so is a capture that is
  shadowed where the procedure is used.

3.2.1
-----

- No tail-call recognition: the last body of `trampoline` is the function it
  calls, and the compiler neither looks for a tail call nor wraps one.

3.1.1
-----

- The trampoline example in the docs shows the Python it renders instead of a
  number it never printed.

3.1.0
-----

- `lambda`, whose body was a single expression at the time.
- A lexical scope check: a name the program never binds is logged at warning
  level, for a reference and for a `set!` alike.

3.0.0
-----

- rkt-pythonize is a library with one export, the macro `#%python-code`, whose
  body is LE and whose value is the Python rendered at expansion time.
- No `#lang`, no reader, no command line, and no Racket form is replaced.

2.0.0
-----

- `#lang rkt-pythonize`: a module is a Python program, compiled at expansion
  time, and exports `python-code`; the executable only writes it out.
- The data-based pipeline and the runtime macro machinery are gone, and with
  them `Symbol`, `eval`, `gensym` and `defmacro`.
