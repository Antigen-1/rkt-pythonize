#lang scribble/manual
@require[@for-label[rkt-pythonize
                    racket/base
                    ]
                   nanopass/base
                   rkt-pythonize
                   racket/pretty
                   ]

@title{rkt-pythonize}
@author{zhanghao}

@defmodule[rkt-pythonize]

My Scheme2Python Compiler.

This library has been tested against Python3.10.

@section{Supported Features}

@itemlist[
@item{Named let}
@item{@racket[let/cc]}
@item{TCO(Tail-Call Optimization)}
@item{Stream}
@item{Inlining}
@item{Partial evaluation}
@item{Scripting}
]

@section{Syntax}

@codeblock[#:keep-lang-line? #f]{
#:lang nanopass
@(pretty-format #:mode 'write (language->s-expression L))
}
@codeblock[#:keep-lang-line? #f]{
#:lang nanopass
@(pretty-format #:mode 'write (language->s-expression LS))
}

@section{Default Primitives}

@code{@(pretty-format #:mode 'write (list 'quote (current-primitives)))}

@section{Functions}

@defparam[current-primitives primitives (listof symbol?)]

@defthing[#:kind "language" L any/c]
@defthing[#:kind "language" LS any/c]
@defproc[#:kind "compiler" (compile-scheme-code (code any/c) (dest path-string?) (#:raw? raw? boolean? #f) (#:script? script? boolean? #f)) any]
@defproc[#:kind "parser" (parse-L (code any/c)) any]
@defproc[#:kind "unparser" (unparse-L (s-exp any/c)) any]
@defproc[#:kind "parser" (parse-LS (code any/c)) any]
@defproc[#:kind "unparser" (unparse-LS (s-exp any/c)) any]
