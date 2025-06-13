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

@section{Supported Features}

@itemlist[
@item{Named let}
@item{@racket[let/cc]}
@item{TCO(Tail-Call Optimization)}
@item{Stream}
@item{Inlining}
@item{Partial evaluation}
]

@section{Syntax}

@codeblock[#:keep-lang-line? #f]{
#:lang nanopass
@(pretty-format #:mode 'write (language->s-expression L))
}

@section{Default Primitives}

@code{@(pretty-format #:mode 'write (list 'quote (current-primitives)))}

@section{Functions}

@defparam[current-primitives primitives (listof symbol?)]

@defproc[#:kind "compiler" (compile-L (code any/c) (dest path-string?) (#:raw? raw? boolean? #f)) any]
