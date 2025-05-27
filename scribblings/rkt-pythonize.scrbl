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
]

@section{Syntax}

@codeblock[#:keep-lang-line? #f]{
#:lang nanopass
@(pretty-format #:mode 'write (language->s-expression L))
}
