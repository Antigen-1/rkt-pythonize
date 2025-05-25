#lang scribble/manual
@require[@for-label[rkt-pythonize
                    racket/base
                    ]
                   nanopass/base
                   rkt-pythonize]

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

@code[#:lang "racket"]{@(format "~s" (language->s-expression L))}
