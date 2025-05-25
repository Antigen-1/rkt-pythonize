#lang scribble/manual
@require[@for-label[rkt-pythonize
                    racket/base]]

@title{rkt-pythonize}
@author{zhanghao}

@defmodule[rkt-pythonize]

My Scheme2Python Compiler.

Supported features include:

@itemlist[
@item{Named let}
@item{@racket[let/cc]}
@item{TCO(Tail-Call Optimization)}
]
