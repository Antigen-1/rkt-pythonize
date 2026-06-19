#lang scribble/manual
@require[@for-label[rkt-pythonize
                    racket/base
                    ]
                   nanopass/base
                   rkt-pythonize
                   racket/pretty
                   @for-syntax[(submod "../main.rkt" test) racket/base racket/list racket/pretty]
                   ]

@title{rkt-pythonize}
@author{zhanghao}

@defmodule[rkt-pythonize]

My Scheme2Python Compiler. The interpreter is tested against CPython(3.10-3.14), PyPy(3.10-3.11) and mypy 1.18.

@section{Supported Features}

@itemlist[
@item{Named let}
@item{@racket[let*]}
@item{@racket[let/cc]}
@item{TCO (Tail-Call Optimization)}
@item{Stream}
@item{Inlining}
@item{Partial evaluation}
@item{Internal defines (@racket[define] inside @racket[lambda], @racket[let], @racket[letrec], @racket[let*], @racket[named let], @racket[with-handler], @racket[begin], and @racket[cond] branch bodies)}
@item{Defines may appear after expressions in any body context}
@item{Top-level @racket[define] via @racket[begin]}
]

@section{Syntax}

The language @racket[L] supports @racket[define] in body positions (e.g., inside @racket[lambda], @racket[let], @racket[letrec], etc.).

@codeblock[#:keep-lang-line? #f]{
#:lang nanopass
@(pretty-format #:mode 'write (language->s-expression L))
}

@section{Default Primitives}

@code{@(pretty-format #:mode 'write (list 'quote (current-primitives)))}

@section{Examples}

@(define-for-syntax render-example
    (lambda (code result)
      #`(begin
          (codeblock
            #,(pretty-format #:mode 'write code))
          (racketresult #,result))))
@(define-syntax (helper _)
  (syntax-case #'() ()
    (()
     #`(begin #,@(for/list ((pair (in-list (reverse (unbox example-table))))) (render-example (car pair) (cdr pair)))))))
@(helper)

@section{Functions}

@defparam[current-primitives primitives (listof symbol?)]

@defthing[#:kind "language" L any/c]
@defproc[#:kind "compiler" (compile-scheme-code (code any/c) (#:opt? opt? boolean? #t)) string?]
@defproc[#:kind "parser" (parse-L (code any/c)) any]
@defproc[#:kind "unparser" (unparse-L (s-exp any/c)) any]
@defthing[#:kind "evaluator" py-lib-string string?]

@section{Changelog}

@subsection{48.0}
@itemlist[
@item{@racket[define] now supports appearing after expressions in all body contexts}
@item{Added @racket[>=] and @racket[<=] primitives}
@item{Added @racket[let*] with internal @racket[define] support}
@item{Fixed @racket[=>] and @racket[=>!] parameter ordering (now uses left-to-right evaluation)}
]

@subsection{47.0}
@itemlist[
@item{Script mode merged with normal mode: @racket[define] now works in all body positions (@racket[lambda], @racket[let], @racket[letrec], @racket[named let], @racket[with-handler], @racket[begin], @racket[cond] branches)}
@item{Removed separate @racket[LS] language and @racket[#%script-begin] form}
@item{Removed @tt{-s}/@tt{--script} CLI flag (define support is always enabled)}
@item{@racket[compile-scheme-code] no longer accepts @racket[#:script?] parameter}
]
