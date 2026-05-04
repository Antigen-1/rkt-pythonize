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

My Scheme2Python Compiler. The interpreter is tested against CPython 3.14.4, PyPy 7.3.17 and mypy 1.18.2.

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

@section{Examples}

@subsection{Basic Print}

@codeblock{
((lambda (mod)
   ((lambda (none)
      (vm-apply none '(1)))
    (get-attribute mod '"print")))
 (dynamic-require '"builtins" none))
}

@subsection{Continuation with let/cc}

@codeblock{
(let/cc cc
  ((lambda (mod)
     ((lambda (print)
        (vm-apply print '("1"))
        (cc (vm-apply print '("2")))
        (vm-apply print '("3")))
      (get-attribute mod '"print")))
   (dynamic-require '"builtins" none)))
}

@subsection{Let and Mutable State}

@codeblock{
(let ((mod (dynamic-require "builtins" none))
      (box #f))
  (let ((print (get-attribute mod "print")))
    (letrec ((proc
              (lambda ()
                (if box
                    (vm-apply print '("-1"))
                    (begin
                      (vm-apply print '("1"))
                      (set! box #t)
                      (proc))))))
      (proc))))
}

@subsection{Array and Hash Access}

@codeblock{
(let ((mod (dynamic-require "builtins" none)))
  (let ((print (get-attribute mod "print")))
    (vm-apply print (@ '(("1")) 0))))
}

@subsection{Fibonacci}

@codeblock{
(let ((mod (dynamic-require "builtins" none)))
  (let ((print
         (lambda (v)
           (let ((l '()))
             (<! l v)
             (vm-apply (get-attribute mod "print") l)))))
    (letrec ((f 0)
             (s 1)
             (proc
              (lambda (n)
                (if (equal? n 0)
                    none
                    (let ((r (+ f s)))
                      (print f)
                      (set! f s)
                      (set! s r)
                      (proc (- n 1)))))))
      (proc 10))))
}

@subsection{Named Let}

@codeblock{
(let ((mod (dynamic-require "builtins" none)))
  (let ((print (lambda (v)
                 (let ((l '()))
                   (<! l v)
                   (vm-apply (get-attribute mod "print") l)))))
    (let loop ((n 10) (r 0))
      (if (equal? n 0)
          (print r)
          (loop (- n 1) (+ n r))))))
}

@subsection{Conditionals}

@codeblock{
(let ((mod (dynamic-require "builtins" none)))
  (let ((print (lambda (v)
                 (let ((l '()))
                   (<! l v)
                   (vm-apply (get-attribute mod "print") l)))))
    (print (and))
    (print (or))
    (print (and #t 1))
    (print (or 2 #t))))
}

@subsection{Stream Processing}

@codeblock{
(letrec ((mod (dynamic-require "builtins" none))
         (print (#%vm-procedure (=> mod "print") 1))
         (stream-for-each
          (lambda (p s)
            (if (equal? s none)
                none
                (begin
                  (p (stream-car s))
                  (stream-for-each p (stream-cdr s))))))
         (numbers '(1 2 3 4 5 6))
         (number-stream (let loop ((i 0))
                          (if (equal? i (length numbers))
                              none
                              (stream-cons (@ numbers i)
                                           (loop (+ i 1)))))))
  (stream-for-each print number-stream)
  (stream-for-each print number-stream))
}

@subsection{Scripting}

@codeblock{
(#%script-begin
 (define for-each (lambda (p l) (if (eq? l null) none (begin (p (car l)) (for-each p (cdr l))))))
 (for-each print (cons 1 (cons 2 (cons 3 null)))))
}

@subsection{Prime Numbers with Stream (Benchmark)}

@codeblock{
(letrec ((builtin (dynamic-require "builtins" none))
         (print (#%vm-procedure (=> builtin "print") 1))
         (stream-filter (lambda (p s)
                          (if (equal? s none)
                              none
                              (let ((f (stream-car s)))
                                (if (p f)
                                    (stream-cons f (stream-filter p (stream-cdr s)))
                                    (stream-filter p (stream-cdr s)))))))
         (numbers (let loop ((n 2))
                    (stream-cons n (loop (+ n 1)))))
         (primes (let loop ((ns numbers))
                   (let ((first (stream-car ns))
                         (rest (stream-cdr ns)))
                     (stream-cons first
                                  (loop (stream-filter
                                         (lambda (n)
                                           (not (equal? (modulo n first) 0)))
                                         rest)))))))
  (let loop ((n 300) (ps primes))
    (if (equal? n 0)
        (print (stream-car ps))
        (loop (- n 1) (stream-cdr ps)))))
}

@section{Functions}

@defparam[current-primitives primitives (listof symbol?)]

@defthing[#:kind "language" L any/c]
@defthing[#:kind "language" LS any/c]
@defproc[#:kind "compiler" (compile-scheme-code (code any/c) (dest path-string?) (#:raw? raw? boolean? #f) (#:script? script? boolean? #f)) any]
@defproc[#:kind "parser" (parse-L (code any/c)) any]
@defproc[#:kind "unparser" (unparse-L (s-exp any/c)) any]
@defproc[#:kind "parser" (parse-LS (code any/c)) any]
@defproc[#:kind "unparser" (unparse-LS (s-exp any/c)) any]
