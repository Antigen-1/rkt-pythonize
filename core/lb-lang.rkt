#lang racket/base

;; The language a program is written in.  A program is a module of this
;; language, so the body's initial bindings are exactly what this module
;; provides: the kernel's application, literal, top-level-reference and body
;; forms, and LB's own forms.  Nothing Racket binds a value to is provided, so
;; a name LB leaves free stays free in the program (it comes out as `#%top`).

(require (for-syntax racket/base))

;; the body of a program is a body: no runtime submodule, no printing wrapper
(define-syntax #%module-begin
  (syntax-rules () [(_ e ...) (#%plain-module-begin e ...)]))

(provide #%module-begin #%app #%datum #%top
         begin set! quote
         define if with-handler trampoline raise defmacro)
;; a macro body is Racket code, and it runs a phase up: it gets the names it
;; works on forms with
(provide (for-syntax #%app #%datum lambda if begin quote
                     list cons map apply
                     syntax->datum syntax->list datum->syntax))

;; A name LB does not bind is not an error: it is a Python global, which the
;; generated program has.  The expander asks every free identifier through
;; #%top, so this is where "free variable" becomes a core form.
(define-syntax #%top
  (syntax-rules () [(_ . x) (#%app #%lb-global (quote x))]))
(define #%lb-global #f)

;; the forms that bind nothing are applications of a core variable, which the
;; expander has no opinion about and leaves alone
(define #%lb-if #f)
(define #%lb-with-handler #f)
(define #%lb-trampoline #f)
(define #%lb-raise #f)

(define-syntax if (syntax-rules () [(_ c t e) (#%app #%lb-if c t e)]))
(define-syntax with-handler
  (syntax-rules () [(_ h e) (#%app #%lb-with-handler h e)]))
(define-syntax trampoline
  (syntax-rules () [(_ e) (#%app #%lb-trampoline e)]))
(define-syntax raise (syntax-rules () [(_ e) (#%app #%lb-raise e)]))

;; a definition has to be a definition the expander knows about, or it cannot
;; give a name a macro introduces a binding of its own
(define-syntax define
  (syntax-rules ()
    [(_ (f x ...) body) (define-values (f) (lambda (x ...) body))]
    [(_ n v) (define-values (n) v)]))

;; a macro is a Racket transformer whose body works on data: its arguments
;; arrive as datums, it answers a datum, and the expander adopts the answer into
;; the macro's own scope
(define-syntax defmacro
  (syntax-rules ()
    [(_ (m . formals) body)
     (define-syntax m
       (lambda (stx)
         (datum->syntax stx
                        (apply (lambda formals body)
                               (map syntax->datum (cdr (syntax->list stx)))))))]))
