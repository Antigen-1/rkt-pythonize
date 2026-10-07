#lang racket/base

;; rkt-pythonize is a library, and its exports are #%python-code and defmacro:
;;
;;   (#%python-code <LE form> ...)
;;
;; is the Python those forms compile to.  The compilation happens at expansion
;; time, so the value of the macro is the rendered source, a string.
;;
;;   (defmacro (name params ...) body ...)
;;
;; binds name to a transformer for the LE inside #%python-code.  The body runs
;; at compile time on the argument forms as data and returns the form that takes
;; their place; name is a syntax binding, so the macro is visible after the
;; defmacro in its own module, or where the module that wrote it is required.
;; There is no hygiene: use gensym for a name of the macro's own.
;;
;; LE is the language core/compile.rkt describes: Racket's s-expression syntax
;; with statements and expressions apart, procedures only through define with
;; any number of bodies, cond, raise, with-handler and trampoline, and quoted
;; data without symbols.  There are no macros and no eval inside it: a macro is
;; written outside it, in Racket, with defmacro.
;;
;; core/names.rkt says what a name is in Python, and it is provided here at the
;; phase the compiler runs in, so a module can say what its program's names are
;; and how they are spelled before the forms that use them:
;;
;;   (begin-for-syntax (python-name-style 'camel))   how a name is joined
;;   (begin-for-syntax (prelude-prefix "_pz_"))      what the prelude is called
;;   (begin-for-syntax (runtime-names (cons 'sys (runtime-names))))
;;   (begin-for-syntax (python-name 'object-ref))    "objectRef", now

(require (for-syntax racket/base)
         (for-syntax "core/names.rkt")
         (for-syntax "core/expand-macro.rkt")
         (for-syntax "syntax/cond.rkt")
         (for-syntax "syntax/thread.rkt")
         (for-syntax "core/check-expression.rkt")
         (for-syntax "core/check-scope.rkt")
         (for-syntax "core/explicit.rkt")
         (for-syntax "core/lift.rkt")
         (for-syntax "core/render.rkt"))

;; A defmacro is a syntax binding whose transformer is a le-macro, which is what
;; tells the pass that this name is a macro for LE.  The body is compiled at
;; phase 1, so requiring this library also puts racket/base there (the for-syntax
;; provide at the end of this module): quasiquote, gensym and the rest are then
;; ordinary names in a defmacro body.
;;
;; With #:space the macro is bound under the space and the name joined by a dot
;; -- (defmacro #:space a (twice x) ...) binds a.twice -- so a program can have
;; two macros of the same name, and the space travels with the transformer.  A
;; use says which one it means: (twice #:space a arg ...).
(define-syntax (defmacro stx)
  (define (space-name space name)
    (datum->syntax name (string->symbol (format "~a.~a" (syntax->datum space) (syntax-e name)))))
  (define (space-key space name)
    (datum->syntax name (list 'quote (syntax->datum space))))
  (syntax-case stx ()
    [(_ marker space (name . params) body ...)
     (eq? (syntax->datum #'marker) '#:space)
     #`(define-syntax #,(space-name #'space #'name)
         (le-macro #,(space-key #'space #'name) (lambda params body ...)))]
    [(_ marker space name transformer)
     (eq? (syntax->datum #'marker) '#:space)
     #`(define-syntax #,(space-name #'space #'name)
         (le-macro #,(space-key #'space #'name) transformer))]
    [(_ (name . params) body ...)
     #'(define-syntax name (le-macro #f (lambda params body ...)))]
    [(_ name transformer)
     #'(define-syntax name (le-macro #f transformer))]))

(define-syntax #%python-code
  (lambda (stx)
    ;; (#%python-code #:space key form ...) is the whole body in that space;
    ;; without the keyword the body is in no space, which is the plain names.
    ;; The keyword is read here, not wrapped around the forms: the body keeps
    ;; its own statement and expression structure.
    (define parts (cdr (syntax->list stx)))
    (define spaced? (and (pair? parts)
                         (keyword? (syntax-e (car parts)))
                         (eq? (syntax-e (car parts)) '#:space)))
    (when (and spaced? (null? (cdr parts)))
      (raise-syntax-error '#%python-code "a #:space body names a space" stx))
    (define space (and spaced? (syntax->datum (cadr parts))))
    (define forms (if spaced? (cddr parts) parts))
    ;; LE --expand-macro--> --syntax/--> LE --check-expression-->
    ;; --check-scope--> LE --make-explicit--> LL --lift--> LB --render--> Python
    ;;
    ;; The syntax/ expansions are the high-level syntax: cond, and the threading
    ;; operators.  They run after the macros (whose expansion may use them) and
    ;; before the checks, which then only ever see LE's own forms.
    (datum->syntax stx
                   (render-program
                    (lift-program
                     (explicit-program
                      (check-scope-program
                       (check-expression-program
                        (expand-thread-program
                         (expand-cond-program
                          (expand-macro-program forms space)))))))))))

(provide #%python-code
         defmacro
         (for-syntax (all-from-out racket/base))
         (for-syntax python-name python-keyword-name python-name-style
                     prelude-prefix runtime-names python-builtins))
