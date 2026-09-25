#lang racket/base

;; #lang rkt-pythonize: the module is a program, and what it exports is the
;; Python it compiles to.
;;
;; racket/base is re-exported as it is, so define, lambda, if, begin, set!,
;; quote and define-syntax are Racket's -- a macro written with define-syntax is
;; an ordinary Racket macro, and hygiene is Racket's.  The bindings this
;; language adds have an lb: prefix.

(require (for-syntax racket/base)
         (for-syntax "core/compile.rkt"))

;; a free name is a Python global
(define-syntax lb-top
  (syntax-rules () [(_ . x) (#%app #%lb-global (quote x))]))
(define #%lb-global #f)

(define-syntax lb:raise (syntax-rules () [(_ e) (#%app #%lb-raise e)]))
(define #%lb-raise #f)

;; the module body is the program: compile it, and export the Python
;; the body has to be expanded before it can be compiled: it is a module body,
;; so it is expanded in that context, and #%plain-module-begin (when it comes
;; back) is what holds the forms
(begin-for-syntax
 (define (expanded-body stx)
  (define expanded
    (local-expand (datum->syntax stx (cons #'#%plain-module-begin (cdr (syntax->list stx))))
                  'module-begin null))
  (define parts (syntax->list expanded))
  (cond [(and parts (eq? (syntax-e (car parts)) '#%plain-module-begin)) (cdr parts)]
        [else parts])))

(define-syntax lb-module-begin
  (lambda (stx)
    (define python (compile-body (expanded-body stx)))
    #`(#%plain-module-begin
       (#%provide python-code)
       (define python-code #,python))))

(provide (except-out (all-from-out racket/base) #%module-begin #%top)
         (rename-out [lb-module-begin #%module-begin] [lb-top #%top])
         lb:raise)
