#lang racket/base

;; rkt-pythonize is a library, and its one export is #%python-code:
;;
;;   (#%python-code <LE form> ...)
;;
;; is the Python those forms compile to.  The compilation happens at expansion
;; time, so the value of the macro is the rendered source, a string.
;;
;; LE is the language core/compile.rkt describes: Racket's s-expression syntax
;; with statements and expressions apart, procedures only through define with
;; any number of bodies, raise, with-handler and trampoline, and quoted data
;; without symbols.  There are no macros and no eval inside it, so a Racket
;; macro that writes LE is how a program grows sugar.

(require (for-syntax racket/base)
         (for-syntax "core/check-scope.rkt")
         (for-syntax "core/explicit.rkt")
         (for-syntax "core/lift.rkt")
         (for-syntax "core/render.rkt"))

(define-syntax #%python-code
  (lambda (stx)
    (define forms (cdr (syntax->list stx)))
    ;; LE --check-scope--> LE --make-explicit--> LL --lift--> LB --render--> Python
    (datum->syntax stx
                   (render-program
                    (lift-program (explicit-program (check-scope-program forms)))))))

(provide #%python-code)
