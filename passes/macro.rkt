#lang racket/base

;; LM -> LB: macros.
;;
;; LM is LB plus `defmacro`:
;;
;;   (defmacro (name param ...) e)          fixed arity
;;   (defmacro (name param ... . rest) e)   the rest of the forms become a list
;;
;; A macro signature is a procedure signature: LB's `define` takes the same two
;; shapes, so the definition of the macro procedure is the same definition.
;;
;; A macro is an ordinary procedure: its parameters are bound to the
;; *unevaluated* argument forms, and it returns a new form.  A form is data --
;; a symbol is an interned `Symbol`, a list is a Python list -- so a macro body
;; destructures and builds forms with the Python operations it already has, and
;; `gensym` is there when it needs a name that cannot be captured.
;;
;; The pass lowers a macro definition to the `define` of that procedure, and a
;; macro call to
;;
;;   (eval '<the call form>)
;;
;; so the expansion happens in the generated program: its `eval` compiles the
;; form, calls the macro through `apply` (or `keyword-apply`, for a body that
;; passes keyword arguments on), and compiles what the macro returns.  The
;; program carries `_macro_signatures`, the table of macro names and arities
;; that `eval` needs to expand a call a macro builds while it runs.

(require nanopass/base
         racket/list
         racket/match
         "../core/base.rkt"
         "make-explicit.rkt")

(provide LM
         parse-LM
         unparse-LM
         macro-signature?
         macro-definitions
         expand-macros)

;; ---------------------------------------------------------------------------
;; The language
;; ---------------------------------------------------------------------------

;; A macro signature is the signature of a procedure: (name param ...) or
;; (name param ... . rest).
(define macro-signature? procedure-signature?)

(define-language LM
  (extends LE)
  (terminals
   (+ (macro-signature (sig))))
  (Expr (e body)
        (+ (defmacro sig e))))

(define-parser parse-LM LM)

;; ---------------------------------------------------------------------------
;; The pass
;; ---------------------------------------------------------------------------

(define registry-name '_macros)

;; Expand the macros of an LM program, giving an LE program.
(define (expand-macros program)
  (define data (unparse-LM program))
  (define macros (macro-definitions data))
  (parse-LE (expand-program data macros)))

;; Every macro the program defines.  Macros live at the top level -- a `begin`
;; chain -- because the macro procedure has to be a global for the generated
;; program's `eval` to find it by name.
(define (macro-definitions program)
  (sort (for/list ([form (in-list (top-level-forms program))]
                   #:when (match form [(list 'defmacro _ _) #t] [_ #f]))
          (car (cadr form)))
        (lambda (a b) (string<? (symbol->string a) (symbol->string b)))))

(define (top-level-forms program)
  (match program
    [(list 'begin form ...) (append* (map top-level-forms form))]
    [_ (list program)]))

(define (expand-program program macros)
  (define body (expand-form program macros #t))
  (if (null? macros)
      body
      ;; the table comes first: a macro call at the top level runs immediately
      (list 'begin (macro-registry macros) body)))

;; The table the Python-side `eval` reads: the names of the program's macros.
(define (macro-registry macros)
  (list 'define registry-name (list 'quote macros)))

(define (expand-form e macros top?)
  (cond
    [(pair? e)
     (define op (car e))
     (cond
       ;; a quoted datum is data, never code: nothing inside it is expanded
       [(eq? op 'quote) e]
       [(and (symbol? op) (memq op macros)) (expand-macro-call e)]
       [(eq? op 'defmacro)
        (unless top?
          (error 'expand-macros "defmacro is only allowed at the top level: ~a" e))
        ;; the signature -- dot and all -- is the signature of the procedure
        (list 'define (cadr e) (expand-form (caddr e) macros #f))]
       [(eq? op 'define)
        ;; the binding is not code, so it is left as it is; the bodies are
        (cons 'define (cons (cadr e) (expand-forms (cddr e) macros #f)))]
       [(eq? op 'set!)
        (match e [(list 'set! name value) (list 'set! name (expand-form value macros #f))])]
       [(eq? op 'raise)
        (match e [(list 'raise value) (list 'raise (expand-form value macros #f))])]
       [(eq? op 'with-handler)
        (cons 'with-handler (expand-forms (cdr e) macros #f))]
       [(eq? op 'trampoline) (cons 'trampoline (expand-forms (cdr e) macros #f))]
       [(eq? op 'if) (cons 'if (expand-forms (cdr e) macros #f))]
       [(eq? op 'begin) (cons 'begin (expand-forms (cdr e) macros top?))]
       [else (cons (expand-form op macros #f) (expand-forms (cdr e) macros #f))])]
    [else e]))

(define (expand-forms forms macros top?)
  (for/list ([form (in-list forms)]) (expand-form form macros top?)))

;; A macro call keeps its argument forms exactly as they were written: the
;; generated program hands the whole call to `eval`, and the macro decides what
;; to do with each form.
(define (expand-macro-call form)
  (list 'eval (list 'quote form)))
