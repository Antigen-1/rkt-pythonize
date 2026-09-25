#lang racket/base

;; LM -> LB: macros.
;;
;; LM is LB plus `defmacro`:
;;
;;   (defmacro (name param ...) e)          fixed arity
;;   (defmacro (name param ... . rest) e)   the rest of the forms become a list
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
         "../core/base.rkt")

(provide LM
         parse-LM
         unparse-LM
         macro-signature?
         macro-definitions
         expand-macros)

;; ---------------------------------------------------------------------------
;; The language
;; ---------------------------------------------------------------------------

;; A macro signature: (name param ...) or (name param ... . rest).  Every
;; parameter is a variable, and the dotted tail is the rest parameter.
(define (macro-signature? v)
  (and (pair? v)
       (variable? (car v))
       (let loop ([params (cdr v)])
         (cond [(null? params) #t]
               [(pair? params) (and (variable? (car params)) (loop (cdr params)))]
               [else (variable? params)]))))

(define-language LM
  (extends LB)
  (terminals
   (+ (macro-signature (s))))
  (Expr (e body)
        (+ (defmacro s e))))

(define-parser parse-LM LM)

;; ---------------------------------------------------------------------------
;; Signatures
;; ---------------------------------------------------------------------------

(define (signature-name signature)
  (car signature))

;; The parameters of the macro procedure (the rest parameter is an ordinary
;; parameter: the call site packs the remaining forms into a list) and whether
;; there is one.
(define (signature-info signature)
  (let loop ([rest (cdr signature)] [params '()])
    (cond [(null? rest) (values (reverse params) #f)]
          [(pair? rest) (loop (cdr rest) (cons (car rest) params))]
          [else (values (reverse (cons rest params)) #t)])))

;; What the runtime table records for a macro: how many argument forms it takes
;; before the rest list, and whether it has one.
(define (signature-arity signature)
  (define-values (params has-rest?) (signature-info signature))
  (if has-rest? (sub1 (length params)) (length params)))

(define (signature-has-rest? signature)
  (define-values (params has-rest?) (signature-info signature))
  has-rest?)

;; ---------------------------------------------------------------------------
;; The pass
;; ---------------------------------------------------------------------------

(define registry-name '_macro_signatures)

;; Expand the macros of an LM program, giving an LB program.
(define (expand-macros program)
  (define data (unparse-LM program))
  (define macros (macro-definitions data))
  (parse-LB (expand-program data macros)))

;; Every macro the program defines.  Macros live at the top level -- a `begin`
;; chain -- because the macro procedure has to be a global for the generated
;; program's `eval` to find it by name.
(define (macro-definitions program)
  (for/fold ([macros (hash)]) ([form (in-list (top-level-forms program))])
    (match form
      [(list 'defmacro signature _)
       (hash-set macros
                 (signature-name signature)
                 (vector (signature-arity signature) (signature-has-rest? signature)))]
      [_ macros])))

(define (top-level-forms program)
  (match program
    [(list 'begin form ...) (append* (map top-level-forms form))]
    [_ (list program)]))

(define (expand-program program macros)
  (define body (expand-form program macros #t))
  (if (zero? (hash-count macros))
      body
      ;; the table comes first: a macro call at the top level runs immediately
      (list 'begin (macro-registry macros) body)))

;; The table the Python-side `eval` reads: macro name -> #(arity has-rest?).
(define (macro-registry macros)
  (list 'define registry-name (list 'quote macros)))

(define (expand-form e macros top?)
  (cond
    [(pair? e)
     (define op (car e))
     (cond
       ;; a quoted datum is data, never code: nothing inside it is expanded
       [(eq? op 'quote) e]
       [(and (symbol? op) (hash-has-key? macros op)) (expand-macro-call e)]
       [(eq? op 'defmacro)
        (unless top?
          (error 'expand-macros "defmacro is only allowed at the top level: ~a" e))
        (define-values (params has-rest?) (signature-info (cadr e)))
        (list 'define (cons (signature-name (cadr e)) params) (expand-form (caddr e) macros #f))]
       [(eq? op 'define)
        (match e
          [(list 'define (cons name params) body)
           (list 'define (cons name params) (expand-form body macros #f))]
          [(list 'define name body)
           (list 'define name (expand-form body macros #f))])]
       [(eq? op 'set!)
        (match e [(list 'set! name value) (list 'set! name (expand-form value macros #f))])]
       [(eq? op 'raise)
        (match e [(list 'raise value) (list 'raise (expand-form value macros #f))])]
       [(eq? op 'with-handler)
        (match e
          [(list 'with-handler handler body)
           (list 'with-handler (expand-form handler macros #f) (expand-form body macros #f))])]
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
