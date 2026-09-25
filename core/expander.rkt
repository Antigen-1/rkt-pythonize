#lang racket/base

;; The front end: LB source text -> fully expanded syntax.
;;
;; LB's forms are Racket bindings in a namespace of our own, because that is
;; what buys hygiene: a macro is a Racket transformer, and a form that binds a
;; name has to be a form the expander knows binds one.
;;
;;   * the binding forms are Racket's own: `begin`, `set!`, and a `define` that
;;     becomes `define-values` (a procedure definition being `define-values`
;;     around a `lambda`);
;;   * the forms that bind nothing -- `if`, `with-handler`, `trampoline`,
;;     `raise` -- are opaque applications of a core variable, which the
;;     expander leaves alone;
;;   * a free variable comes out as `(#%top . x)`, which is exactly what a
;;     Python global is;
;;   * `defmacro` is `define-syntax` with a body that works on data: the
;;     arguments arrive as datums, the body answers a datum, and the expander
;;     gives every name it answers the macro's own scope.
;;
;; A program is a module body, so a definition may sit anywhere a statement may.

(require racket/list
         racket/port)

(provide expand-source
         expanded-ids
         core-namespace
         lb-lang-path)

(require racket/list
         racket/port
         racket/runtime-path)

;; the language a program is a module of, by absolute path
(define-runtime-path lb-lang-path "lb-lang.rkt")

(define core-namespace (make-base-namespace))

(define (read-all-syntax source)
  (define in (open-input-string source))
  (port-count-lines! in)
  (let loop ([forms '()])
    (define form (read-syntax 'lb in))
    (if (eof-object? form) (reverse forms) (loop (cons form forms)))))

;; LB source text -> the expanded program body, as syntax.  The program is a
;; module of `lb-lang`, so its forms are expanded in a body, where a definition
;; is visible to what follows it.
(define (expand-source source)
  (define program
    (format "(module lb-program (file ~s) ~a)" (path->string lb-lang-path) source))
  (parameterize ([current-namespace core-namespace])
    (module-body (expand (read-syntax 'lb (open-input-string program))))))

;; The body of an expanded module: (module name lang body ...)
(define (module-body stx)
  (define parts (syntax->list stx))
  (cond [(and parts (eq? (syntax-e (car parts)) 'module)) (cadddr parts)]
        [else stx]))

;; Every identifier named `name` in a syntax object, in the order they appear.
(define (expanded-ids stx name)
  (let loop ([s stx] [acc '()])
    (cond [(identifier? s) (if (eq? (syntax-e s) name) (cons s acc) acc)]
          [(syntax->list s)
           (for/fold ([acc acc]) ([x (in-list (syntax->list s))]) (loop x acc))]
          [else acc])))
