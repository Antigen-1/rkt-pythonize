#lang racket/base

;; LE -> LE, expanding the macros a defmacro defined, and leaving the rest of
;; the program as it is.
;;
;; defmacro binds its name to a transformer with define-syntax, so a macro is a
;; syntax binding: visible exactly where the name is bound -- after the defmacro
;; in its own module, or through a require of the module that wrote it -- and
;; nowhere else.  The transformer runs at compile time on the argument forms as
;; data and returns the form that takes their place; it is found through
;; syntax-local-value, which is a pure expander lookup (no registry, no eval, no
;; dynamic-require), so it behaves the same compiled or from source.  There is
;; no hygiene: the expansion is re-syntaxed where it is used.
;;
;; A macro may live in a space, bound under the space and the name joined by a
;; dot: (defmacro #:space a (twice x) ...) binds a.twice.  A form says which
;; space it means with (#:space key name arg ...), which expands that macro call
;; and its subforms in that space; #%python-code takes the same keyword to set
;; the space of a whole body, and (#:space #f ...) is the plain name inside a
;; spaced body.  Without a space a name is looked up plainly, as it always was.
;;
;; This pass runs first, so a macro may expand into anything LE has, cond
;; included, and into another macro.

(require racket/list)

(provide expand-macro-program
         le-macro
         le-macro-proc)

;; what defmacro binds: the transformer, marked so that this pass only expands
;; what defmacro made -- an ordinary macro or a phase-1 procedure that happens
;; to share the name is left for the checks to talk about.  It is applicable so
;; that define-syntax accepts it, and using the name in Racket code says why.
;; The space is #f for a macro with a plain name and the key a defmacro was
;; given with #:space otherwise, so an error can name the macro in full.
(struct le-macro (space proc)
  #:property prop:procedure
  (lambda (self stx)
    (raise-syntax-error 'defmacro
                        "this macro is for LE: use it inside #%python-code"
                        stx)))

;; a macro that expands into itself has to stop somewhere
(define expansion-limit 1000)

(define (head stx) (and (pair? (syntax->list stx)) (syntax-e (car (syntax->list stx)))))
(define (mk stx datum) (datum->syntax stx datum))

(define (form-location stx)
  (define line (syntax-line stx))
  (if line
      (format "~a:~a:~a" (syntax-source stx) line (add1 (syntax-column stx)))
      "?"))

(define (not-le stx message)
  (error 'expand-macro "~a: ~a: ~a" (form-location stx) message (syntax->datum stx)))

;; space is the space the whole body is in: the key #%python-code was given, or
;; #f for the plain names
(define (expand-macro-program forms [space #f])
  (ensure-expanding!)
  (define budget (box expansion-limit))
  (define expanded (for/list ([f (in-list forms)]) (expand f space budget)))
  (check-no-space-left expanded)
  expanded)

;; syntax-local-value is only legal while a program is being expanded, and that
;; is where this pass runs: inside the transformer of #%python-code.  Say so
;; rather than fall over if it is ever called from somewhere else.
(define (ensure-expanding!)
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (error 'expand-macro
                            "a macro is expanded while a program is compiled, and this is not a compilation"))])
    (syntax-local-context)
    (void)))

(define (expand stx space budget)
  (cond [(not (syntax->list stx)) stx]
        [else
         (define parts (syntax->list stx))
         (cond
           ;; (#:space key name arg ...): this macro call, and the form it
           ;; expands to, are in that space, whatever the body's space is
           [(space-form? parts) (expand-spaced stx parts budget)]
           [else
            (define name (and (pair? parts) (identifier? (car parts)) (car parts)))
            (define macro (and name (macro-in-space name space)))
            (cond
              [macro (expand (apply-macro name macro (cdr parts) stx budget) space budget)]
              [else (walk stx parts space budget)])])]))

(define (expand-spaced stx parts budget)
  (unless (pair? (cdr parts))
    (not-le stx "a #:space form names a space and a macro: (#:space key name arg ...)"))
  (define rest (cddr parts))
  (unless (= 1 (length rest))
    (not-le stx "a #:space form names one macro: (#:space key name arg ...)"))
  (define space (syntax->datum (cadr parts)))
  (define form (car rest))
  (define form-parts (syntax->list form))
  (unless (and form-parts (pair? form-parts) (identifier? (car form-parts)))
    (not-le stx "a #:space form names a macro: (#:space key name arg ...)"))
  (define name (car form-parts))
  (define macro (macro-in-space name space))
  (unless macro
    (not-le stx (if space
                    (format "no macro named ~a in space ~a" (syntax-e name) space)
                    (format "no macro named ~a" (syntax-e name)))))
  (expand (apply-macro name macro (cdr form-parts) stx budget) space budget))

;; the traversal for everything that is not a macro call; a form's subforms are
;; in the same space as the form
(define (walk stx parts space budget)
  (case (head stx)
    [(quote) stx]
    ;; a parameter list is not a form, even when it holds a macro's name
    [(lambda)
     (if (< (length parts) 2)
         stx
         (mk stx (list* 'lambda (cadr parts)
                        (for/list ([f (in-list (cddr parts))]) (expand f space budget)))))]
    [(define)
     (cond [(< (length parts) 3) stx] ; check-expression says what is wrong
           [(identifier? (cadr parts))
            (mk stx (list 'define (cadr parts) (expand (caddr parts) space budget)))]
           [else
            ;; a signature is not a form either
            (mk stx (list* 'define (cadr parts)
                           (for/list ([f (in-list (cddr parts))]) (expand f space budget))))])]
    [else (mk stx (for/list ([p (in-list parts)]) (expand p space budget)))]))

;; the marker: (#:space key name arg ...)
(define (space-form? parts)
  (and (pair? parts) (keyword? (syntax-e (car parts))) (eq? (syntax-e (car parts)) '#:space)))

;; the transformer a name means in a space: space.name, or the plain name when
;; there is no space
(define (macro-in-space id space)
  (macro-of (if space (qualified-name id space) id)))

;; the binding a spaced macro is stored under: space and name joined by a dot,
;; synthesized from the use site's identifier so it resolves where the use is
(define (qualified-name id space)
  (datum->syntax id (string->symbol (format "~a.~a" space (syntax-e id)))))

;; the transformer a name is bound to, when it was a defmacro that bound it
(define (macro-of id)
  (define value (syntax-local-value id (lambda () #f)))
  (and (le-macro? value) value))

;; what an error calls the macro: a spaced macro by its full name
(define (macro-label id macro)
  (define space (le-macro-space macro))
  (if space (format "~a.~a" space (syntax-e id)) (format "~a" (syntax-e id))))

(define (apply-macro id macro args stx budget)
  (spend! stx budget)
  (define proc (le-macro-proc macro))
  (define given (length args))
  (unless (procedure-arity-includes? proc given)
    (not-le stx (format "the macro ~a takes ~a, and is given ~a argument~a"
                        (macro-label id macro) (arity-text (procedure-arity proc))
                        given (if (= given 1) "" "s"))))
  (define result
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (not-le stx (format "the macro ~a failed: ~a"
                                           (macro-label id macro) (exn-message e))))])
      (apply proc (for/list ([a (in-list args)]) (syntax->datum a)))))
  (cond [(form-datum? result) (mk stx result)]
        [(syntax? result)
         (not-le stx (format "the macro ~a returned a syntax object: a defmacro returns the form to use as data"
                             (macro-label id macro)))]
        [else
         (not-le stx (format "the macro ~a expanded into ~s, which is not a form"
                             (macro-label id macro) result))]))

;; each expansion spends one from the budget, so a macro that expands into
;; itself stops with an error instead of running forever
(define (spend! stx budget)
  (set-box! budget (sub1 (unbox budget)))
  (when (< (unbox budget) 0)
    (not-le stx (format "more than ~a macro expansions: a macro probably expands into itself"
                        expansion-limit))))

(define (arity-text arity)
  (define (arguments n) (format "~a argument~a" n (if (= n 1) "" "s")))
  (cond [(integer? arity) (arguments arity)]
        [(arity-at-least? arity) (format "at least ~a" (arguments (arity-at-least-value arity)))]
        [else "a fixed number of arguments"]))

;; what a transformer may return: a name, a literal, or a list of them
(define (form-datum? datum)
  (or (symbol? datum) (boolean? datum) (number? datum) (string? datum)
      (char? datum) (bytes? datum) (null? datum) (pair? datum)
      (vector? datum) (hash? datum)))

;; a #:space form is a macro call, and this pass is the only one that knows what
;; that means: nothing may reach expand-cond or the checks with one left.  This
;; also catches one in a place that is not a form, such as a parameter list.
(define (check-no-space-left forms)
  (for ([f (in-list forms)]) (check-form f)))

(define (check-form stx)
  (cond [(and (keyword? (syntax-e stx)) (eq? (syntax-e stx) '#:space))
         (not-le stx "a #:space marker has to start a macro call: (#:space key name arg ...)")]
        [(not (syntax->list stx)) (void)]
        [else
         (define parts (syntax->list stx))
         (cond [(eq? (head stx) 'quote) (void)]
               [(space-form? parts)
                (not-le stx "a #:space form is a macro call: (#:space key name arg ...)")]
               [else (for ([p (in-list parts)]) (check-form p))])]))
