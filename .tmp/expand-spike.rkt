#lang racket/base
;; Spike: LB's core forms as Racket bindings, user macros as syntax-rules
;; transformers, and a whole program expanded as a module body at transpile
;; time -- deep enough that nested macros expand and hygiene is observable.

;; the namespace carries LB's own module body, which is how a #lang does it
(define ns (make-base-namespace))

(parameterize ([current-namespace ns])
  (eval '(require (for-syntax racket/base)))
  ;; the body of a program is a body, so its begin must be a real one
  (eval '(define-syntax #%module-begin (syntax-rules () [(_ e ...) (begin e ...)])))
  (eval '(begin
           ;; the forms that do not bind anything stay opaque: an application of
           ;; a core variable, which the expander leaves alone
           (define #%lb-if #f)
           (define-syntax if (syntax-rules () [(_ c t e) (#%app #%lb-if c t e)]))
           ;; the binding forms are Racket's own, so the expander knows what
           ;; they bind and hygiene has something to work with
           (define-syntax define
             (syntax-rules ()
               [(_ (f x ...) body) (define-values (f) (lambda (x ...) body))]
               [(_ n v) (define-values (n) v)]))
           ;; macros a user writes, with plain define-syntax
           (define-syntax swap
             (syntax-rules ()
               [(_ a b) (begin (define tmp a) (set! a b) (set! b tmp))])))))

;; the whole program is a module body, which is where LB's statements live
(define (expand-program . forms)
  (parameterize ([current-namespace ns])
    (expand (datum->syntax #f (cons '#%module-begin forms)))))

(define (ids-named stx name)
  (let loop ([s stx] [acc '()])
    (cond [(identifier? s) (if (eq? (syntax-e s) name) (cons s acc) acc)]
          [(syntax->list s) (for/fold ([acc acc]) ([x (in-list (syntax->list s))]) (loop x acc))]
          [else acc])))

(define (report label . forms)
  (define expanded (apply expand-program forms))
  (define tmps (reverse (ids-named expanded 'tmp)))
  (printf "~a\n  in : ~s\n  out: ~s\n  tmp ids: ~a\n\n"
          label forms (syntax->datum expanded)
          (if (null? tmps)
              "none"
              (format "~a, bound-identifier=? against the first: ~a"
                      (length tmps)
                      (map (lambda (i) (bound-identifier=? (car tmps) i)) tmps)))))

(report "core if" '(if x y z))
(report "a procedure definition" '(define (f x) (if x 1 2)))
(report "hygiene: the macro's tmp against the user's tmp"
        '(begin (define tmp 1) (swap tmp other) tmp))
