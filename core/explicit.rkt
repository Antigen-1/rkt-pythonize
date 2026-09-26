#lang racket/base

;; LE -> LL.
;;
;; LE lets a form that takes a thunk write the body out -- several forms, or a
;; single expression -- and LL makes the thunk explicit: a form that takes one
;; always gets a lambda.  Nothing else changes.

(require racket/list)

(provide explicit-program)

(define (head stx) (and (pair? (syntax->list stx)) (syntax-e (car (syntax->list stx)))))
(define (mk stx datum) (datum->syntax stx datum))
(define (statement? stx) (memq (head stx) '(define set!)))

(define (explicit-program forms)
  (for/list ([f (in-list forms)]) (explicit f)))

(define (explicit stx)
  (cond [(not (syntax->list stx)) stx]
        [else
         (define parts (syntax->list stx))
         (case (head stx)
           [(quote) stx]
           [(with-handler)
            (mk stx (list* 'with-handler (explicit (cadr parts)) (thunk (cddr parts) stx)))]
           [(trampoline)
            (mk stx (list* 'trampoline (value (cdr parts) stx)))]
           [(define)
            (define target (cadr parts))
            (if (identifier? target)
                (mk stx (list 'define target (explicit (caddr parts))))
                ;; the signature is not a form
                (mk stx (list* 'define target (for/list ([f (in-list (cddr parts))]) (explicit f)))))]
           [else (mk stx (for/list ([p (in-list parts)]) (explicit p)))])]))

;; with-handler takes a thunk, and a lone lambda already is one
(define (thunk forms stx)
  (cond [(and (null? (cdr forms)) (eq? (head (car forms)) 'lambda))
         (list (explicit (car forms)))]
        [else (list (explicit (mk stx (list* 'lambda '() forms))))]))

;; trampoline takes the function to call, so a lone expression stays as it is
(define (value forms stx)
  (cond [(and (null? (cdr forms)) (not (statement? (car forms))))
         (list (explicit (car forms)))]
        [else (list (explicit (mk stx (list* 'lambda '() forms))))]))
