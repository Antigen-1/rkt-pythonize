#lang racket/base

;; LE -> LE, expanding cond into the ifs and begins it means, and leaving the
;; rest of the program as it is.
;;
;; (cond [test body ...] ... [else body ...]) is the first clause whose test is
;; true, with its body as an implicit begin, and the clauses after it in the
;; else position: a clause body of several forms becomes a begin, and a single
;; form is already the body.  else is a keyword, and only legal as the last
;; clause's test; a cond that runs out of clauses raises, as Racket's does.
;;
;; This pass runs first, so the checks after it only ever see if and begin.

(require racket/list)

(provide expand-cond-program)

(define (head stx) (and (pair? (syntax->list stx)) (syntax-e (car (syntax->list stx)))))
(define (mk stx datum) (datum->syntax stx datum))

(define (form-location stx)
  (define line (syntax-line stx))
  (if line
      (format "~a:~a:~a" (syntax-source stx) line (add1 (syntax-column stx)))
      "?"))

(define (not-le stx message)
  (error 'expand-cond "~a: ~a: ~a" (form-location stx) message (syntax->datum stx)))

(define (expand-cond-program forms)
  (for/list ([f (in-list forms)]) (expand f)))

(define (expand stx)
  (cond [(not (syntax->list stx)) stx]
        [else
         (define parts (syntax->list stx))
         (case (head stx)
           [(quote) stx]
           [(cond) (expand (cond->if stx (cdr parts)))]
           ;; a parameter list is not a form, even when it holds the name cond
           [(lambda)
            (if (< (length parts) 2)
                stx
                (mk stx (list* 'lambda (cadr parts)
                               (for/list ([f (in-list (cddr parts))]) (expand f)))))]
           [(define)
            (cond [(< (length parts) 3) stx] ; check-expression says what is wrong
                  [(identifier? (cadr parts))
                   (mk stx (list 'define (cadr parts) (expand (caddr parts))))]
                  [else
                   ;; a signature is not a form either
                   (mk stx (list* 'define (cadr parts)
                                  (for/list ([f (in-list (cddr parts))]) (expand f))))])]
           [else (mk stx (for/list ([p (in-list parts)]) (expand p)))])]))

(define (cond->if stx clauses)
  (when (null? clauses) (not-le stx "a cond needs at least one clause"))
  (clauses->if stx clauses))

(define (clauses->if stx clauses)
  (define clause (car clauses))
  (define rest (cdr clauses))
  (define parts (syntax->list clause))
  (unless parts (not-le clause "a cond clause is a [test body ...] pair"))
  (define test (car parts))
  (define body (cdr parts))
  (define else? (and (identifier? test) (eq? (syntax-e test) 'else)))
  (when (and else? (pair? rest))
    (not-le clause "else is only legal as the last cond clause"))
  (when (null? body)
    (not-le clause (if else? "an else clause needs a body" "a cond clause needs a body")))
  (when (and (identifier? (car body)) (eq? (syntax-e (car body)) '=>))
    (not-le clause "a cond clause with => is not supported"))
  (for ([f (in-list body)])
    (when (and (identifier? f) (eq? (syntax-e f) 'else))
      (not-le f "else is a cond keyword: it is only legal as the last clause's test")))
  (if else?
      (clause-body clause body)
      (mk clause
          (list 'if test (clause-body clause body)
                (if (pair? rest)
                    (clauses->if stx rest)
                    (mk stx (list 'raise "cond: no clause matched")))))))

;; the body of a clause is an implicit begin
(define (clause-body clause body)
  (if (null? (cdr body))
      (car body)
      (mk clause (list* 'begin body))))
