#lang racket/base
(require racket/list racket/match)
(provide expand-defines)

(define (flatten-begins body)
  (foldr (lambda (e acc)
           (if (and (pair? e) (eq? (car e) 'begin))
               (append (flatten-begins (cdr e)) acc)
               (cons e acc)))
         null
         body))

(define (expand-body-defines body)
  (define flattened (flatten-begins body))
  (define-values (defs rest)
    (partition
     (lambda (e)
       (and (pair? e) (eq? (car e) 'define)))
     flattened))
  (if (null? defs)
      (map expand-defines flattened)
      (let* ((id+vals
              (map normalize-define defs))
             (rest-processed (expand-body-defines rest))
             (bindings (map (lambda (p) (list (car p) (cadr p))) id+vals)))
        (list `(letrec ,bindings ,(cons 'begin rest-processed))))))

(define (normalize-define d)
  (match d
    [`(define (,name ,params* ...) ,body* ...)
     (list name `(lambda (,@params*) ,@(expand-body-defines body*)))]
    [`(define ,name ,val)
     (list name (expand-defines val))]
    [_ (error 'expand-defines "invalid define: ~a" d)]))

(define (expand-cond-clause clause)
  (match clause
    [`(else ,body* ...)
     `(else ,@(expand-body-defines body*))]
    [`(,test ,body* ...)
     `(,(expand-defines test) ,@(expand-body-defines body*))]
    [else (error 'expand-defines "invalid cond clause: ~a" clause)]))

(define (expand-defines sexp)
  (match sexp
    [(or `(lambda ,params ,body* ...)
         `(λ ,params ,body* ...))
     `(lambda ,params ,@(expand-body-defines body*))]
    [`(let/cc ,x ,body* ...)
     `(let/cc ,x ,@(expand-body-defines body*))]
    [`(with-handler ,handler ,body* ...)
     `(with-handler ,(expand-defines handler) ,@(expand-body-defines body*))]
    [`(let ((,x* ,e*) ...) ,body* ...)
     (define bindings (map list x* (map expand-defines e*)))
     `(let ,bindings ,@(expand-body-defines body*))]
    [`(letrec ((,x* ,e*) ...) ,body* ...)
     (define bindings (map list x* (map expand-defines e*)))
     `(letrec ,bindings ,@(expand-body-defines body*))]
    [`(let* ((,x* ,e*) ...) ,body* ...)
     (define bindings (map list x* (map expand-defines e*)))
     `(let* ,bindings ,@(expand-body-defines body*))]
    [`(let ,loop ((,x* ,e*) ...) ,body* ...)
     (define bindings (map list x* (map expand-defines e*)))
     `(let ,loop ,bindings ,@(expand-body-defines body*))]
    [`(begin ,body* ...)
     `(begin ,@(expand-body-defines body*))]
    [`(cond ,clauses ...)
     `(cond ,@(map expand-cond-clause clauses))]
    [`(if ,e0 ,e1 ,e2)
     `(if ,(expand-defines e0) ,(expand-defines e1) ,(expand-defines e2))]
    [`(if ,e0 ,e1)
     `(if ,(expand-defines e0) ,(expand-defines e1))]
    [`(,f ,args* ...)
     `(,(expand-defines f) ,@(map expand-defines args*))]
    [_ sexp]))
