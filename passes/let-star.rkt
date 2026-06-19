#lang racket/base
(require racket/match)
(provide expand-let*)

(define (expand-let* expr)
  (match expr
    [`(let* () ,body* ...)
     `(begin ,@(map expand-let* body*))]
    [`(let* ((,x ,e) ,rest ...) ,body* ...)
     `(let ((,x ,(expand-let* e)))
        ,(expand-let* `(let* ,rest ,@body*)))]
    [(? pair? p)
     (map expand-let* p)]
    [_ expr]))
