#lang racket/base
(require "L0.rkt" racket/runtime-path racket/file racket/contract json)
(provide (contract-out (generate-python-file (-> any/c path-string? any)))
         parse-L0 unparse-L0 L0)

(define-runtime-path core-py "core.py")
(define py-lib-string (file->string core-py))

(define (generate-python-file code dest)
  (call-with-output-file
    #:exists 'truncate/replace
    dest
    (lambda (out)
      (write-string py-lib-string out)
      (newline out)
      (displayln (format "run(~s)" (jsexpr->string (render-L0 code))) out))))

(module* test racket/base
  (require rackunit racket/system racket/file racket/port (submod ".."))
  (define python-exe (find-executable-path "python"))
  (let ((temp (make-temporary-file)))
    (generate-python-file
     (parse-L0 '((lambda (x)
                   (if (get-attribute x '"False")
                       none
                       ((get-attribute x '"print")
                        '"ok")))
                 (dynamic-require '"builtins" none)))
     temp)
    (check-equal?
     (with-output-to-string
       (lambda () (system* python-exe temp)))
     "ok\n"))
  (let ((temp (make-temporary-file)))
    (generate-python-file
     (parse-L0 '((lambda (x)
                   ((lambda (print)
                      (if (get-attribute x '"True")
                          (begin
                            (set! x '"1")
                            (print x))
                          none))
                    (get-attribute x '"print")))
                 (dynamic-require '"builtins" none)))
     temp)
    (check-equal?
     (with-output-to-string
       (lambda ()
         (system* python-exe temp)))
     "1\n")))
