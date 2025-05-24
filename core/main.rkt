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
