#lang racket/base
(require "L0.rkt" racket/runtime-path racket/file racket/contract json)
(provide (contract-out (generate-python-file (-> any/c path-string? #:raw? boolean? any)))
         parse-L0 unparse-L0 L0
         py-lib-string
         primitives)

(define-runtime-path core-py "core.py")
(define py-lib-string (file->string core-py))

(define (generate-python-file code dest #:raw? raw?)
  (call-with-output-file
    #:exists 'truncate/replace
    dest
    (lambda (out)
      (if raw?
          (displayln (jsexpr->string (render-L0 code) #:null 'none) out)
          (begin
            (write-string py-lib-string out)
            (newline out)
            (displayln (format "run(~s)" (jsexpr->string (render-L0 code) #:null 'none)) out))))))
