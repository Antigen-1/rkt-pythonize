#lang racket/base
(require "base.rkt" "assignment.rkt" "check-uniquification.rkt" racket/runtime-path racket/file racket/contract json)
(provide (contract-out (compile-L0 (-> any/c path-string? #:raw? boolean? any)))
         current-primitives
         (rename-out (parse-LS parse-L0)
                     (unparse-LS unparse-L0)
                     (LS L0))
         py-lib-string
         primitives)

(define-runtime-path core-py "core.py")
(define py-lib-string (file->string core-py))

(define render-L0
  (compose1 render-LB check-uniquification expand-assignments))

(define (compile-L0 code dest #:raw? raw?)
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
