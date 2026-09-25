#lang racket/base

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included LICENSE-MIT and LICENSE-APACHE files.
;; If you would prefer to use a different license, replace those files with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here

(require "core/main.rkt" "passes/uniquify.rkt" "passes/explicit.rkt" "passes/cps.rkt" "passes/quote.rkt" "passes/let.rkt"
         "passes/named-let.rkt" "passes/cond.rkt" "passes/chain.rkt" "passes/vm.rkt"
         "passes/stream.rkt" "passes/more-cond.rkt" "passes/cond-explicit.rkt"
         "passes/partial-evaluate.rkt" "passes/L0-uniquify.rkt" "passes/handler.rkt" "passes/main.rkt"
         racket/contract racket/file)
(provide L parse-L unparse-L current-primitives py-lib-string
         (contract-out (rename compile compile-scheme-code
                               (->* (any/c)
                                    (#:opt? boolean?)
                                    any))))

(define py-lib-string (file->string core-py))

(define (repeat-pass n p e)
  (let loop ((n n) (e e))
    (if (= n 0)
        e
        (loop (- n 1) (p e)))))

(define (compile code #:opt? (opt? #t))
  ((compose1
    compile-L0
    L0-uniquify
    cps
    (lambda (e) 
      (if opt?
          (repeat-pass 5 partial-evaluate e)
          e))
    uniquify
    make-explicit
    add-quote
    expand-let
    expand-named-let
    expand-cond
    expand-chain
    expand-vm
    expand-stream
    expand-more-cond
    make-cond-explicit
    expand-exn-handler
    expand-defines
    parse-L)
   code))

(module+ main
  ;; (Optional) main submodule. Put code here if you need it to be executed when
  ;; this file is run using DrRacket or the `racket` executable.  The code here
  ;; does not run when this file is required by another module. Documentation:
  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29

  (require racket/cmdline racket/match racket/list racket/system racket/pretty raco/command-name)
  (define dest (box #f))
  (define json? (box #f))
  (define python (box (or (cond ((getenv "PYTHON_EXE") => find-executable-path) (else #f))
                          (find-executable-path "python3") 
                          (find-executable-path "python"))))

  (define (execute exe code (form 'unknown))
    (cond ((system* exe core-py code) => void)
          (else (raise-user-error 'rkt-pythonize "Fail to run the scheme code:\n~a" (pretty-format #:mode 'write form)))))

  (command-line
    #:program (short-program+command-name)
    #:once-each
    [("-o" "--output") o "Where to write generated code" (set-box! dest o)]
    [("-p" "--python") py "Set the python executable" (set-box! python (find-executable-path py))]
    [("-j" "--json") "Recognize supplied files as json codes" (set-box! json? #t)]
    #:ps
    "When -j/--json is not provided:"
    "If -o/--ouput is provided, json codes will be saved to the specified file."
    "Otherwise, json codes will be evaluated directly."
    "When -j/--json is provided:"
    "Exactly one json file should be provided and will then be executed."
    #:args files
    (define/contract python-exe 
        path-string?
        (unbox python))
    (match* (files json?)
      (((list source0 sources ...) (box #f))
       (define dest-path (unbox dest))

       (define form
          (cons 'begin
            (append*
             (map
               (lambda (source)
                 (call-with-input-file
                   source
                   (lambda (in)
                     (let loop ()
                       (define v (read in))
                       (if (eof-object? v)
                           null
                           (cons v (loop)))))))
               (cons source0 sources)))))
       (define compiled (compile form))

       (if dest-path
           (call-with-output-file dest-path #:exists 'truncate/replace (lambda (out) (write-string compiled out)))
           (execute python-exe compiled form)))
      (((list json) (box #t))
       (execute python-exe (file->string json)))
      ((files json?)
       (raise-user-error 'rkt-pythonize "Malformed arguments:\n\tfiles: ~s\n\tjson?: ~s" files (unbox json?))))))

