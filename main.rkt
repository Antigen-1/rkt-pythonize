#lang racket/base

(module+ test
  (require rackunit racket/port racket/system racket/file))

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

(require "core/main.rkt" "passes/uniquify.rkt" "passes/explicit.rkt")

(define (generate code dest)
  ((compose1
    (lambda (code) (generate-python-file code dest))
    uniquify
    make-explicit
    parse-L2)
   code))

(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (define python-exe (find-executable-path "python"))
  (define (test code output)
    (let ((temp (make-temporary-file)))
    (generate code temp)
    (check-equal?
     (with-output-to-string
       (lambda ()
         (system* python-exe temp)))
     output)))

  ;; Uniquify
  (test '((lambda (mod)
            ((lambda (apply)
               (apply '1))
             (get-attribute mod '"print")))
          (dynamic-require '"builtins" none))
        "1\n")
  ;; Explicit
  (test '((lambda (mod)
            ((lambda (print)
               '2
               (print '1))
             (get-attribute mod '"print")))
          (dynamic-require '"builtins" none))
        "1\n")
  )

(module+ main
  ;; (Optional) main submodule. Put code here if you need it to be executed when
  ;; this file is run using DrRacket or the `racket` executable.  The code here
  ;; does not run when this file is required by another module. Documentation:
  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29

  (require racket/cmdline racket/contract racket/file raco/command-name)
  (define dest (box #f))
  (command-line
    #:program (short-program+command-name)
    #:once-each
    [("-o" "--output") o "Where to write generated python code" (set-box! dest o)]
    #:args (source0 . sources)
    (define/contract dest-path path-string? (unbox dest))
    (generate
     (cons 'begin (map file->value (cons source0 sources)))
     dest-path)))
