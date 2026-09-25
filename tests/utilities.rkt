#lang racket/base

;; Helpers shared by the end-to-end tests.  They transpile LB source text and
;; run the result with a real Python interpreter, so the tests check what the
;; generated program does, not only what it looks like.
;;
;; The interpreter is `$TEST_PYTHON_EXE` when that is set, and otherwise
;; `python3' from PATH, or `python'.

(require rackunit
         racket/string
         racket/system
         "../main.rkt")

(provide transpile
         python-executable
         run-python
         check-python-output
         check-python-source
         check-python-contains
         check-python-failure)

;; Resolve the interpreter here: `system*` wants a program that exists, and
;; finding it once gives a better error message than a failed exec.
(define (python-executable)
  (define configured (getenv "TEST_PYTHON_EXE"))
  (define candidates (if configured (list configured) (list "python3" "python")))
  (or (for/or ([name (in-list candidates)]) (find-executable-path name))
      (raise-user-error 'python-executable
                        "no Python interpreter found (tried ~a; set TEST_PYTHON_EXE)"
                        (string-join candidates ", "))))

;; Run Python source.  Returns (values stdout stderr status), where the status
;; is #t when the interpreter exited cleanly and the exit code otherwise.
(define (run-python code)
  (define out (open-output-string))
  (define err (open-output-string))
  (define status
    (parameterize ([current-output-port out]
                   [current-error-port err])
      (system* (python-executable) "-c" code)))
  (values (get-output-string out) (get-output-string err) status))

;; Check the exact Python text generated for `source`.
(define (check-python-source source expected)
  (check-equal? (transpile source) expected))

;; Check that the Python text generated for `source` contains `needle`, which is
;; how the prelude pieces are tested.
(define (check-python-contains source needle)
  (define code (transpile source))
  (check-true (string-contains? code needle)
              (format "expected ~s in:\n~a" needle code)))

;; Check that the program `source` transpiles to prints exactly `expected`.
(define (check-python-output source expected)
  (define code (transpile source))
  (define-values (out err status) (run-python code))
  (if (eq? status #t)
      (check-equal? out expected (format "stdout of:\n~a" code))
      (fail (format "running this exited with ~a:~n~a--- generated ---~n~a--- stderr ---~n~a"
                    status source code err))))

;; Check that the program `source` transpiles to fails in Python with a message
;; matching `rx`: a `raise` that no handler catches, for example.
(define (check-python-failure source rx)
  (define-values (out err status) (run-python (transpile source)))
  (check-not-eq? status #t (format "expected a Python error for:\n~a" source))
  (check-regexp-match rx err))
