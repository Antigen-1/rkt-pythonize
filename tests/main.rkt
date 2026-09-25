#lang racket/base

;; Tests for the command line entry point: it reads an LB program from a file or
;; from the standard ports, and writes the Python program to a file or to
;; standard output.

(require rackunit
         racket/file
         racket/path
         racket/port
         racket/string
         "../main.rkt"
         "utilities.rkt")

(module+ test
  (define directory (make-temporary-file "rkt-pythonize~a" 'directory))

  (define (write-source name source)
    (define path (build-path directory name))
    (display-to-file source path #:exists 'replace)
    path)

  ;; Run the command line with `args`, with `input` on stdin, and return the
  ;; exit code together with what it wrote to stdout and stderr.
  (define (cli args [input ""])
    (define out (open-output-string))
    (define err (open-output-string))
    (define code
      (parameterize ([current-input-port (open-input-string input)]
                     [current-output-port out]
                     [current-error-port err])
        (run-cli args)))
    (values code (get-output-string out) (get-output-string err)))

  (test-case "an input file is transpiled to a .py file next to it"
    (define source (write-source "hello.lb" "(print \"hello\")\n"))
    (define-values (code out err) (cli (list (path->string source))))
    (check-equal? code 0 "exit code")
    (check-equal? out "" "nothing on stdout")
    (check-equal? err "" "nothing on stderr")
    (define target (path-replace-extension source #".py"))
    (check-true (file-exists? target) "the .py file was written")
    (check-equal? (file->string target) (transpile "(print \"hello\")\n"))
    (define-values (py-out py-err py-status) (run-python (file->string target)))
    (check-equal? py-status #t (format "python failed: ~a" py-err))
    (check-equal? py-out "hello\n"))

  (test-case "-o chooses the output file"
    (define source (write-source "twice.lb" "(print (* 2 21))\n"))
    (define target (build-path directory "chosen.py"))
    (define-values (code out err)
      (cli (list "-o" (path->string target) (path->string source))))
    (check-equal? code 0 (format "exit code, stderr: ~a" err))
    (check-equal? out "")
    (check-true (file-exists? target))
    (check-equal? (file->string target) (transpile "(print (* 2 21))\n")))

  (test-case "stdin goes to stdout"
    (define-values (code out err) (cli '() "(print (+ 1 2))\n"))
    (check-equal? code 0 (format "exit code, stderr: ~a" err))
    (check-equal? out (transpile "(print (+ 1 2))\n"))
    (check-equal? err "")
    (define-values (code2 out2 err2) (cli '("-o" "-") "(print (+ 1 2))\n"))
    (check-equal? code2 0 (format "exit code, stderr: ~a" err2))
    (check-equal? out2 out))

  (test-case "bad command lines fail with a usage message"
    (define-values (code out err) (cli (list "one.lb" "two.lb")))
    (check-equal? code 1)
    (check-equal? out "")
    (check-true (string-contains? err "expected at most one input file"))
    (check-true (string-contains? err "usage: raco rkt-pythonize"))
    ;; an option after the input file is the common mistake, so it gets a hint
    (define-values (code2 out2 err2) (cli (list "one.lb" "-o" "two.py")))
    (check-equal? code2 1)
    (check-true (string-contains? err2 "options come before the input file")))

  (test-case "an unreadable or unparsable input fails"
    (define-values (code out err)
      (cli (list (path->string (build-path directory "absent.lb")))))
    (check-equal? code 1)
    (check-true (string-contains? err "rkt-pythonize:"))
    (define source (write-source "bad.lb" "()\n"))
    (define-values (code2 out2 err2) (cli (list (path->string source))))
    (check-equal? code2 1)
    (check-true (string-contains? err2 "rkt-pythonize:"))))
