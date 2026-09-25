#lang racket/base

;; rkt-pythonize: a Lisp-to-Python transpiler.
;;
;;   raco rkt-pythonize [<file>|-] [-o <output>]
;;
;; With no <file>, or with `-', the LB program is read from stdin and the Python
;; program goes to stdout.  With a <file> the Python program is written next to
;; it with a `.py' extension, unless `-o' says otherwise.
;;
;; The module doubles as the package's main module: `racket main.rkt', the
;; `rkt-pythonize' launcher, and `raco rkt-pythonize' (see `raco-commands' in
;; info.rkt) all run the `main' submodule below.

(require racket/cmdline
         racket/file
         racket/path
         racket/port
         "core/base.rkt"
         "core/python.rkt"
         "passes/macro.rkt")

(provide run-cli
         ;; the LB language
         LB
         parse-LB
         unparse-LB
         variable?
         literal?
         datum?
         ;; macro expansion: LM -> LB
         LM
         parse-LM
         unparse-LM
         macro-signature?
         expand-macros
         ;; LB -> Python
         compile-LB
         python-name
         transpile)

;; LB source text -> Python source text.  Reading is Racket's own `read`, so
;; there is no lexer to maintain; a source file with several top-level forms
;; becomes one `(begin form ...)`, and an empty file an empty `(begin)`.
;;
;; The pipeline is: read, check as LM, expand macros into LB, compile to Python.
(define (transpile source)
  (define forms (read-forms source))
  (define program
    (cond [(null? forms) '(begin)]
          [(null? (cdr forms)) (car forms)]
          [else (cons 'begin forms)]))
  (compile-LB (expand-macros (parse-LM program))))

(define (read-forms source)
  (define in (open-input-string source))
  (let loop ([forms '()])
    (define form (read in))
    (if (eof-object? form) (reverse forms) (loop (cons form forms)))))

(define program-name "rkt-pythonize")

(define usage-text
  (string-append
   "usage: raco " program-name " [-o <output>] [<file>|-]\n"
   "\n"
   "Transpile an LB program to Python.  With no <file>, or with `-', the LB\n"
   "program is read from stdin.  The Python program is written next to <file>\n"
   "with a `.py' extension, or to <output> when that is given (`-' for stdout).\n"
   "Options come before the input file.\n"))

;; Run the command line `args' and return the exit code.
(define (run-cli args)
  (define output #f)
  (define given '())
  (command-line
   #:program (string-append "raco " program-name)
   #:argv (list->vector args)
   #:once-each
   [("-o" "--output") path "write the Python program to <output> (`-' for stdout)"
                       (set! output path)]
   #:args files
   (set! given files))
  (cond
    [(> (length given) 1)
     (eprintf "~a: expected at most one input file, got ~a~n" program-name (length given))
     (when (ormap (lambda (arg) (regexp-match? #rx"^-" arg)) given)
       (eprintf "~a: options come before the input file~n" program-name))
     (eprintf "~a" usage-text)
     1]
    [else
     (define input (if (null? given) "-" (car given)))
     (define target
       (cond [output output]
             [(string=? input "-") "-"]
             [else (path->string (path-replace-extension (string->path input) #".py"))]))
     (with-handlers ([exn:fail?
                      (lambda (e)
                        (eprintf "~a: ~a~n" program-name (exn-message e))
                        1)])
       (define source
         (if (string=? input "-")
             (port->string (current-input-port))
             (file->string input)))
       (define python (transpile source))
       (if (string=? target "-")
           (display python)
           (display-to-file python (string->path target) #:exists 'replace))
       0)]))

(module+ main
  (exit (run-cli (vector->list (current-command-line-arguments)))))
