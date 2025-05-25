#lang racket/base

(module+ test
  (require rackunit racket/port racket/system racket/file racket/pretty))

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
         "passes/named-let.rkt")

(define (generate code dest #:raw? (raw? #f))
  ((compose1
    (lambda (code) (generate-python-file code dest #:raw? raw?))
    cps
    uniquify
    make-explicit
    add-quote
    expand-let
    expand-named-let
    parse-L6)
   code))

(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (define python-exe (find-executable-path "python"))
  (define (test code output)
    (pretty-write code)
    (let ((temp (make-temporary-file)))
      (generate code temp)
      (check-equal?
       (time
        (with-output-to-string
          (lambda ()
            (check-true (system* python-exe temp)))))
       output)
      (delete-directory/files temp)))

  ;; Uniquify
  (test '((lambda (mod)
            ((lambda (none)
               (vm-apply none '(1)))
             (get-attribute mod '"print")))
          (dynamic-require '"builtins" none))
        "1\n")
  ;; Explicit
  (test '((lambda (mod)
            ((lambda (print)
               '2
               (vm-apply print '(1)))
             (get-attribute mod '"print")))
          (dynamic-require '"builtins" none))
        "1\n")
  ;; CPS
  (test '(let/cc cc
           ((lambda (mod)
              ((lambda (print)
                 (vm-apply print '("1"))
                 (cc (vm-apply print '("2")))
                 (vm-apply print '("3")))
               (get-attribute mod '"print")))
            (dynamic-require '"builtins" none)))
        "1\n2\n")
  ;; Quote
  (test '2 "")
  (test '#f "")
  (test '"" "")
  ;; Let & Letrec
  (test '(let ((mod (dynamic-require "builtins" none)))
           (let ((print (get-attribute mod "print")))
             (vm-apply print '("1"))))
        "1\n")
  (test '(let ((mod (dynamic-require "builtins" none))
               (box #f))
           (let ((print (get-attribute mod "print")))
             (letrec ((proc
                       (lambda ()
                         (if box
                             (vm-apply print '("-1"))
                             (begin
                               (vm-apply print '("1"))
                               (set! box #t)
                               (proc))))))
               (proc))))
        "1\n-1\n")
  ;; Ref & Set
  (test '(let ((mod (dynamic-require "builtins" none)))
           (let ((print (get-attribute mod "print")))
             (vm-apply print (@ '(("1")) 0))))
        "1\n")
  (test '(let ((mod (dynamic-require "builtins" none)))
           (let ((print (get-attribute mod "print"))
                 (l '("1")))
             (! l 0 "2")
             (vm-apply print l)))
        "2\n")
  ;; Fibonacci
  (test '(let ((mod (dynamic-require "builtins" none)))
           (let ((print
                  (lambda (v)
                    (let ((l '()))
                      (<! l v)
                      (vm-apply (get-attribute mod "print") l)))))
             (letrec ((f 0)
                      (s 1)
                      (proc
                       (lambda (n)
                         (if (equal? n 0)
                             none
                             (let ((r (+ f s)))
                               (print f)
                               (set! f s)
                               (set! s r)
                               (proc (- n 1)))))))
               (proc 10))))
        "0\n1\n1\n2\n3\n5\n8\n13\n21\n34\n")
  ;; Named let
  (test '(let ((mod (dynamic-require "builtins" none)))
           (let ((print (lambda (v)
                          (let ((l '()))
                            (<! l v)
                            (vm-apply (get-attribute mod "print") l)))))
             (let loop ((n 10) (r 0))
               (if (equal? n 0)
                   (print r)
                   (loop (- n 1) (+ n r))))))
        "55\n")
  )

(module+ main
  ;; (Optional) main submodule. Put code here if you need it to be executed when
  ;; this file is run using DrRacket or the `racket` executable.  The code here
  ;; does not run when this file is required by another module. Documentation:
  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29

  (require racket/cmdline racket/contract raco/command-name)
  (define dest (box #f))
  (define raw? (box #f))
  (command-line
    #:program (short-program+command-name)
    #:once-each
    [("-o" "--output") o "Where to write generated python code" (set-box! dest o)]
    [("-r" "--raw") "Enable the compiler to generate raw json syntax tree" (set-box! raw? #t)]
    [("-c" "--core") "Display the core evaluator through standard output" (displayln py-lib-string)]
    #:args (source)
    (define/contract dest-path path-string? (unbox dest))
    (define raw?-bool (unbox raw?))
    (generate
     #:raw? raw?-bool
     (cons 'begin
           (call-with-input-file
             source
             (lambda (in)
               (let loop ()
                 (define v (read in))
                 (if (eof-object? v)
                     null
                     (cons v (loop)))))))
     dest-path)))
