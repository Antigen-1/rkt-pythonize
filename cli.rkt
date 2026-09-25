#lang racket/base

;; raco rkt-pythonize [-o <output>] <module>
;;
;; A module in #lang rkt-pythonize compiles itself, so this only asks it for its
;; python-code and writes it out.

(require racket/cmdline
         racket/file
         racket/path
         racket/port)

(provide run-cli)

(define program-name "rkt-pythonize")

(define (run-cli args)
  (define output #f)
  (define given '())
  (command-line
   #:program (string-append "raco " program-name)
   #:argv (list->vector args)
   #:once-each
   [("-o" "--output") path "write the Python program to <output> (`-' for stdout)"
                       (set! output path)]
   #:args files (set! given files))
  (cond
    [(null? given)
     (eprintf "~a: give me a module in #lang rkt-pythonize~n" program-name) 1]
    [(> (length given) 1)
     (eprintf "~a: one module at a time~n" program-name) 1]
    [else
     (define file (car given))
     (define target
       (cond [output output]
             [else (path->string (path-replace-extension (string->path file) #".py"))]))
     (with-handlers ([exn:fail?
                      (lambda (e) (eprintf "~a: ~a~n" program-name (exn-message e)) 1)])
       (define python
         (dynamic-require `(file ,(path->string (path->complete-path (string->path file))))
                          'python-code))
       (if (string=? target "-")
           (display python)
           (display-to-file python (string->path target) #:exists 'replace))
       0)]))

(module+ main
  (exit (run-cli (vector->list (current-command-line-arguments)))))
