#lang racket/base

;; The second stage: the program as Python.  Every .rkt of the source
;; directory is compiled to the Python it renders and written as .py under
;; build/python, every .py is copied as it is, and a directory that is a
;; package gets an empty __init__.py if the program did not write one.

(require racket/file
         racket/list
         racket/path
         racket/string
         "project.rkt")

(provide render-project)

(define (render-project project #:quiet? [quiet? #f])
  (define python-directory (project-python-directory project))
  (when (directory-exists? python-directory)
    (delete-directory/files python-directory))
  (make-directory* python-directory)
  ;; one namespace for the program: the compiler's names are made once per
  ;; namespace, so the modules of one program agree about them
  (define namespace (make-base-namespace))
  (for ([file (in-list (project-files project))])
    (define output (project-output-file project file))
    (make-directory* (path-only output))
    (case (source-file-kind file)
      [(python)
       (copy-file (source-file-path file) output #t)]
      [(racket)
       (display-to-file (python-of-file file namespace) output #:exists 'replace)]
      [else
       (error 'build-executable "~a: unknown source kind ~a"
              (path->string (source-file-path file))
              (source-file-kind file))])
    (unless quiet?
      (printf "build: ~a -> ~a\n"
              (path->string (source-file-path file))
              (path->string output))))
  (write-initialisers python-directory)
  (project-modules project))

;; the Python a .rkt module renders: the value of its python, which
;; #%python-code makes a string
(define (python-of-file file namespace)
  (define path (source-file-path file))
  (define python
    (parameterize ([current-namespace namespace]
                   ;; a build neither reads nor writes the compiled files
                   ;; Racket would otherwise leave beside the sources
                   [use-compiled-file-paths '()])
      (dynamic-require (list 'file (path->string path)) 'python (lambda () #f))))
  (cond [(not python)
         (error 'build-executable
                "~a: every .rkt of the program provides python, the Python it renders"
                (path->string path))]
        [(not (string? python))
         (error 'build-executable
                "~a: python is ~s, not the string it renders"
                (path->string path)
                python)]
        [else python]))

;; a package is a directory with an __init__.py; one the program did not
;; write itself is empty
(define (write-initialisers root)
  (for ([dir (in-list (directories root))] #:unless (equal? dir root))
    (define init (build-path dir "__init__.py"))
    (unless (file-exists? init)
      (display-to-file "" init #:exists 'replace))))

(define (directories root)
  (sort (walk-directories root) string<? #:key path->string))

(define (walk-directories dir)
  (append*
   (cons (list dir)
         (for/list ([element (in-list (sort (with-handlers ([exn:fail? (lambda (e) '())])
                                              (directory-list dir))
                                          string<? #:key path->string))])
           (define path (build-path dir element))
           (if (directory-exists? path) (walk-directories path) '())))))
