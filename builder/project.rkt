#lang racket/base

;; A project is a directory that holds a program:
;;
;;   <project>/src/…              the program: every .rkt there is a module of
;;                                it and provides python, every .py there is a
;;                                module of it
;;   <project>/requirements.txt   the Python dependencies, if any
;;   <project>/build/             what the build writes: python/, venv/,
;;                                pyinstaller/, bin/
;;
;; This module is the first stage of the build: it reads a project and says
;; what the build will do -- which file is which Python module, what the
;; program is called, where it starts, and what Python dependencies it
;; declares -- and it refuses a project that cannot be built, before anything
;; is compiled or written.

(require racket/list
         racket/path
         racket/string)

(provide (struct-out project)
         (struct-out source-file)
         (struct-out attachment)
         (struct-out requirements)
         read-project
         project-python-directory
         project-venv-directory
         project-output-file
         project-entry-file
         project-module-path
         runtime-module)

;; a source file of the program: its path, its path under the source
;; directory, the Python module it is, and what it is written in
(struct source-file (path relative module kind) #:transparent)

;; an attachment: a source file or directory of the project, and the relative
;; path its contents land at under the program's root
(struct attachment (source dest) #:transparent)

;; the Python dependencies a project declares: a requirements file, whose
;; lines pip reads, and requirement strings the build was given
(struct requirements (path entries) #:transparent)

;; the module the build writes, and packs, so a program can find what was
;; attached to it
(define runtime-module "rkt_pythonize_runtime")

;; dir         the project's directory
;; source      the program's directory, under dir
;; dest        the directory the build writes, under dir
;; name        the executable's name
;; entry       the module the program starts in
;; files       the program's source files, sorted
;; modules     the Python module names the program has, sorted
;; requirements the project's Python dependencies
;; attachments the files and directories the program ships
(struct project (dir source dest name entry files modules requirements attachments)
  #:transparent)

;; a Python module name: what a file's name and its directories must be, so
;; that (import x) -- which renders the name as it is written -- finds it
(define python-name #rx"^[A-Za-z_][A-Za-z0-9_]*$")

(define (python-name? s) (and (regexp-match? python-name s) #t))

;; the name a directory is called, whether or not it is written with a
;; trailing separator
(define (directory-name dir)
  (define text (path->string (simplify-path (path->complete-path dir))))
  (define trimmed (regexp-replace #rx"[/\\\\]+$" text ""))
  (define match (regexp-match #rx"([^/\\\\]+)$" trimmed))
  (and match (cadr match)))

;; ---- what the program is

;; the module a file under the source directory is: its name without the
;; extension, its directories as packages.  net/http.rkt is net.http, and an
;; __init__ is the package it initialises.
(define (module-of relative)
  (define parts (for/list ([element (in-list (explode-path relative))])
                  (path-element->string element)))
  (define file (last parts))
  (define stem (regexp-replace #rx"\\.[^.]*$" file ""))
  (define directories (drop-right parts 1))
  (for ([part (in-list (append directories (list stem)))])
    (unless (python-name? part)
      (error 'build-executable
             "~a: ~a is not a Python name, and a source file's path is the module's name"
             (path->string relative) part)))
  (cond [(equal? stem "__init__")
         (if (null? directories)
             (error 'build-executable
                    "~a: the source directory itself is not a package, so it has no __init__"
                    (path->string relative))
             (string-join directories "."))]
        [else (string-join (append directories (list stem)) ".")]))

;; every file of the program, in a stable order: the .rkt and .py under the
;; source directory, and not the compiled files or caches a tool may leave
(define (source-files dir)
  (sort (for/list ([path (in-list (walk dir))]
                   #:when (member (path-get-extension path) '(#".rkt" #".py")))
          path)
        string<? #:key path->string))

(define (walk dir)
  (append*
   (for/list ([element (in-list (sort (with-handlers ([exn:fail? (lambda (e) '())])
                                        (directory-list dir))
                                     string<? #:key path->string))])
     (define path (build-path dir element))
     (cond [(directory-exists? path)
            (if (ignored-name? element) '() (walk path))]
           [(file-exists? path) (list path)]
           [else '()]))))

(define (ignored-name? element)
  (define name (path-element->string element))
  (or (member name '("compiled" "__pycache__"))
      (regexp-match? #rx"^\\." name)))

;; is child the same as parent or under it?
(define (inside? parent child)
  (define relative (find-relative-path parent child))
  (and (path? relative)
       (not (regexp-match? #rx"^\\.\\.[/\\\\]" (path->string relative)))))

;; the directory the rendered program is written to
(define (project-python-directory project)
  (build-path (project-dest project) "python"))

;; the directory the project's venv is in: with the rest of what the build
;; writes, so removing the build removes it
(define (project-venv-directory project)
  (build-path (project-dest project) "venv"))

;; where one source file's Python is written: the same path, as .py
(define (project-output-file project file)
  (build-path (project-python-directory project)
              (path-replace-extension (source-file-relative file) #".py")))

;; the file a module's code is in
(define (project-module-path dir module)
  (path-add-extension (apply build-path dir (map string->path (string-split module ".")))
                      #".py"))

(define (project-entry-file project)
  (project-module-path (project-python-directory project) (project-entry project)))

;; ---- attachments

;; a destination is a path under the program's root: relative, no "..", and
;; without the separator --add-data is written with
(define (check-destination! source dest directory?)
  (define (not-a-destination)
    (error 'build-executable
           "~a: ~s is not a destination under the program's root"
           (path->string source) dest))
  (unless (and (string? dest) (not (string=? dest ""))) (not-a-destination))
  (when (regexp-match? #rx"[:;]" dest) (not-a-destination))
  (unless (relative-path? (string->path dest)) (not-a-destination))
  (when (regexp-match? #rx"(^|[/\\\\])\\.\\.([/\\\\]|$)" dest) (not-a-destination))
  (when (and directory? (equal? dest "."))
    (error 'build-executable
           "~a: a directory needs a destination with a name, not ~s"
           (path->string source) dest)))

(define (attachment-of dir source dest)
  (define source-path (build-path dir source))
  (define directory? (directory-exists? source-path))
  (unless (or directory? (file-exists? source-path))
    (error 'build-executable "~a: nothing to attach" (path->string source-path)))
  (check-destination! source-path dest directory?)
  (attachment source-path dest))

;; the destination of a #:data entry: (cons source dest), or (list source dest)
(define (entry-parts entry)
  (cond [(and (pair? entry) (string? (cdr entry))) (values (car entry) (cdr entry))]
        [(and (list? entry) (= 2 (length entry))) (values (first entry) (second entry))]
        [else
         (error 'build-executable
                "#:data takes (cons source dest) pairs, and not ~e"
                entry)]))

;; the data/ directory a project may have, and then whatever #:data names
(define (read-attachments dir data)
  (define data-directory (build-path dir "data"))
  (define convention
    (if (directory-exists? data-directory)
        (list (attachment data-directory "data"))
        '()))
  (define named
    (for/list ([entry (in-list data)])
      (define-values (source dest) (entry-parts entry))
      (attachment-of dir source dest)))
  (define all (append convention named))
  (check-one-attachment-a-destination all)
  all)

;; two attachments cannot land on one destination: the build would write one
;; over the other
(define (check-one-attachment-a-destination attachments)
  (define seen (make-hash))
  (for ([attachment (in-list attachments)])
    (define other (hash-ref seen (attachment-dest attachment) #f))
    (when other
      (error 'build-executable
             "~a and ~a both land at ~a"
             (path->string (attachment-source other))
             (path->string (attachment-source attachment))
             (attachment-dest attachment)))
    (hash-set! seen (attachment-dest attachment) attachment)))

;; ---- Python dependencies

;; #f is the project's own requirements.txt, if it has one; a path names
;; another file; a list of strings adds requirements to the project's file when
;; it has one
(define (read-requirements dir declared)
  (define convention (build-path dir "requirements.txt"))
  (define (project-file path)
    (simplify-path (path->complete-path path dir)))
  (cond
    [(not declared)
     (requirements (and (file-exists? convention) convention) '())]
    [(path-string? declared)
     (define path (project-file declared))
     (unless (file-exists? path)
       (error 'build-executable
              "~a: there is no requirements file ~a"
              (path->string dir) (path->string path)))
     (requirements path '())]
    [(and (list? declared) (andmap string? declared))
     (for ([entry (in-list declared)])
       (when (string=? (string-trim entry) "")
         (error 'build-executable "#:requirements has an empty requirement")))
     (requirements (and (file-exists? convention) convention) declared)]
    [else
     (error 'build-executable
            "#:requirements takes a requirements file or a list of requirement strings, not ~e"
            declared)]))

;; ---- reading a project

(define (read-project dir
                      #:source [source "src"]
                      #:dest [dest "build"]
                      #:name [name #f]
                      #:entry [entry "main"]
                      #:data [data '()]
                      #:requirements [declared #f])
  (define dir* (simplify-path (path->complete-path dir)))
  (unless (directory-exists? dir*)
    (error 'build-executable "no such project directory: ~a" (path->string dir*)))
  (define source* (build-path dir* source))
  (unless (directory-exists? source*)
    (error 'build-executable
           "~a: there is no source directory ~a"
           (path->string dir*) (path->string source*)))
  (define dest* (build-path dir* dest))
  (when (or (equal? source* dest*) (inside? source* dest*))
    (error 'build-executable
           "~a: the build directory cannot be inside the source directory"
           (path->string dest*)))
  (define files
    (for/list ([path (in-list (source-files source*))])
      (define relative (find-relative-path source* path))
      (source-file path relative (module-of relative)
                   (if (equal? (path-get-extension path) #".rkt") 'racket 'python))))
  (check-one-file-a-module files)
  (define modules (sort (remove-duplicates (map source-file-module files)) string<?))
  (unless (member entry modules)
    (error 'build-executable
           "~a: the entry module ~a is not one of the program's modules: ~a"
           (path->string dir*) entry (string-join modules ", ")))
  (define name* (or name (directory-name dir*) "program"))
  (unless (regexp-match? #rx"^[A-Za-z0-9][A-Za-z0-9._-]*$" name*)
    (error 'build-executable "~a is not a name for an executable" name*))
  (when (member runtime-module modules)
    (error 'build-executable
           "~a: the build writes the module ~a, so the program cannot have one"
           (path->string dir*) runtime-module))
  (define requirements* (read-requirements dir* declared))
  (define attachments* (read-attachments dir* data))
  (project dir* source* dest* name* entry files modules requirements* attachments*))

;; two source files that are one module cannot both be the module
(define (check-one-file-a-module files)
  (define seen (make-hash))
  (for ([file (in-list files)])
    (define module (source-file-module file))
    (define other (hash-ref seen module #f))
    (when other
      (error 'build-executable
             "~a and ~a are both the module ~a"
             (path->string (source-file-path other))
             (path->string (source-file-path file))
             module))
    (hash-set! seen module file)))
