#lang racket/base

;; The last stage: pyinstaller turns build/python into one file.  It cannot
;; see a module a program imports with importlib.import_module -- which is
;; what (import x) renders -- so the build names every module of the program
;; to it, and the modules of the Python dependencies installed in the venv.

(require racket/file
         racket/list
         racket/path
         racket/string
         racket/system
         "project.rkt")

(provide pack-program
         pack-arguments)

(define (windows?) (eq? (system-type 'os) 'windows))

;; --add-data writes SRC and DEST apart with the platform's path separator
(define path-separator (if (windows?) ";" ":"))

(define (executable-name name)
  (if (windows?) (string-append name ".exe") name))

;; every module to name: the program's, the runtime's, and the dependencies the
;; build was given, with the packages they are in
(define (modules-to-name project dependencies)
  (define modules
    (append (list runtime-module) (project-modules project) dependencies))
  (sort (remove-duplicates
         (append modules (append-map packages-of modules))
         equal?)
        string<?))

(define (packages-of module)
  (define parts (string-split module "."))
  (for/list ([i (in-range 1 (length parts))])
    (string-join (take parts i) ".")))

;; the arguments pyinstaller is run with, so a test can read them without
;; building an executable
(define (pack-arguments project python-directory hidden-imports
                        #:onefile? [onefile? #t]
                        #:quiet? [quiet? #f])
  (define dist (build-path (project-dest project) "bin"))
  (define work (build-path (project-dest project) "pyinstaller"))
  (define dependencies
    (filter (lambda (module) (not (string=? module ""))) hidden-imports))
  (append
   (list "-m" "PyInstaller"
         (if onefile? "--onefile" "--onedir")
         "--noconfirm" "--clean"
         "--log-level" (if quiet? "ERROR" "WARN")
         "--name" (project-name project)
         "--distpath" (path->string dist)
         "--workpath" (path->string (build-path work "build"))
         "--specpath" (path->string work)
         "--paths" (path->string python-directory))
   (append-map (lambda (module) (list "--hidden-import" module))
               (modules-to-name project dependencies))
   (append-map (lambda (attachment)
                 (list "--add-data"
                       (string-append (path->string (attachment-source attachment))
                                      path-separator
                                      (attachment-dest attachment))))
               (project-attachments project))
   (list (path->string (project-entry-file project)))))

(define (pack-program project venv-python
                      #:onefile? [onefile? #t]
                      #:hidden-imports [hidden-imports '()]
                      #:quiet? [quiet? #f])
  (define python-directory (project-python-directory project))
  (define dist (build-path (project-dest project) "bin"))
  (define work (build-path (project-dest project) "pyinstaller"))
  (make-directory* dist)
  (make-directory* work)
  (define arguments
    (pack-arguments project python-directory hidden-imports
                    #:onefile? onefile?
                    #:quiet? quiet?))
  (unless quiet?
    (printf "build: pyinstaller ~a ~a\n"
            (if onefile? "--onefile" "--onedir")
            (path->string (project-entry-file project))))
  (unless (apply system* venv-python arguments)
    (error 'build-executable "pyinstaller failed"))
  (define artifact
    (if onefile?
        (build-path dist (executable-name (project-name project)))
        (build-path dist (project-name project)
                    (executable-name (project-name project)))))
  (unless (file-exists? artifact)
    (error 'build-executable "pyinstaller wrote no ~a" (path->string artifact)))
  artifact)
