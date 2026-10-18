#lang racket/base

;; The build system's one export.  A project's installer.rkt -- or a build.rkt
;; a person runs, or a person -- says
;;
;;   (require rkt-pythonize/builder)
;;   (build-executable here)
;;
;; and gets the path of the executable it built.  The project is a directory
;; with the program in src/ and its Python dependencies in requirements.txt;
;; the build writes build/python (the rendered program), build/venv (the
;; project's Python dependencies and PyInstaller), build/pyinstaller (its
;; work) and build/bin (the executable).
;;
;;   #:name          the executable's name         default: the directory's name
;;   #:source        where the program is          default: "src"
;;   #:entry         the module the program starts in, "main" by default
;;   #:data          attachments, (cons source dest) ...   default: none
;;   #:requirements  a requirements file, or requirement strings; #f reads
;;                   requirements.txt at the project's root if it has one
;;   #:hidden-imports  Python modules to name to PyInstaller   default: none
;;   #:collect-dependencies?  name the venv's modules to PyInstaller
;;                   default: #t
;;   #:dest          where the build writes        default: "build"
;;   #:python        the interpreter that makes the venv, "python3" by default
;;   #:venv          the project's venv, if not build/venv
;;   #:pyinstaller   the version installed into that venv, "6.22.3" by
;;                   default; #f for any
;;   #:onefile?      one file, or one directory    default: #t
;;   #:quiet?        say nothing, except on error  default: #f

(require racket/path
         "builder/project.rkt"
         "builder/render.rkt"
         "builder/venv.rkt"
         "builder/pack.rkt")

(provide build-executable)

;; a list of Python module names, and not something that will fail in
;; pyinstaller's command line
(define (check-hidden-imports modules)
  (unless (and (list? modules)
               (andmap (lambda (module)
                         (and (string? module) (not (string=? module ""))))
                       modules))
    (error 'build-executable
           "#:hidden-imports takes a list of Python module names, not ~e"
           modules))
  modules)

(define (build-executable project-directory
                          #:source [source "src"]
                          #:dest [dest "build"]
                          #:name [name #f]
                          #:entry [entry "main"]
                          #:data [data '()]
                          #:requirements [requirements #f]
                          #:hidden-imports [hidden-imports '()]
                          #:collect-dependencies? [collect-dependencies? #t]
                          #:python [python "python3"]
                          #:venv [venv #f]
                          #:pyinstaller [pyinstaller "6.22.3"]
                          #:onefile? [onefile? #t]
                          #:quiet? [quiet? #f])
  (check-hidden-imports hidden-imports)
  (unless (boolean? collect-dependencies?)
    (error 'build-executable
           "#:collect-dependencies? takes a boolean, not ~e"
           collect-dependencies?))
  (define project (read-project project-directory
                                #:source source
                                #:dest dest
                                #:name name
                                #:entry entry
                                #:data data
                                #:requirements requirements))
  (unless quiet?
    (printf "build: ~a, ~a module~a, ~a attachment~a, from ~a\n"
            (project-name project)
            (length (project-modules project))
            (if (= 1 (length (project-modules project))) "" "s")
            (length (project-attachments project))
            (if (= 1 (length (project-attachments project))) "" "s")
            (path->string (project-source project))))
  (render-project project #:quiet? quiet?)
  (define venv-directory
    (if venv
        (path->complete-path venv (project-dir project))
        (project-venv-directory project)))
  (define venv-python
    (prepare-venv venv-directory
                  (project-requirements project)
                  #:python python
                  #:pyinstaller pyinstaller
                  #:working-directory (project-dir project)
                  #:quiet? quiet?))
  (define dependency-imports
    (if collect-dependencies?
        (venv-hidden-imports venv-python)
        '()))
  (define artifact
    (pack-program project venv-python
                  #:hidden-imports (append dependency-imports hidden-imports)
                  #:onefile? onefile?
                  #:quiet? quiet?))
  (unless quiet?
    (printf "build: ~a\n" (path->string artifact)))
  artifact)
