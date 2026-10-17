#lang racket/base

;; The build system's one export.  A project's installer.rkt -- or a build.rkt
;; a person runs, or a person -- says
;;
;;   (require rkt-pythonize/builder)
;;   (build-executable here)
;;
;; and gets the path of the executable it built.  The project is a directory
;; with the program in src/; the build writes build/python (the rendered
;; program), build/pyinstaller (its work) and build/bin (the executable).
;;
;;   #:name        the executable's name         default: the directory's name
;;   #:source      where the program is          default: "src"
;;   #:entry       the module the program starts in, "main" by default
;;   #:data        attachments, (cons source dest) ...   default: none
;;   #:dest        where the build writes        default: "build"
;;   #:python      the interpreter that makes the venv, "python3" by default
;;   #:venv        the venv pyinstaller lives in, if not the installation's
;;   #:pyinstaller the version the venv gets, "6.22.3" by default; #f for any
;;   #:onefile?    one file, or one directory    default: #t
;;   #:quiet?      say nothing, except on error  default: #f

(require "builder/project.rkt"
         "builder/render.rkt"
         "builder/venv.rkt"
         "builder/pack.rkt")

(provide build-executable)

(define (build-executable project-directory
                          #:source [source "src"]
                          #:dest [dest "build"]
                          #:name [name #f]
                          #:entry [entry "main"]
                          #:data [data '()]
                          #:python [python "python3"]
                          #:venv [venv #f]
                          #:pyinstaller [pyinstaller "6.22.3"]
                          #:onefile? [onefile? #t]
                          #:quiet? [quiet? #f])
  (define project (read-project project-directory
                                #:source source
                                #:dest dest
                                #:name name
                                #:entry entry
                                #:data data))
  (unless quiet?
    (printf "build: ~a, ~a module~a, ~a attachment~a, from ~a\n"
            (project-name project)
            (length (project-modules project))
            (if (= 1 (length (project-modules project))) "" "s")
            (length (project-attachments project))
            (if (= 1 (length (project-attachments project))) "" "s")
            (path->string (project-source project))))
  (render-project project #:quiet? quiet?)
  (define venv-directory (or venv (default-venv-directory)))
  (define python* (ensure-venv venv-directory #:python python #:quiet? quiet?))
  (ensure-pyinstaller python* pyinstaller #:quiet? quiet?)
  (define artifact (pack-program project python* #:onefile? onefile? #:quiet? quiet?))
  (unless quiet?
    (printf "build: ~a\n" (path->string artifact)))
  artifact)
