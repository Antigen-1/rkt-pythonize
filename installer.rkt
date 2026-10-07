#lang racket/base

;; The package's installer.  info.rkt names it, and raco pkg runs it when the
;; package is installed or updated: it writes the files that are generated --
;; the manual, README.md and CHANGELOG.md -- from the Scribble sources, so an
;; installed copy has them as the sources say.
;;
;; It is run from wherever raco pkg happens to be, so it finds its own
;; directory and builds from there.

(require racket/runtime-path
         "build-docs.rkt")

(define-runtime-path here ".")

(build-docs here)
