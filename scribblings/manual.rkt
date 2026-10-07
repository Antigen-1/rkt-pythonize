#lang racket/base

;; The manual, as a document: the README's chapters, and then the changelog.
;;
;; README.md is rendered from scribblings/readme.scrbl, and this manual is that
;; same text: a chapter is written once, and the manual's chapters sit at the
;; same level as the changelog.  Nothing is generated here -- build-docs.rkt
;; writes only the two Markdown files -- so the manual and the README cannot
;; drift apart, and no chapter is titled README.

(require racket/list
         racket/runtime-path
         scribble/manual
         scribble/struct)

(provide doc)

;; the two documents sit beside this file, wherever it is loaded from
(define-runtime-path here ".")
(define readme (dynamic-require (build-path here "readme.scrbl") 'doc))
(define changelog (dynamic-require (build-path here "changelog.scrbl") 'doc))

;; A chapter of a titled document is a part there, so the README's chapters are
;; its parts, and they are reparented here unchanged: the styles and tags they
;; were written with come along with them.
(define readme-parts (part-parts readme))

;; A part's seventh field is the list of parts it is made of.  The accessor for
;; it is part-parts above, which is the documented way to read it; the check
;; here is what says the field is still the one we rebuild with, so that a
;; change in Scribble's structure fails loudly instead of pruning the manual.
(define fields (cdr (vector->list (struct->vector readme))))
(unless (equal? (list-ref fields 6) readme-parts)
  (error 'manual "the seventh field of a part is not its parts any more"))

(define doc
  (apply part (list-set fields 6 (append readme-parts (list changelog)))))
