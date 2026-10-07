#lang racket/base

;; The Markdown files that are generated are written here:
;;
;;   README.md, CHANGELOG.md   scribblings/readme.scrbl and
;;                             scribblings/changelog.scrbl, each with its own
;;                             top-level heading
;;
;; The manual is not generated: scribblings/manual.rkt assembles it from those
;; two documents, so its chapters are the README's own chapters.  Edit the
;; Scribble sources, then run build-docs -- or install the package, which runs
;; it through installer.rkt.

(require racket/file
         racket/list
         racket/path
         racket/system)

(provide build-docs)

;; a chapter as Markdown, checked to start with the heading its own file wants
(define (write-markdown scribblings dir chapter heading target)
  (define work (build-path dir ".build-docs"))
  (make-directory* work)
  (define raco (or (find-executable-path "raco") (error 'build-docs "no raco on PATH")))
  (define source (build-path scribblings chapter))
  (unless (system* raco "scribble" "--markdown" "--dest" (path->string work)
                   (path->string source))
    (error 'build-docs "raco scribble failed on ~a" (path->string source)))
  (define rendered (build-path work (path-replace-extension chapter #".md")))
  (unless (file-exists? rendered)
    (error 'build-docs "raco scribble wrote no ~a" (path->string rendered)))
  (define lines (file->lines rendered))
  (unless (and (pair? lines)
               (member (first lines) (list (format "# ~a" heading)
                                           (format "## ~a" heading)
                                           (format "# ## ~a" heading))))
    (error 'build-docs "~a starts with ~s, not the ~a heading"
           chapter (if (pair? lines) (first lines) "") heading))
  (define destination (build-path dir target))
  (define new (string->path (format "~a.new" destination)))
  (call-with-output-file new #:exists 'truncate
    (lambda (o) (for ([l (in-list lines)]) (displayln l o))))
  (rename-file-or-directory new destination #t)
  (printf "build-docs: wrote ~a\n" target))

;; build-docs [dir]: write the generated files of the package in dir
(define (build-docs [dir (current-directory)])
  (define scribblings (build-path dir "scribblings"))
  (unless (directory-exists? scribblings)
    (error 'build-docs "no scribblings directory in ~a" (path->string dir)))
  (write-markdown scribblings dir "readme.scrbl" "rkt-pythonize" "README.md")
  (write-markdown scribblings dir "changelog.scrbl" "Changelog" "CHANGELOG.md")
  (define work (build-path dir ".build-docs"))
  (when (directory-exists? work) (delete-directory/files work))
  (void))

(module+ main
  (define args (current-command-line-arguments))
  (build-docs (if (zero? (vector-length args))
                  (current-directory)
                  (string->path (vector-ref args 0)))))
