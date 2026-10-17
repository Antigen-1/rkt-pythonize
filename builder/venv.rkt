#lang racket/base

;; The build starts with nothing but python and its venv module: pyinstaller
;; is not a dependency of starting the build, it is installed into a venv the
;; build makes -- the one the rkt-pythonize installation manages.

(require racket/file
         racket/path
         racket/port
         racket/string
         racket/system)

(provide default-venv-directory
         venv-python-path
         ensure-venv
         ensure-pyinstaller)

(define (windows?) (eq? (system-type 'os) 'windows))

;; one venv for this installation: under the add-on directory of the Racket
;; installation rkt-pythonize is installed in, where raco pkg cannot take it
;; away
(define (default-venv-directory)
  (build-path (find-system-path 'addon-dir) "rkt-pythonize" "venv"))

(define (venv-python-path directory)
  (build-path directory
              (if (windows?) "Scripts" "bin")
              (if (windows?) "python.exe" "python")))

(define (executable name)
  (or (find-executable-path name)
      (and (file-exists? name) (path->complete-path name))))

(define (command-output command . arguments)
  (define output (open-output-string))
  (define ok
    (parameterize ([current-output-port output]
                   [current-error-port (open-output-nowhere)])
      (apply system* command arguments)))
  (values ok (string-trim (get-output-string output))))

;; the venv's python, made if it is not there yet
(define (ensure-venv directory #:python [python "python3"] #:quiet? [quiet? #f])
  (define directory* (simplify-path (path->complete-path directory)))
  (define python* (executable python))
  (unless python*
    (error 'build-executable
           "~a is not an executable: #:python names the interpreter that makes the venv"
           python))
  (define venv-python (venv-python-path directory*))
  (cond [(file-exists? venv-python) venv-python]
        [else
         (unless quiet?
           (printf "build: making the venv in ~a\n" (path->string directory*)))
         (unless (system* python* "-m" "venv" (path->string directory*))
           (error 'build-executable
                  "~a -m venv ~a failed: is the venv module installed?"
                  (path->string python*)
                  (path->string directory*)))
         (unless (file-exists? venv-python)
           (error 'build-executable "~a: the venv has no python"
                  (path->string directory*)))
         venv-python]))

;; pyinstaller, installed into the venv at the version the build asks for
(define (ensure-pyinstaller venv-python version #:quiet? [quiet? #f])
  (define installed (pyinstaller-version venv-python))
  (define wanted (and version (format "pyinstaller==~a" version)))
  (cond [(and version (equal? installed version)) venv-python]
        [(and (not version) installed) venv-python]
        [else
         (unless quiet?
           (printf "build: installing ~a in ~a\n"
                   (or wanted "pyinstaller")
                   (path->string venv-python)))
         (define arguments
           (append (list "-m" "pip" "install" "--disable-pip-version-check")
                   (if quiet? (list "-q") '())
                   (list (or wanted "pyinstaller"))))
         (unless (apply system* venv-python arguments)
           (error 'build-executable "installing pyinstaller in ~a failed"
                  (path->string venv-python)))
         venv-python]))

(define (pyinstaller-version venv-python)
  (define-values (ok output)
    (command-output venv-python "-c" "import PyInstaller; print(PyInstaller.__version__)"))
  (and ok output))
