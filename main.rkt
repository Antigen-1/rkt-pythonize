#lang racket/base

(module+ test
  (require rackunit racket/system racket/pretty))

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included LICENSE-MIT and LICENSE-APACHE files.
;; If you would prefer to use a different license, replace those files with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here

(require "core/main.rkt" "passes/uniquify.rkt" "passes/explicit.rkt" "passes/cps.rkt" "passes/quote.rkt" "passes/let.rkt"
         "passes/named-let.rkt" "passes/cond.rkt" "passes/chain.rkt" "passes/vm.rkt"
         "passes/stream.rkt" "passes/more-cond.rkt" "passes/cond-explicit.rkt" "passes/beta-reduce.rkt"
         "passes/partial-evaluate.rkt" "passes/L0-uniquify.rkt" "passes/handler.rkt" "passes/main.rkt"
         "script/script.rkt"
         racket/contract racket/file)
(provide L parse-L unparse-L LS parse-LS unparse-LS current-primitives py-lib-string
         (contract-out (rename compile compile-scheme-code
                               (->* (any/c)
                                    (#:script? boolean?)
                                    any))))

(define py-lib-string (file->string core-py))

(define (repeat-pass n p e)
  (let loop ((n n) (e e))
    (if (= n 0)
        e
        (loop (- n 1) (p e)))))

(define (compile code #:script? (script? #f))
  ((compose1
    compile-L0
    L0-uniquify
    cps
    (lambda (e) (repeat-pass 5 (compose1 partial-evaluate beta-reduce) e))
    uniquify
    make-explicit
    add-quote
    expand-let
    expand-named-let
    expand-cond
    expand-chain
    expand-vm
    expand-stream
    expand-more-cond
    make-cond-explicit
    expand-exn-handler
    (if script? 
      (compose1 LS->L parse-LS)
      parse-L))
   code))

(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (provide example-table)

  (define python-exe (or (cond ((getenv "TEST_PYTHON_EXE") => find-executable-path) (else #f))
                         (find-executable-path "python3") (find-executable-path "python")))
  (define example-table (make-hash))
  (define (test code output #:script? (script? #f) #:example? (example? #f))
    (test-begin
      (pretty-write code)
      (displayln "Compilation:")
      (define json (time (compile code #:script? script?)))
      (displayln "Evaluation:")
      (check-equal?
        (let ((out (open-output-string)))
         (check-true
          (time
           (parameterize ((current-output-port out))
             (system* python-exe core-py json))))
         (get-output-string out))
        output)
      (if example? (hash-set! example-table code output) (void))))

  ;; Uniquify
  (test '((lambda (mod)
            ((lambda (none)
               (vm-apply none '(1)))
             (get-attribute mod '"print")))
          (dynamic-require '"builtins" none))
        "1\n")
  ;; Explicit
  (test '((lambda (mod)
            ((lambda (print)
               '2
               (vm-apply print '(1)))
             (get-attribute mod '"print")))
          (dynamic-require '"builtins" none))
        "1\n")
  ;; CPS
  (test '(let/cc cc
           ((lambda (mod)
              ((lambda (print)
                 (vm-apply print '("1"))
                 (cc (vm-apply print '("2")))
                 (vm-apply print '("3")))
               (get-attribute mod '"print")))
            (dynamic-require '"builtins" none)))
        "1\n2\n")
  (test '(let/cc cc
           ((lambda (mod)
              ((lambda (print)
                 (vm-apply print '("1")))
               (get-attribute mod '"print")))
            (dynamic-require '"builtins" none)))
        "1\n")
  (test '(let/cc cc cc) "")
  ;; Quote
  (test '2 "")
  (test '#f "")
  (test '"" "")
  (test #hasheq{} "")
  (test ''() "")
  ;; Let & Letrec
  (test '(let ((mod (dynamic-require "builtins" none)))
           (let ((print (get-attribute mod "print")))
             (vm-apply print '("1"))))
        "1\n")
  (test '(let ((mod (dynamic-require "builtins" none))
               (box #f))
           (let ((print (get-attribute mod "print")))
             (letrec ((proc
                       (lambda ()
                         (if box
                             (vm-apply print '("-1"))
                             (begin
                               (vm-apply print '("1"))
                               (set! box #t)
                               (proc))))))
               (proc))))
        "1\n-1\n")
  ;; Ref & Set & Has
  (test '(let ((mod (dynamic-require "builtins" none)))
           (let ((print (get-attribute mod "print")))
             (vm-apply print (@ '(("1")) 0))))
        "1\n")
  (test '(let ((mod (dynamic-require "builtins" none)))
           (let ((print (get-attribute mod "print")))
             (vm-apply print (@ '#hasheq((a . (1))) "a"))))
        "1\n")
  (test '(let ((mod (dynamic-require "builtins" none)))
           (let ((print (get-attribute mod "print"))
                 (l '("1")))
             (! l 0 "2")
             (vm-apply print l)))
        "2\n")
  (test '(let ((mod (dynamic-require "builtins" none)))
           (let ((print (get-attribute mod "print"))
                 (t '#hasheq((a . (1)))))
             (! t "a" '(2))
             (vm-apply print (@ t "a"))))
        "2\n")
  (test '(let ((mod (dynamic-require "builtins" none)))
           (let ((print (get-attribute mod "print"))
                 (t '#hasheq((a . (1))))
                 (l '()))
             (<! l (? t "a"))
             (vm-apply print l)))
        "True\n")
  ;; Fibonacci
  (test '(let ((mod (dynamic-require "builtins" none)))
           (let ((print
                  (lambda (v)
                    (let ((l '()))
                      (<! l v)
                      (vm-apply (get-attribute mod "print") l)))))
             (letrec ((f 0)
                      (s 1)
                      (proc
                       (lambda (n)
                         (if (equal? n 0)
                             none
                             (let ((r (+ f s)))
                               (print f)
                               (set! f s)
                               (set! s r)
                               (proc (- n 1)))))))
               (proc 10))))
        "0\n1\n1\n2\n3\n5\n8\n13\n21\n34\n"
        #:example? #t)
  ;; Named let
  (test '(let ((mod (dynamic-require "builtins" none)))
           (let ((print (lambda (v)
                          (let ((l '()))
                            (<! l v)
                            (vm-apply (get-attribute mod "print") l)))))
             (let loop ((n 10) (r 0))
               (if (equal? n 0)
                   (print r)
                   (loop (- n 1) (+ n r))))))
        "55\n")
  ;; Cond
  (test '(let ((mod (dynamic-require "builtins" none)))
           (let ((print (lambda (v)
                          (let ((l '()))
                            (<! l v)
                            (vm-apply (get-attribute mod "print") l)))))
             (print (and))
             (print (or))
             (print (and #t 1))
             (print (or 2 #t))
             ))
        "True\nFalse\n1\n2\n")
  (test '(let ((mod (dynamic-require "builtins" none)))
           (let ((print (lambda (v)
                          (let ((l '()))
                            (<! l v)
                            (vm-apply (get-attribute mod "print") l)))))
             (if #t (print 1))
             (print (if #f 1))
             ))
        "1\nNone\n")
  ;; Simple begin forms
  (test '(letrec ((mod (dynamic-require "builtins" none))
                  (print (lambda (v)
                           (let ((l '()))
                             (<! l v)
                             (vm-apply (get-attribute mod "print") l)))))
           (begin
             (print "1"))
           )
        "1\n")
  (test '(letrec ((mod (dynamic-require "builtins" none))
                  (print (lambda (v)
                           (let ((l '()))
                             (<! l v)
                             (vm-apply (=> mod "print") l)))))
            (print (begin (begin 1))))
        "1\n")
  (test '(letrec ((mod (dynamic-require "builtins" none))
                  (print (lambda (v)
                           (let ((l '()))
                             (<! l v)
                             (vm-apply (=> mod "print") l)))))
            (print (begin (begin 1 2))))
        "2\n")
  ;; Chain
  (test '(letrec ((mod (dynamic-require "builtins" none))
                  (print (lambda (v)
                           (let ((l '()))
                             (<! l v)
                             (vm-apply (=> mod "print") l)))))
           (print "1")
           )
        "1\n")
  (test '(letrec ((mod (dynamic-require "builtins" none))
                  (print (lambda (v)
                           (let ((l '()))
                             (<! l v)
                             (vm-apply (=> mod "print") l)))))
           (=>! mod "a" 2)
           (print (=> mod "a"))
           )
        "2\n")
  ;; VM
  (test '(letrec ((mod (dynamic-require "builtins" none))
                  (print (#%vm-procedure (=> mod "print") 1)))
           (print "1")
           )
        "1\n")
  (test '(letrec ((mod (dynamic-require "builtins" none))
                  (print (#%vm-procedure (=> mod "print") #f)))
           (print "1" "2")
           )
        "1 2\n")
  (test '(letrec ((mod (dynamic-require "builtins" none))
                  (print (#%vm-procedure (=> mod "print") 1)))
           (print (vm-apply (#%scm-procedure + 2) '(1 2)))
           )
        "3\n")
  (test '(letrec ((mod (dynamic-require "builtins" none))
                  (print (#%vm-procedure (=> mod "print") #f)))
           (vm-apply (#%scm-procedure print #f)
                     '(1 2 3))
           )
        "1 2 3\n")
  ;; Error
  (test `(error "") "")
  (test `(raise "") "")
  (test `(print (error "")) "")
  (test `(if (raise "") (print "") (print "")) "")
  (test `((error "") "") "")
  (test `(with-handler (lambda (v) (if (is-a? v str-type) (print v) (throw v)))
            (throw "A"))
         "A\n"
         #:example? #t)
  (test `(with-handler (lambda (v) (if (is-a? v str-type) (print v) (throw v)))
            (throw 1))
         ""
         #:example? #t)
  (test `(let/cc cc
            (with-handler (lambda (v) (if (is-a? v str-type) (print v) (throw v)))
              (throw (cc "A"))))
         ""
         #:example? #t)
  (test `(with-handler (lambda (v) (if (is-a? v str-type) (print v) (throw v)))
            ((make-procedure (lambda (s) (print (throw (@ s 0))))) "A"))
         "A\n"
         #:example? #t)
  (test `(let/cc cc
            (with-handler (lambda (v) (if (is-a? v str-type) (print v) (throw v)))
              ((make-procedure (lambda (s) (throw (cc (@ s 0))))) "A")))
         ""
         #:example? #t)
  (test `(with-handler
            print 
            (vm-apply (#%scm-procedure (lambda () (throw 1)) 0) '()))
        "1\n"
        #:example? #t)
  (test `(with-handler
          (lambda (v) (print (+ v 1)))
          (vm-apply (#%scm-procedure (lambda () (throw 1)) 0) '()))
        "2\n"
        #:example? #t)
  ;; Stream
  (test '(letrec ((mod (dynamic-require "builtins" none))
                  (print (#%vm-procedure (=> mod "print") 1))
                  (stream-for-each
                   (lambda (p s)
                     (if (equal? s none)
                         none
                         (begin
                           (p (stream-car s))
                           (stream-for-each p (stream-cdr s))))))
                  (numbers '(1 2 3 4 5 6))
                  (number-stream (let loop ((i 0))
                                   (if (equal? i (length numbers))
                                       none
                                       (stream-cons (@ numbers i)
                                                    (loop (+ i 1)))))))

           (stream-for-each print number-stream)
           (stream-for-each print number-stream))
        "1\n2\n3\n4\n5\n6\n1\n2\n3\n4\n5\n6\n"
        )
  ;; More cond
  (test '(letrec ((print (#%vm-procedure (=> (dynamic-require "builtins" none) "print") 1)))
           (cond (else (print 1)))
           (cond (1 (print 1))
                 (else (print 2)))
           (cond (#f (print 1))
                 (else (print 2)))
           (cond (1 (print 1) (print 2))
                 (else none)))
        "1\n1\n2\n1\n2\n")
  ;; Partial evaluation
  (test `(letrec ((print (#%vm-procedure (=> (dynamic-require "builtins" none) "print") 1)))
           (print (+ 1 2))
           (print (/ 1 2.0))
           (print (/ 1 2))
           (print (quotient 1 2.0))
           (print (modulo 1 2.0))
           (print (negate 1.0))
           (print (eq? none 'none))
           (print (eq? 'none 'none))
           (print (eq? "a" ,(string #\a)))
           (print (eq? 1 1.0))
           (print (eq? '(1) '(1)))
           (print (eq? '#hasheq{} '#hasheq{}))
           (print (equal? 1 1.0))
           (print (equal? '(1) '(1)))
           (print (equal? '(1) '(1 2)))
           (print (equal? '#hasheq{(x . 1)}
                          '#hasheq{(x . 1)}))
           (print (equal? '#hasheq{(x . 1)}
                          '#hasheq{(x . 1)
                                   (y . 2)}))
           (print (equal? '(1) '#hasheq{(x . 1)}))
           (print (if 1 1 2))
           (print (if + 2 1))
           (print (if #f 2 3))
           (print (if (lambda (x) x) 4 5))
           (apply print '("1"))
           (print (is-a? 1 object-type))
           (print (not dynamic-require))
           (print (not #t))
           )
        "3\n0.5\n0.5\n0.0\n1.0\n-1.0\nTrue\nTrue\nTrue\nFalse\nFalse\nFalse\nTrue\nTrue\nFalse\nTrue\nFalse\nFalse\n1\n2\n3\n4\n1\nTrue\nFalse\nFalse\n"
        #:example? #t)
  ;; Linked lists
  (let ((test-list (build-list 40 (lambda (n) (random 0 10000)))))
    (test `(letrec ((array-list->linked-list (lambda (al) (let ((len (length al))) (let loop ((i 0)) (if (equal? i len) null (cons (@ al i) (loop (+ i 1))))))))
                    (append (lambda (l1 l2)
                              (if (eq? l1 null)
                                  l2
                                  (cons (car l1) (append (cdr l1) l2)))))
                    (partition (lambda (n l p)
                                  (if (eq? l null)
                                      (cons null (cons null null))
                                      (let ((first (car l))
                                            (sl (partition n (cdr l) p)))
                                        (if (p first n)
                                            (cons (cons first (@ sl 0)) (cons (@ sl 1) null))
                                            (cons (@ sl 0) (cons (cons first (@ sl 1)) null)))))))
                    (sort (lambda (l)
                            (if (eq? l null)
                                l
                                (let ((first (car l))
                                      (rest (cdr l)))
                                    (let ((pl (partition first rest <)))
                                      (append (sort (@ pl 0)) (cons first (sort (@ pl 1))))))))))
                (print (sort (array-list->linked-list ',test-list))))
          (string-append (format "~a" (sort test-list <)) "\n")
          #:example? #t))
  ;; Scripting
  (test '(#%script-begin
          (define for-each (lambda (p l) (if (eq? l null) none (begin (p (car l)) (for-each p (cdr l))))))
          (for-each print (cons 1 (cons 2 (cons 3 null)))))
        "1\n2\n3\n"
        #:script? #t
        #:example? #t)
  ;; Benchmark
  (let ((test-list (build-list 5000 (lambda (n) (random 0 10000)))))
    (test `(letrec ((array-list->linked-list (lambda (al) (let ((len (length al))) (let loop ((i 0)) (if (equal? i len) null (cons (@ al i) (loop (+ i 1))))))))
                    (append (lambda (l1 l2)
                              (if (eq? l1 null)
                                  l2
                                  (cons (car l1) (append (cdr l1) l2)))))
                    (partition (lambda (n l p)
                                  (if (eq? l null)
                                      (cons null (cons null null))
                                      (let ((first (car l))
                                            (sl (partition n (cdr l) p)))
                                        (if (p first n)
                                            (cons (cons first (@ sl 0)) (cons (@ sl 1) null))
                                            (cons (@ sl 0) (cons (cons first (@ sl 1)) null)))))))
                    (sort (lambda (l)
                            (if (eq? l null)
                                l
                                (let ((first (car l))
                                      (rest (cdr l)))
                                    (let ((pl (partition first rest <)))
                                      (append (sort (@ pl 0)) (cons first (sort (@ pl 1))))))))))
                (print (sort (array-list->linked-list ',test-list))))
          (string-append (format "~a" (sort test-list <)) "\n")))
  (test '(letrec ((builtin (dynamic-require "builtins" none))
                  (print (#%vm-procedure (=> builtin "print") 1))
                  (stream-filter (lambda (p s)
                                   (if (equal? s none)
                                       none
                                       (let ((f (stream-car s)))
                                         (if (p f)
                                             (stream-cons f (stream-filter p (stream-cdr s)))
                                             (stream-filter p (stream-cdr s)))))))
                  (numbers (let loop ((n 2))
                             (stream-cons n (loop (+ n 1)))))
                  (primes (let loop ((ns numbers))
                            (let ((first (stream-car ns))
                                  (rest (stream-cdr ns)))
                              (stream-cons first
                                           (loop (stream-filter
                                                  (lambda (n)
                                                    (not (equal? (modulo n first) 0)))
                                                  rest)))))))
           (let loop ((n 300) (ps primes))
             (if (equal? n 0)
                 (print (stream-car ps))
                 (loop (- n 1) (stream-cdr ps)))))
        "1993\n"
        #:example? #t)
)   

(module+ main
  ;; (Optional) main submodule. Put code here if you need it to be executed when
  ;; this file is run using DrRacket or the `racket` executable.  The code here
  ;; does not run when this file is required by another module. Documentation:
  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29

  (require racket/cmdline racket/match racket/list racket/system racket/pretty raco/command-name)
  (define dest (box #f))
  (define script? (box #f))
  (define json? (box #f))
  (define python (box (or (cond ((getenv "PYTHON_EXE") => find-executable-path) (else #f))
                          (find-executable-path "python3") 
                          (find-executable-path "python"))))

  (define (execute exe code (form 'unknown))
    (cond ((system* exe core-py code) => void)
          (else (raise-user-error 'rkt-pythonize "Fail to run the scheme code:\n~a" (pretty-format #:mode 'write form)))))

  (command-line
    #:program (short-program+command-name)
    #:once-each
    [("-o" "--output") o "Where to write generated code" (set-box! dest o)]
    [("-s" "--script") "Use input files as scripts" (set-box! script? #t)]
    [("-p" "--python") py "Set the python executable" (set-box! python (find-executable-path py))]
    [("-j" "--json") "Recognize supplied files as json codes" (set-box! json? #t)]
    #:ps
    "When -j/--json is not provided:"
    "If -o/--ouput is provided, json codes will be saved to the specified file."
    "Otherwise, json codes will be evaluated directly."
    "When -j/--json is provided:"
    "Exactly one json file should be provided and will then be executed."
    #:args files
    (define/contract python-exe 
        path-string?
        (unbox python))
    (match* (files json?)
      (((list source0 sources ...) (box #f))
       (define dest-path (unbox dest))
       (define script?-bool (unbox script?))

       (define form
          (cons (if script?-bool '#%script-begin 'begin)
            (append*
             (map
              (lambda (source)
                (call-with-input-file
                  source
                  (lambda (in)
                    (let loop ()
                      (define v (read in))
                      (if (eof-object? v)
                          null
                          (cons v (loop)))))))
              (cons source0 sources)))))
       (define compiled (compile #:script? script?-bool form))

       (if dest-path
           (call-with-output-file dest-path #:exists 'truncate/replace (lambda (out) (write-string compiled out)))
           (execute python-exe compiled form)))
      (((list json) (box #t))
       (execute python-exe (file->string json)))
      ((files json?)
       (raise-user-error 'rkt-pythonize "Malformed arguments:\n\tfiles: ~s\n\tjson?: ~s" files (unbox json?))))))
