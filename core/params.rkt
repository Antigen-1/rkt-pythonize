#lang racket/base

;; The parameter lists define and lambda share, and the arguments a call hands
;; a procedure, and the keyword arguments in both.
;;
;; A signature is
;;
;;   (p ... #:k k ... . rest)
;;
;; positional parameters, keyword parameters and the rest parameter in any
;; order, with the rest parameter last (the reader puts it there, and so does
;; Racket).  Python wants the positional parameters first and a keyword-only
;; one after a star, so a signature renders as
;;
;;   def f(p, ..., *rest, k, ...)     or, with no rest parameter, * where it
;;                                    would be
;;
;; and a keyword parameter is the Python parameter name: (f p #:k k) calls it.
;; In Python a keyword and the parameter it fills are one name, so a keyword's
;; name -- converted as any name is, #:foo-bar is foo_bar and #:even? is even_p
;; -- and the name of the parameter it binds have to come out the same, and a
;; signature that spells them differently is a compile error.
;;
;; A call writes its keyword arguments where it likes, Racket's way:
;;
;;   (f 1 #:k 2 3 #:j 4)  renders  f(1, 3, k=2, j=4)
;;
;; the positional arguments in their order first, then the keyword ones in
;; theirs.  That is the order Python takes them in, so it is also the order
;; they are evaluated in, which is why the arguments of a call move.

(require racket/list
         "names.rkt")

(provide keyword-param keyword-param-keyword keyword-param-name
         signature-parts signature-datum keyword-signature-datum
         argument-parts)

;; a keyword parameter: the keyword, and the parameter it binds
(struct keyword-param (keyword name) #:transparent)

(define (fail! message)
  (error 'params "~a" message))

;; the parts of the signature a define target or a lambda takes:
;; (values the positional names, the keyword parameters, the rest name or #f).
;; A caller that has a form to point at hands in its own fail, which is how the
;; errors read as the form they belong to.
(define (signature-parts sig [fail fail!])
  (define (name! item)
    (if (symbol? item)
        item
        (fail (format "a parameter is a name, and this is ~s" item))))
  (define (walk rest positional keywords tail)
    (cond [(null? rest) (values (reverse positional) (reverse keywords) tail)]
          [(pair? rest)
           (define item (car rest))
           (cond [(keyword? item)
                  (define-values (name more) (keyword-parameter item (cdr rest) fail))
                  (walk more positional (cons (keyword-param item name) keywords) tail)]
                 [else (walk (cdr rest) (cons (name! item) positional) keywords tail)])]
          [else (walk '() positional keywords (name! rest))]))
  (define-values (positional keywords rest)
    ;; a lone rest parameter is a signature too: (define (f . rest) ...)
    (if (symbol? sig)
        (values '() '() sig)
        (begin
          (unless (or (null? sig) (pair? sig))
            (fail (format "a parameter list is (name ...) or (name ... . rest), and this is ~s"
                          sig)))
          (walk sig '() '() #f))))
  (check-once! (append positional
                       (map keyword-param-name keywords)
                       (if rest (list rest) '()))
              fail)
  (values positional keywords rest))

;; what a keyword parameter names: #:k k takes the keyword #:k and binds k, and
;; in Python those are the one parameter, so they have to be the one name
(define (keyword-parameter keyword rest fail)
  (cond [(null? rest)
         (fail (format "the keyword ~a takes a parameter: write ~a name" keyword keyword))]
        [(not (symbol? (car rest)))
         (fail (format "the parameter of ~a is a name, and this is ~s" keyword (car rest)))]
        [else
         (define name (car rest))
         (define keyword-name (python-keyword-name keyword))
         (define parameter-name (python-name name))
         (unless (string=? keyword-name parameter-name)
           (fail (format "the keyword ~a is ~a in Python and its parameter ~a is ~a: write ~a ~a"
                         keyword keyword-name name parameter-name keyword keyword-name)))
         (values name (cdr rest))]))

;; two parameters that are one Python name are one parameter in the def, so a
;; parameter list that spells them differently is refused here, where both
;; names can be said
(define (check-once! names fail)
  (define seen '())
  (for ([name (in-list names)])
    (define python (python-name name))
    (define other (assoc python seen))
    (cond [(not other) (set! seen (cons (cons python name) seen))]
          [(eq? (cdr other) name)
           (fail (format "the parameter ~a is bound twice" name))]
          [else
           (fail (format "the parameters ~a and ~a are both ~a in Python: rename one of them"
                         (cdr other) name python))])))

;; the signature as data, in the one order: the positional parameters, then the
;; keywords, then the rest parameter
(define (signature-datum positional keywords rest)
  (define names (append positional (keyword-signature-datum keywords)))
  (if rest (append names rest) names))

(define (keyword-signature-datum keywords)
  (append* (for/list ([k (in-list keywords)])
             (list (keyword-param-keyword k) (keyword-param-name k)))))

;; the arguments of a call: (values the positional ones, the (keyword value)
;; pairs).  A keyword argument is one value, and two keywords that are one name
;; are one keyword to the procedure the call reaches, so both are refused here,
;; where the form can be named.  `keyword-name` says what a keyword is called:
;; a call to a procedure converts it, and a macro call -- whose keywords are
;; Racket's -- does not convert at all.
(define (argument-parts args [fail fail!] #:keyword-name [keyword-name python-keyword-name])
  (define (walk rest positional keywords)
    (cond [(null? rest) (values (reverse positional) (reverse keywords))]
          [(keyword? (syntax-e (car rest)))
           (define keyword (syntax-e (car rest)))
           (when (null? (cdr rest))
             (fail (format "the keyword ~a takes a value: write ~a value" keyword keyword)))
           (define name (keyword-name keyword))
           (define other
             (for/first ([k (in-list keywords)] #:when (equal? (keyword-name (car k)) name)) k))
           (when other
             (fail (if (eq? (car other) keyword)
                       (format "the keyword ~a is given twice" keyword)
                       (format "the keywords ~a and ~a are both ~a in Python: give it once"
                               (car other) keyword name))))
           (walk (cddr rest) positional (cons (list keyword (cadr rest)) keywords))]
          [else (walk (cdr rest) (cons (car rest) positional) keywords)]))
  (walk args '() '()))
