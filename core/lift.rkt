#lang racket/base

;; LE -> LL.
;;
;; LE has lambdas with any body, and procedures defined where they stand.  LL
;; has neither: every procedure is a top-level define, and a procedure that
;; refers to a variable of an enclosing scope takes that variable as a leading
;; parameter.  So a lambda becomes a reference to the define it was lifted to --
;; with the variables it captures passed in -- and no def is nested in a def.
;;
;; The variables a procedure captures are found by walking its body: a name the
;; enclosing scopes bind to a variable is captured, a name the body binds itself
;; (a parameter, or a define) is not, and a name the program never binds is a
;; Python global, which a top-level def can reach on its own.

(require racket/list)

(provide lift-program)

;; what a name is bound to: a variable of an enclosing scope, a lifted
;; procedure, the fact that the program defines it at the top level (a Python
;; global, so nothing to pass), or a binder of the form being walked
(struct var (id) #:transparent)
(struct proc (lifted params rest captured) #:mutable #:transparent)
(struct inner () #:transparent)
(define inner-binding (inner))

(define counter 0)
(define (next-id) (set! counter (add1 counter)) counter)

(define (fresh base)
  (string->symbol
   (format "_lift~a~a" (next-id) (if (eq? base 'lambda) "" (format "_~a" base)))))

(define (head stx) (and (pair? (syntax->list stx)) (syntax-e (car (syntax->list stx)))))
(define (mk stx datum) (datum->syntax stx datum))
(define (lookup env sym) (cond [(assoc sym env) => cdr] [else #f]))
(define (define? stx) (eq? (head stx) 'define))
(define (statement? stx) (memq (head stx) '(define set!)))

(define (form-location stx)
  (define line (syntax-line stx))
  (if line
      (format "~a:~a:~a" (syntax-source stx) line (add1 (syntax-column stx)))
      "?"))

(define (not-le stx message)
  (error 'lift "~a: ~a: ~a" (form-location stx) message (syntax->datum stx)))

;; (x y . rest) -> the parameters, and the rest parameter or #f
(define (list-params sig)
  (define (walk rest acc)
    (cond [(null? rest) (values (reverse acc) #f)]
          [(pair? rest) (walk (cdr rest) (cons (syntax-e (car rest)) acc))]
          [else (values (reverse acc) (syntax-e rest))]))
  (walk sig '()))

;; a define target is a signature: (f x y . rest)
(define (param-names target)
  (list-params (cdr (syntax-e target))))

(define (target-name target)
  (if (identifier? target) (syntax-e target) (syntax-e (car (syntax-e target)))))

;; ---- what a form captures

(define (captures stx env)
  (define found '())
  (define (note sym binding)
    (unless (assoc sym found)
      (set! found (cons (cons sym (var-id binding)) found))))
  (define (walk stx env)
    (cond [(identifier? stx)
           (define binding (lookup env (syntax-e stx)))
           (when (var? binding) (note (syntax-e stx) binding))]
          [(not (syntax->list stx)) (void)]
          [else
           (define parts (syntax->list stx))
           (case (head stx)
             [(quote) (void)]
             [(lambda) (walk-body (cddr parts) (bind-lambda-params (cadr parts) env))]
             [(define)
              (define target (cadr parts))
              (cond [(identifier? target) (walk (caddr parts) env)]
                    [else (walk-body (cddr parts) (bind-define-params target env))])]
             [else (for ([p (in-list parts)]) (walk p env))])]))
  (define (bind-names params rest env)
    (define env* (for/fold ([env env]) ([p (in-list params)])
                   (cons (cons p inner-binding) env)))
    (if rest (cons (cons rest inner-binding) env*) env*))
  (define (bind-lambda-params target env)
    (define-values (params rest) (list-params (syntax-e target)))
    (bind-names params rest env))
  (define (bind-define-params target env)
    (define-values (params rest) (param-names target))
    (bind-names params rest env))
  (define (walk-body forms env)
    ;; a body binds what it defines: its own names are not a capture
    (define env* (for/fold ([env env]) ([f (in-list forms)] #:when (define? f))
                   (cons (cons (target-name (cadr (syntax->list f))) inner-binding) env)))
    (for ([f (in-list forms)]) (walk f env*)))
  (walk stx env)
  (reverse found))

;; ---- lifting

(define lifted-defs '())
(define (add-def stx) (set! lifted-defs (append lifted-defs (list stx))))

(define (lift-program forms)
  (set! counter 0)
  (set! lifted-defs '())
  ;; a name the program defines at the top level is a Python global: a lifted
  ;; def reaches it by name, so it is not passed in
  (define env (for/list ([f (in-list forms)] #:when (define? f))
                (cons (target-name (cadr (syntax->list f))) 'global)))
  (define statements (for/list ([f (in-list forms)]) (lift-top f env)))
  (append lifted-defs statements))

(define (lift-top f env)
  (cond [(not (define? f)) (lift f env '())]
        [else
         (define target (cadr (syntax->list f)))
         (cond [(identifier? target)
                (mk f (list 'define target (lift (caddr (syntax->list f)) env '())))]
               [else
                ;; a top-level procedure keeps its name
                (define-values (params rest) (param-names target))
                (when (null? (cddr (syntax->list f))) (not-le f "a procedure needs a body"))
                (define env* (bind-params params rest env))
                (define body (lift-body (cddr (syntax->list f)) env* '()))
                (mk f (list* 'define
                             (mk f (cons (target-name target)
                                         (if rest (append params rest) params)))
                             body))])]))

(define (bind-params params rest env)
  (define env* (for/fold ([env env]) ([p (in-list params)])
                 (cons (cons p (var (next-id))) env)))
  (if rest (cons (cons rest (var (next-id))) env*) env*))

;; a body: name the procedures it defines first (so they can call each other),
;; work out what each of them captures, then lift them and the body itself
(define (lift-body forms env current)
  (define-values (env* procs) (bind-lift forms env))
  (for ([entry (in-list procs)])
    (set-proc-captured! (cadr entry) (captures (car entry) env*)))
  (for ([entry (in-list procs)]) (lift-proc entry env*))
  (for/list ([f (in-list forms)]) (lift f env* current)))

(define (bind-lift forms env)
  (for/fold ([env env] [procs '()]) ([f (in-list forms)] #:when (define? f))
    (define target (cadr (syntax->list f)))
    (cond [(identifier? target)
           (values (cons (cons (syntax-e target) (var (next-id))) env) procs)]
          [else
           (define-values (params rest) (param-names target))
           (define p (proc (fresh (target-name target)) params rest '()))
           (values (cons (cons (target-name target) p) env) (append procs (list (list f p))))])))

(define (lift-proc entry env)
  (define f (car entry))
  (define p (cadr entry))
  (define body-forms (cddr (syntax->list f)))
  (when (null? body-forms) (not-le f "a procedure needs a body"))
  (define captured (proc-captured p))
  (define names (map car captured))
  (define env* (bind-params (proc-params p) (proc-rest p) env))
  (define all-params (append names (proc-params p)))
  (add-def (mk f (list* 'define
                        (mk f (cons (proc-lifted p)
                                    (if (proc-rest p)
                                        (append all-params (proc-rest p))
                                        all-params)))
                        (lift-body body-forms env* captured)))))

;; the value of a lifted procedure where it is used as one: the define itself
;; if it captures nothing, and a lambda that passes what it captures otherwise
(define (reference stx lifted names params rest)
  (cond [(null? names) (mk stx lifted)]
        [else
         (define target (if rest (append params rest) params))
         (define call (if rest
                          (list 'apply lifted (cons 'list (append names params (list rest))))
                          (cons lifted (append names params))))
         (mk stx (list 'lambda target call))]))

(define (lift stx env current)
  (cond
    [(identifier? stx) stx]
    [(not (syntax->list stx)) stx]
    [else
     (define parts (syntax->list stx))
     (case (head stx)
       [(quote) stx]
       [(lambda)
        (define target (cadr parts))
        (define body (cddr parts))
        (when (null? body) (not-le stx "a lambda needs a body"))
        (define-values (params rest) (list-params (syntax-e target)))
        (define captured (captures stx env))
        (define names (map car captured))
        (define lifted (fresh 'lambda))
        (define env* (bind-params params rest env))
        (define all-params (append names params))
        (add-def (mk stx (list* 'define
                                (mk stx (cons lifted (if rest (append all-params rest) all-params)))
                                (lift-body body env* captured))))
        (reference stx lifted names params rest)]
       [(define)
        (define target (cadr parts))
        (cond [(identifier? target)
               (mk stx (list 'define target (lift (caddr parts) env current)))]
              [else
               ;; the lifted procedure under its own local name
               (define p (lookup env (target-name target)))
               (mk stx (list 'define (target-name target)
                             (reference stx (proc-lifted p) (map car (proc-captured p))
                                        (proc-params p) (proc-rest p))))])]
       [(set!)
        (check-set! stx env current)
        (mk stx (list 'set! (cadr parts) (lift (caddr parts) env current)))]
       [(with-handler)
        (mk stx (list* 'with-handler (lift (cadr parts) env current)
                       (lift-thunk (cddr parts) stx env current)))]
       [(trampoline)
        (mk stx (list* 'trampoline (lift-trampoline (cdr parts) stx env current)))]
       [else
        (define f (car parts))
        (define binding (and (identifier? f) (lookup env (syntax-e f))))
        (define args (for/list ([a (in-list (cdr parts))]) (lift a env current)))
        (cond [(proc? binding)
               (check-captures stx binding env)
               (mk stx (cons (proc-lifted binding)
                             (append (map car (proc-captured binding)) args)))]
              [else (mk stx (cons (lift f env current) args))])])]))

;; with-handler takes a thunk, so its body becomes one: a lone lambda already
;; is one, and anything else -- including a single expression -- is wrapped
(define (lift-thunk forms stx env current)
  (cond [(and (null? (cdr forms)) (eq? (head (car forms)) 'lambda))
         (list (lift (car forms) env current))]
        [else (list (lift (mk stx (list* 'lambda '() forms)) env current))]))

;; trampoline takes the function to call, so a lone expression stays as it is
;; and more than one body becomes a thunk that runs them
(define (lift-trampoline forms stx env current)
  (cond [(and (null? (cdr forms)) (not (statement? (car forms))))
         (list (lift (car forms) env current))]
        [else (list (lift (mk stx (list* 'lambda '() forms)) env current))]))

;; the captures of the lifted procedure have to be the variables in scope here,
;; or the value passed in would be the wrong one
(define (check-captures stx binding env)
  (for ([c (in-list (proc-captured binding))])
    (define here (lookup env (car c)))
    (unless (and (var? here) (= (var-id here) (cdr c)))
      (not-le stx (format "~a captures ~a, which is shadowed here; rename one of them"
                          (proc-lifted binding) (car c))))))

(define (check-set! stx env current)
  (define target (cadr (syntax->list stx)))
  (when (identifier? target)
    (define captured (assoc (syntax-e target) current))
    (when captured
      (define here (lookup env (syntax-e target)))
      (when (and (var? here) (= (var-id here) (cdr captured)))
        (not-le stx (format "a lifted procedure cannot set! ~a: it captures it"
                            (syntax-e target)))))))
