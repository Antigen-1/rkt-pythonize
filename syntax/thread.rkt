#lang racket/base

;; LE -> LE, expanding the threading operators into the calls they mean, run by
;; a trampoline so that a chain is flat however long it is.
;;
;;   (-> x (f a) (g b))    threads x into the first position of a step: (g (f x a) b)
;;   (->> x (f a) (g b))   threads it into the last positional argument instead
;;
;; A step is a call or a bare name, and the value of the chain is what the last
;; step makes of the value before it.
;;
;; An exception interrupts.  A step whose value is an instance of Exception --
;; or of a subclass of it -- ends the chain there and the chain's value is that
;; instance: the steps after it do not run.  Nothing else interrupts, so what the
;; last step makes is handed back in a one-element list and the driver takes it
;; out -- which is what keeps a value that happens to be callable from being
;; mistaken for the next step, and what keeps an exception value from being
;; confused with a chain that ran to its end.
;;
;; The trampoline is what runs the chain.  A step that is not the last answers
;; the call that makes the next one, a procedure of no arguments; None is not
;; one, so the trampoline stops there; and the last step answers the list.  Each
;; step returns before the next is called, so the Python is flat however many
;; steps there are -- that is the tail-call optimization the trampoline gives.

(require racket/list
         "../core/names.rkt"
         "../core/params.rkt")

(provide expand-thread-program)

(define (head stx) (and (pair? (syntax->list stx)) (syntax-e (car (syntax->list stx)))))
(define (mk stx datum) (datum->syntax stx datum))

(define (form-location stx)
  (define line (syntax-line stx))
  (if line
      (format "~a:~a:~a" (syntax-source stx) line (add1 (syntax-column stx)))
      "?"))

(define (not-le stx message)
  (error 'thread "~a: ~a: ~a" (form-location stx) message (syntax->datum stx)))

(define (expand-thread-program forms)
  (for/list ([f (in-list forms)]) (expand f)))

(define (expand stx)
  (cond [(not (syntax->list stx)) stx]
        [else
         (define parts (syntax->list stx))
         (case (head stx)
           [(quote) stx]
           [(->) (expand (thread stx (cdr parts) #t))]
           [(->>) (expand (thread stx (cdr parts) #f))]
           ;; a parameter list is not a form
           [(lambda)
            (if (< (length parts) 2)
                stx
                (mk stx (list* 'lambda (cadr parts)
                               (for/list ([f (in-list (cddr parts))]) (expand f)))))]
           [(define)
            (cond [(< (length parts) 3) stx] ; check-expression says what is wrong
                  [(identifier? (cadr parts))
                   (mk stx (list 'define (cadr parts) (expand (caddr parts))))]
                  [else
                   (mk stx (list* 'define (cadr parts)
                                  (for/list ([f (in-list (cddr parts))]) (expand f))))])]
           [else (mk stx (for/list ([p (in-list parts)]) (expand p)))])]))

;; (-> value step ...): the chain, as a driver that unwraps what the last step
;; left in the list the trampoline stopped at
(define (thread stx parts first?)
  (when (null? parts) (not-le stx "a threading form takes a value to thread"))
  (define value (car parts))
  (define steps (cdr parts))
  (cond [(null? steps) value]
        [else
         (define taken (symbols-of stx))
         (define boxed (temporary-name 'boxed taken))
         (mk stx
             (list (mk stx (list 'lambda (mk stx (list boxed))
                                 (mk stx (list 'if (mk stx (list 'isinstance boxed 'Exception))
                                               boxed
                                               (mk stx (list 'object-ref boxed 0))))))
                   (mk stx (list 'trampoline
                                 (mk stx (list (step-lambda stx steps value taken first?)
                                               value))))))]))

;; a step: a procedure of the value, which names what the step makes of it.
;; An exception there ends the chain; the last step leaves the value in a list; any
;; other step answers the call that makes the next one, which is a procedure of
;; no arguments -- the trampoline calls it, and it returns before the step it
;; calls does.
(define (step-lambda stx steps value taken first?)
  (define step (car steps))
  (define rest (cdr steps))
  (define param (temporary-name 'value taken))
  (define result (temporary-name 'result taken))
  (define made (step-call stx step param first?))
  (define answer
    (if (null? rest)
        (mk stx (list 'list result))
        (mk stx (list 'lambda '()
                      (mk stx (list (step-lambda stx rest result taken first?) result))))))
  (mk stx (list 'lambda (mk stx (list param))
                (mk stx (list 'define result made))
                (mk stx (list 'if (mk stx (list 'isinstance result 'Exception))
                              result
                              answer)))))

;; the step with the value threaded in: -> puts it first, ->> puts it after the
;; positional arguments and before the keyword ones, which is the last positional
;; argument a Python call can take
(define (step-call stx step value first?)
  (cond [(identifier? step) (mk stx (list step value))]
        [(keyword? (syntax-e step))
         (not-le stx "a step is a call or a name, not a keyword")]
        [(not (syntax->list step)) (not-le stx "a step is a call or a name")]
        [(eq? (head step) 'quote) (not-le stx "a step is a call or a name, not data")]
        [else
         (define parts (syntax->list step))
         (define f (car parts))
         (cond [first? (mk stx (cons f (cons value (cdr parts))))]
               [else
                (define-values (positional keywords)
                  (argument-parts (cdr parts) (lambda (message) (not-le step message))))
                ;; a keyword argument is two elements of the call, as it is
                ;; written: the keyword and the value it carries
                (mk stx (append (cons f positional)
                                (list value)
                                (append* (for/list ([keyword (in-list keywords)])
                                           (list (car keyword) (cadr keyword))))))])]))

;; every name the form writes, so that a name a generated form binds is one the
;; program did not write
(define (symbols-of stx)
  (define found '())
  (define (walk x)
    (cond [(symbol? x) (unless (memq x found) (set! found (cons x found)))]
          [(pair? x) (walk (car x)) (walk (cdr x))]
          [(vector? x) (for ([v (in-vector x)]) (walk v))]
          [else (void)]))
  (walk (syntax->datum stx))
  found)
