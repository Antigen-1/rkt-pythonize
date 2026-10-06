#lang racket/base

;; The name tests: core/names.rkt is the one module that says what a Scheme
;; name is in Python, so what it says is pinned here, character by character,
;; in both styles -- and the names the compiler makes up for itself too.
;;
;; The rules, and the table below:
;;
;;   - becomes _ (or capitalizes the letter after it), ? becomes _p (or P),
;;   ! becomes _b (or B), a name that is a Python keyword takes a trailing _,
;;   and a name that starts with a digit takes a leading _
;;
;; tests/dsl.rkt renders whole programs and runs them; this file is the module
;; underneath them.

(require rackunit
         racket/list
         "../core/names.rkt")

(define (snake-name sym) (parameterize ([python-name-style 'snake]) (python-name sym)))
(define (camel-name sym) (parameterize ([python-name-style 'camel]) (python-name sym)))
(define (snake-keyword keyword)
  (parameterize ([python-name-style 'snake]) (python-keyword-name keyword)))
(define (camel-keyword keyword)
  (parameterize ([python-name-style 'camel]) (python-keyword-name keyword)))

(module+ test
  (test-case "the style a conversion is in is a parameter, and snake is its default"
    (check-equal? (python-name-style) 'snake)
    (check-equal? (python-name 'even?) "even_p")
    (check-equal? (parameterize ([python-name-style 'camel]) (python-name 'even?)) "isEven")
    (check-exn exn:fail? (lambda () (python-name-style 'nonsense))))

  (test-case "a name is converted character by character, in both styles"
    ;; name                 snake            camel
    (define cases
      '((x                  "x"              "x")
        (even?              "even_p"         "isEven")
        (set-car!           "set_car_b"      "setCarB")
        (object-ref         "object_ref"     "objectRef")
        (object-has-attr?   "object_has_attr_p" "isObjectHasAttr")
        (my-name            "my_name"        "myName")
        (a-b-c              "a_b_c"          "aBC")
        (a--b               "a__b"           "a_B")
        (-a                 "_a"             "A")
        (a-                 "a_"             "a_")
        (-                  "_"              "_")
        (--                 "__"             "__")
        (a-b-               "a_b_"           "aB_")
        (a1-b2              "a1_b2"          "a1B2")
        (a-1                "a_1"            "a_1")
        (a?                 "a_p"            "isA")
        (a!                 "a_b"            "aB")
        (a-?                "a__p"           "isA_")
        (a!b                "a_bb"           "aBb")
        (a?b?               "a_pb_p"         "isAPb")
        (keep_case          "keep_case"      "keep_case")
        (CamelCase          "CamelCase"      "CamelCase")))
    (for ([case (in-list cases)])
      (define-values (sym expected-snake expected-camel) (apply values case))
      (check-equal? (snake-name sym) expected-snake (format "~a, snake" sym))
      (check-equal? (camel-name sym) expected-camel (format "~a, camel" sym))))

  (test-case "with camel a trailing ? names a predicate"
    (check-equal? (camel-name 'even?) "isEven")
    (check-equal? (camel-name 'my-func?) "isMyFunc")
    (check-equal? (camel-name 'object-has-attr?) "isObjectHasAttr")
    (check-equal? (camel-name 'is-even?) "isIsEven")
    (check-equal? (camel-name '?) "is_")
    ;; a ? that is not the end of a name is not one
    (check-equal? (camel-name 'a?b) "aPb")
    ;; and with snake a ? is _p, wherever it is
    (check-equal? (snake-name 'even?) "even_p")
    (check-equal? (snake-name 'a?b) "a_pb"))

  (test-case "a Python keyword takes a trailing _ in either style"
    (for ([case (in-list '((class "class_") (lambda "lambda_") (None "None_")
                           (True "True_") (is "is_") (in "in_") (pass "pass_")
                           (yield "yield_") (global "global_") (async "async_")))])
      (check-equal? (snake-name (car case)) (cadr case))
      (check-equal? (camel-name (car case)) (cadr case)))
    ;; and a name that only becomes a keyword is not one
    (check-equal? (snake-name 'class-) "class_")
    (check-equal? (snake-name 'classy) "classy"))

  (test-case "a name that starts with a digit takes a leading _"
    (for ([case (in-list '((1st "_1st") (|9| "_9") (1-2 "_1_2") (2x-y "_2x_y")))])
      (check-equal? (snake-name (car case)) (cadr case) (format "~a, snake" (car case))))
    (check-equal? (camel-name '1st) "_1st")
    (check-equal? (camel-name '2x-y) "_2xY"))

  (test-case "a keyword argument's name is converted as a name is"
    (check-equal? (snake-keyword '#:foo-bar) "foo_bar")
    (check-equal? (camel-keyword '#:foo-bar) "fooBar")
    (check-equal? (snake-keyword '#:even?) "even_p")
    (check-equal? (camel-keyword '#:even?) "isEven")
    (check-equal? (snake-keyword '#:class) "class_")
    (check-equal? (snake-keyword '#:1st) "_1st")
    (check-equal? (snake-keyword '#:k) "k"))

  (test-case "a prefix and a suffix go around the conversion of the name"
    (check-equal? (python-name 'list #:prefix "_lift1_") "_lift1_list")
    (check-equal? (python-name 'even? #:suffix "_x") "even_p_x")
    (check-equal? (python-name 'x #:prefix "a_" #:suffix "_b") "a_x_b")
    ;; what is converted is the name, so what it is in Python is inside them
    (check-equal? (python-name 'class #:prefix "a_") "a_class_")
    (check-equal? (python-name 'x-y #:prefix "p_") "p_x_y")
    (check-equal? (parameterize ([python-name-style 'camel])
                    (python-name 'object-ref #:prefix "_lift1_"))
                  "_lift1_objectRef"))

  (test-case "the conversion of a Python name is that name"
    ;; which is what lets a pass convert a name it made up, and write it where
    ;; the program says, and get the same name back
    (define names '(x even? set-car! object-ref object-has-attr? my-name a--b -a a-
                    class 1st a? a! keep_case))
    (for ([style (in-list '(snake camel))])
      (parameterize ([python-name-style style])
        (for ([sym (in-list names)])
          (define once (python-name sym))
          (check-equal? (python-name (string->symbol once)) once
                        (format "~a in ~a" sym style))))))

  (test-case "a name the compiler makes up says so, and where it came from"
    (define lambda-name (generated-name #f))
    (define inner-name (generated-name 'inner))
    (define object-name (generated-name 'object-ref))
    (check-equal? generated-prefix "_lift_")
    ;; a name a program writes is not one of these: a UUID is not written down
    (check-true (regexp-match? #px"^_lift_[0-9a-f]{8}$" (symbol->string lambda-name)))
    (check-true (regexp-match? #px"^_lift_[0-9a-f]{8}_inner$" (symbol->string inner-name)))
    (check-true (regexp-match? #px"^_lift_[0-9a-f]{8}_object_ref$" (symbol->string object-name)))
    (parameterize ([python-name-style 'camel])
      (check-true (regexp-match? #px"^_lift_[0-9a-f]{8}_objectRef$"
                                 (symbol->string (generated-name 'object-ref)))))
    ;; two of them are two names, and each is a Python name already, which the
    ;; conversion of the program gives back
    (check-not-equal? (generated-name #f) (generated-name #f))
    (for ([name (in-list (list lambda-name inner-name object-name))])
      (check-equal? (python-name name) (symbol->string name))))

  (test-case "a piece's name is the prelude's prefix and the piece's own"
    (parameterize ([prelude-prefix ""])
      (check-equal? (piece-name 'list) "list")
      (check-equal? (piece-name 'object-ref) "object_ref")
      (check-equal? (piece-name 'object-set!) "object_set_b")
      (check-equal? (piece-name 'object-has-attr?) "object_has_attr_p")
      (check-equal? (piece-name 'keyword-apply) "keyword_apply")
      (check-equal? (piece-name 'begin) "begin")
      (check-equal? (piece-name 'raise) "raise_")
      (check-equal? (piece-name 'trampoline) "trampoline")
      (check-equal? (piece-name 'with-handler) "with_handler")
      (check-equal? (piece-name 'import) "import_module"))
    (parameterize ([prelude-prefix "_pz_"])
      (check-equal? (piece-name 'list) "_pz_list")
      (check-equal? (piece-name 'object-ref) "_pz_object_ref")
      (check-equal? (piece-name 'begin) "_pz_begin")
      (check-equal? (piece-name 'raise) "_pz_raise_")
      (parameterize ([python-name-style 'camel])
        (check-equal? (piece-name 'object-ref) "_pz_objectRef")
        (check-equal? (piece-name 'keyword-apply) "_pz_keywordApply"))
      ;; the runtime's own names are the prelude's and do not follow the style
      (check-equal? (piece-name 'begin) "_pz_begin"))
    ;; the prefix is a parameter, and by default it is one the compiler makes up
    (check-true (string? (prelude-prefix)))
    ;; a UUID's first bytes, so two runs and two programs do not share one
    (check-true (regexp-match? #px"^_[0-9a-f]{8}_$" (prelude-prefix)))
    (check-exn exn:fail? (lambda () (prelude-prefix 1)))
    (check-equal? (parameterize ([prelude-prefix "x_"]) (piece-name 'list)) "x_list")
    ;; every piece has a name, and the pieces that answer to an LE name are the
    ;; ones a program may use
    (for ([piece (in-list piece-order)])
      (check-true (string? (piece-name piece))))
    (check-equal? (sort (hash-keys runtime-pieces) symbol<?)
                  (sort '(list apply keyword-apply object-ref object-set!
                          object-get-attr object-set-attr! object-has-attr?)
                        symbol<?)))

  (test-case "the compiler calls a piece by a name of its own"
    ;; the name the compiler writes where it needs a piece itself, which a
    ;; program's own apply cannot take
    (check-equal? (piece-call 'apply) 'prelude-apply)
    (check-eq? (called-piece 'prelude-apply) 'apply)
    (check-eq? (called-piece 'prelude-list) 'list)
    ;; the piece's own name is the program's to write, and is not one of these
    (check-false (called-piece 'apply))
    (check-false (called-piece 'list)))

  (test-case "a name the runtime already has is known, and one it does not is not"
    (check-true (known-runtime-name? 'print))
    (check-true (known-runtime-name? 'list))
    (check-true (known-runtime-name? 'object-ref))
    (check-true (known-runtime-name? 'len))
    (check-false (known-runtime-name? 'nowhere))
    ;; the builtins are Python names, and it is the conversion of a name that
    ;; is looked up in them: isinstance is one, is-instance is is_instance
    (check-true (known-runtime-name? 'isinstance))
    (check-false (known-runtime-name? 'is-instance))
    (parameterize ([python-name-style 'camel])
      (check-true (known-runtime-name? 'object-ref))
      (check-false (known-runtime-name? 'nowhere)))))
