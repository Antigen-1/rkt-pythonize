#lang rkt-pythonize
(define-syntax-rule (unless c body) (if c #f body))
(unless #f (print "ran"))
