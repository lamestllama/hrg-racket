#lang racket/base
(require racket/set racket/list)
(require "dot.rkt" "graph.rkt" "grammar.rkt" "score.rkt" "recognise.rkt")

(define G (load-dot "examples/elecnet.dot"))

(define (show tag lib)
  (define cov (recognise-cover G lib))
  (define dl (sexpr-principled-dl G lib cov))
  (printf "~a: total=~a morph=~a cover=~a bridge=~a~n"
          tag
          (real->decimal-string (hash-ref dl 'total-dl) 1)
          (real->decimal-string (hash-ref dl 'morpheme-dl) 1)
          (real->decimal-string (hash-ref dl 'message-dl) 1)
          (real->decimal-string (hash-ref dl 'mapping-dl) 1)))

(show "empty " '())
(show "edge  " '((2 (1) ((0 . 1)))))
(show "edge+V" '((2 (1) ((0 . 1))) (3 (0) ((0 . 1) (0 . 2)))))
