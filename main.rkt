#lang racket/base
;; Entry point. Loads a DOT file, builds a trivial cover, computes DL.
;; This is the "starting program" — the seed grammar that future
;; iterations will let modify itself.
;;
;; Usage:  racket main.rkt path/to/network.dot

(require racket/cmdline)
(require "dot.rkt" "graph.rkt" "grammar.rkt" "score.rkt")

(define dot-path
  (command-line #:program "hrg-racket"
                #:args (path) path))

(define G (load-dot dot-path))
(printf "loaded ~a: ~a nodes, ~a edges~n"
        dot-path (graph-n-nodes G) (graph-n-edges G))

(define cover (trivial-cover G))
(define gram (grammar-from-cover G cover))
(define dl (principled-dl G gram))
(printf "trivial cover: ~a rules, ~a instances, DL=~a bits~n"
        (length (grammar-rules gram))
        (length (grammar-instances gram))
        (real->decimal-string (hash-ref dl 'total-dl) 1))
(printf "  morpheme=~a  message=~a  mapping=~a~n"
        (real->decimal-string (hash-ref dl 'morpheme-dl) 1)
        (real->decimal-string (hash-ref dl 'message-dl) 1)
        (real->decimal-string (hash-ref dl 'mapping-dl) 1))
