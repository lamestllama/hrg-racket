#lang racket/base
;; Entry point. Loads a DOT file, builds a trivial cover, computes DL.
;; This is the "starting program" — the seed grammar that future
;; iterations will let modify itself.
;;
;; Usage:  racket main.rkt path/to/network.dot

(require racket/cmdline)
(require "dot.rkt" "graph.rkt" "grammar.rkt" "score.rkt"
         "descent.rkt")

(define verbose? (make-parameter #f))
(define dot-path
  (command-line #:program "hrg-racket"
                #:once-each
                [("-v" "--verbose") "Print sweep-by-sweep progress"
                                    (verbose? #t)]
                #:args (path) path))

(define G (load-dot dot-path))
(printf "loaded ~a: ~a nodes, ~a edges~n"
        dot-path (graph-n-nodes G) (graph-n-edges G))

(define seed (leaf-absorbing-cover G))
(define seed-gram (grammar-from-cover G seed))
(define seed-dl (principled-dl G seed-gram))
(printf "seed cover: ~a rules, ~a instances, DL=~a bits~n"
        (length (grammar-rules seed-gram))
        (length (grammar-instances seed-gram))
        (real->decimal-string (hash-ref seed-dl 'total-dl) 1))

(define-values (final-cover final-dl)
  (descend G seed #:verbose? (verbose?)))
(define final-gram (grammar-from-cover G final-cover))
(printf "after descent: ~a rules, ~a instances, DL=~a bits~n"
        (length (grammar-rules final-gram))
        (length (grammar-instances final-gram))
        (real->decimal-string final-dl 1))
(for ([r (in-list (sort (grammar-rules final-gram)
                         (lambda (a b)
                           (> (template-n-interior a)
                              (template-n-interior b)))))])
  (printf "  R~a: interior=~an, tentacles=~an, owned=~ae~n"
          (template-rule-id r)
          (template-n-interior r)
          (template-n-tentacles r)
          (template-n-owned-edges r)))
