#lang racket/base
;; Greedy DL descent. From an initial cover, repeatedly try every
;; adjacent-pair merge; accept the move that lowers principled-DL the
;; most. Stop when no merge improves DL. Mirrors the merge sweep from
;; /Users/eric/hrg/hrg/_engine.py:descend.

(require racket/list)
(require "graph.rkt" "grammar.rkt" "score.rkt" "moves.rkt")

(provide descend)

(define (descend G cover #:max-sweeps [max-sweeps 50] #:verbose? [verbose? #f])
  (let loop ([cover cover]
             [sweep 0])
    (define gram (grammar-from-cover G cover))
    (define dl (hash-ref (principled-dl G gram) 'total-dl))
    (when verbose?
      (printf "  sweep ~a: ~a rules, ~a instances, DL=~a~n"
              sweep (length (grammar-rules gram)) (length cover)
              (real->decimal-string dl 1)))
    (cond
      [(>= sweep max-sweeps) (values cover dl)]
      [else
       (define-values (best-pair best-cover best-dl)
         (let next ([pairs (adjacent-instances G cover)]
                    [best-pair #f] [best-cover #f] [best-dl dl])
           (cond
             [(null? pairs) (values best-pair best-cover best-dl)]
             [else
              (define pair (car pairs))
              (define trial (try-merge G cover (car pair) (cdr pair)))
              (define trial-gram (grammar-from-cover G trial))
              (define trial-dl
                (hash-ref (principled-dl G trial-gram) 'total-dl))
              (cond
                [(< trial-dl best-dl)
                 (next (cdr pairs) pair trial trial-dl)]
                [else (next (cdr pairs) best-pair best-cover best-dl)])])))
       (cond
         [best-pair (loop best-cover (+ sweep 1))]
         [else (values cover dl)])])))
