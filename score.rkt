#lang racket/base
;; Principled DL for a (graph, grammar) pair. Same shape as
;; /Users/eric/hrg/hrg/_engine.py:principled_dl, but the orbit-aut
;; discount and tentacle-orbit aut are stubbed to 0 (we'll add WL
;; refinement once the bootstrap loop is in place).

(require racket/set racket/list)
(require "graph.rkt" "grammar.rkt" "dl.rkt")

(provide principled-dl)

(define (principled-dl G g)
  (define live (grammar-instances g))
  (define rules (grammar-rules g))
  (define n-rules (max (length rules) 1))
  (define lg-rules (log2 (max n-rules 2)))
  (define N (graph-n-nodes G))
  (define nbits (log2 (max N 2)))

  ;; --- Morpheme: per-template structure --------------------------
  (define rule-cost
    (for/sum ([r (in-list rules)])
      (define n (template-n-interior r))
      (define e (template-n-owned-edges r))
      (define p (template-n-tentacles r))
      (define max-edges (quotient (* n (- n 1)) 2))
      (+ (log-star n)
         (log2-binom max-edges e)
         (if (> p 0) (log2-binom n p) 0))))

  ;; Composition morpheme: stubbed (no compositions yet — bootstrap will
  ;; add them once non-trivial rules emerge).
  (define comp-rule-cost 0.0)

  ;; --- Message: per-instance rule-name --------------------------
  (define message-dl (* (length live) lg-rules))

  ;; --- Mapping: where to drop each instance into G --------------
  (define tpl-map
    (for/sum ([inst (in-list live)])
      (define p (set-count (instance-tentacles inst)))
      (if (= p 0) nbits (* p nbits))))

  ;; Composition mapping: stubbed.
  (define comp-map 0.0)

  (define morph (+ rule-cost comp-rule-cost))
  (define mapping (+ tpl-map comp-map))
  (define total (+ morph message-dl mapping))
  (hash 'total-dl total
        'morpheme-dl morph
        'message-dl message-dl
        'mapping-dl mapping))
