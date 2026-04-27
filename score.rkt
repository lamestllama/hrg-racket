#lang racket/base
;; Principled DL for a (graph, grammar) pair. Same shape as
;; /Users/eric/hrg/hrg/_engine.py:principled_dl, but the orbit-aut
;; discount and tentacle-orbit aut are stubbed to 0 (we'll add WL
;; refinement once the bootstrap loop is in place).

(require racket/set racket/list)
(require "graph.rkt" "grammar.rkt" "dl.rkt" "sexpr-dl.rkt")

(provide principled-dl sexpr-principled-dl)

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

;; ---------------------------------------------------------------
;; sexpr-principled-dl — DL on the s-expression representation, with
;; the cover encoded as a *partition* rather than a list of explicit
;; node names. The receiver knows the host graph G and the library;
;; the encoding tells them, in order, which of the still-uncovered
;; host-nodes each instance claims, then the bridge edges between
;; tentacle slots.
;;
;;   library-dl = sexpr-dl(library)
;;       The library is just integers — small.
;;   cover-dl = sum over instances of:
;;       log2(num-rules)     -- which template
;;     + log2_binom(R, k)    -- which subset of remaining R nodes
;;     where R is the number of uncovered nodes BEFORE this instance.
;;   bridges-dl = sum over (instance-pair) of:
;;       log2_binom(t_a * t_b, k)  -- which of the possible
;;                                    tentacle-slot pairs are bridged
;;
;; This is much cheaper than encoding each cover entry's nodes
;; independently — early instances pay log2(C(N, k)) but later ones
;; pay log2(C(small remaining, k)) which is small. Big templates
;; therefore have low per-instance cost AND fewer total cover entries
;; — both effects favour rich grammars.
;; ---------------------------------------------------------------

(define (instance-pair-key i j)
  (cond [(< i j) (cons i j)] [else (cons j i)]))

(define (sexpr-principled-dl G library cover)
  (define library-bits (sexpr-dl library))
  (define n-rules (max (length library) 1))
  (define n-nodes (graph-n-nodes G))

  (define live (filter (lambda (i) (positive? (set-count (instance-interior i))))
                       cover))
  (define sorted-cover
    (sort live
          (lambda (a b)
            (symbol<?
             (car (sort (set->list (instance-interior a)) symbol<?))
             (car (sort (set->list (instance-interior b)) symbol<?))))))

  ;; Cover bits, in two pieces:
  ;;  (a) partition cost — per instance, log2_binom(R, k) bits for
  ;;      which-of-the-R-uncovered-nodes-this-instance-claims.
  ;;  (b) rule-id sequence cost — KT-adaptive code over the multiset
  ;;      of rule-ids, total bits ~ log2(C(N + R, R)) where R is the
  ;;      effective alphabet (library + 1 for the 'singleton' fallback).
  ;;      This is much cheaper than log2(num-rules) per instance once
  ;;      the cover settles into a few common rules.
  (define-values (partition-bits final-remaining)
    (for/fold ([acc 0.0] [remaining n-nodes])
              ([inst (in-list sorted-cover)])
      (define k (set-count (instance-interior inst)))
      (values (+ acc (_log2_binom remaining k)) (- remaining k))))
  (define rule-id-bits
    (let* ([alphabet (max (+ n-rules 1) 2)]    ; +1 for singleton fallback
           [total (length sorted-cover)])
      (cond [(= total 0) 0.0]
            [else (_log2_binom (+ total alphabet -1) total)])))
  (define cover-bits (+ partition-bits rule-id-bits))

  ;; Bridges: encode in two layers.
  ;;  Layer 1: which (instance, instance) pairs are connected at all.
  ;;     log2_binom(C(N_inst, 2), num_bridged_pairs) bits.
  ;;  Layer 2: per bridged pair, log2_binom(t_a*t_b, k) — which
  ;;     tentacle-slot pairs carry edges.
  ;; Layer 1 is the dominant term and grows like (num_bridge_edges *
  ;; log2(num_pairs / num_bridges)) — adding templates that absorb
  ;; edges into instances reduces num_bridge_edges directly, which is
  ;; where the real description-length savings come from.
  (define inst-idx-of-node (make-hash))
  (for ([inst (in-list sorted-cover)] [idx (in-naturals)]
        #:when #t
        [v (in-set (instance-interior inst))])
    (hash-set! inst-idx-of-node v idx))
  (define bridges-by-pair (make-hash))
  (for ([e (in-list (graph-edges G))])
    (define ia (hash-ref inst-idx-of-node (car e) #f))
    (define ib (hash-ref inst-idx-of-node (cdr e) #f))
    (when (and ia ib (not (= ia ib)))
      (hash-update! bridges-by-pair (instance-pair-key ia ib)
                    (lambda (n) (+ n 1)) 0)))
  (define n-inst (length sorted-cover))
  (define n-pairs-possible (quotient (* n-inst (- n-inst 1)) 2))
  (define n-bridged-pairs (hash-count bridges-by-pair))
  (define which-pairs-bits
    (_log2_binom n-pairs-possible n-bridged-pairs))
  (define slot-pattern-bits
    (for/sum ([(pair k) (in-hash bridges-by-pair)])
      (define A (list-ref sorted-cover (car pair)))
      (define B (list-ref sorted-cover (cdr pair)))
      (define ta (max 1 (set-count (instance-tentacles A))))
      (define tb (max 1 (set-count (instance-tentacles B))))
      (_log2_binom (* ta tb) k)))
  (define bridges-bits (+ which-pairs-bits slot-pattern-bits))

  (define total (+ library-bits cover-bits bridges-bits))
  (hash 'total-dl total
        'morpheme-dl library-bits
        'message-dl cover-bits
        'mapping-dl bridges-bits))

;; helper — log2(C(n, k)), reused across modules; defined locally so
;; we don't add a require dependency just for one function.
(define (_log2_binom n k)
  (cond
    [(or (< k 0) (> k n) (= n 0)) 0.0]
    [(or (= k 0) (= k n)) 0.0]
    [else
     (- (_log2_factorial n)
        (_log2_factorial k)
        (_log2_factorial (- n k)))]))

(define (_log2_factorial n)
  (cond
    [(<= n 1) 0.0]
    [else
     (for/sum ([i (in-range 2 (+ n 1))])
       (/ (log i) (log 2)))]))
