#lang racket/base
;; Greedy template-matching with tentacle-aware canonical fingerprints,
;; with a per-graph candidate cache.
;;
;; Each recognise call inside the bootstrap loop ran enumerate-connected-
;; subsets + tentacle-set + WL fingerprint from scratch — even though the
;; underlying graph never changes within a run. With ~30 candidate
;; templates per round × ~6 rounds, that's 180 redundant passes.
;;
;; The fix: a (graph → k → entries) cache, populated lazily, where each
;; entry is (nodes induced e-count tents fp). recognise just iterates the
;; cached entries, filters by `covered`, and looks up fingerprints in a
;; template hash. Bootstrap creates one cache at start and reuses it.

(require racket/set racket/list)
(require "graph.rkt" "grammar.rkt" "canonical.rkt")

(provide recognise-cover make-recogniser-cache)

(define (template-size t) (car t))
(define (template-tentacle-indices t) (cadr t))
(define (template-edges t) (caddr t))

(define (make-recogniser-cache)
  (make-hash))

(define (cache-entries-for-size G k cache)
  (cond
    [(hash-has-key? cache k) (hash-ref cache k)]
    [else
     (define entries
       (for/list ([nodes (in-list (enumerate-connected-subsets G k))])
         (define induced (induced-edges G nodes))
         (define e-count (length induced))
         (define tents (compute-tentacle-set G nodes))
         (define fp (induced-canonical-fingerprint G nodes tents))
         (list nodes induced e-count tents fp)))
     (hash-set! cache k entries)
     entries]))

(define (recognise-cover G library #:cache [cache (make-recogniser-cache)])
  (cond
    [(null? library)
     (for/list ([v (in-list (graph-nodes G))] [i (in-naturals)])
       (instance i (set v) (set) (set) -1))]
    [else
     (define template-by-fp (make-hash))
     (for ([t (in-list library)])
       (hash-set! template-by-fp (template-fingerprint t) t))
     (define sizes
       (sort
        (remove-duplicates
         (for/list ([t (in-list library)]) (template-size t)))
        >))
     (define covered (mutable-set))
     (define instances '())
     (define iid 0)

     (for ([k (in-list sizes)])
       (define entries (cache-entries-for-size G k cache))
       (for ([entry (in-list entries)])
         (define nodes (car entry))
         (when (andmap (lambda (v) (not (set-member? covered v))) nodes)
           (define fp (list-ref entry 4))
           (when (hash-has-key? template-by-fp fp)
             (define interior (list->set nodes))
             (define owned (list->set (cadr entry)))
             (set! instances
                   (cons (instance iid interior (set) owned -1) instances))
             (set! iid (+ iid 1))
             (for ([v (in-list nodes)])
               (set-add! covered v))))))

     (for ([v (in-list (graph-nodes G))]
           #:unless (set-member? covered v))
       (set! instances
             (cons (instance iid (set v) (set) (set) -1) instances))
       (set! iid (+ iid 1)))

     (reverse instances)]))
