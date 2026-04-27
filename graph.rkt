#lang racket/base
;; Undirected graph: nodes are symbols, edges are unordered pairs.
;; Stored as a hash-of-node→set-of-neighbours.

(require racket/set racket/list)

(provide
 (struct-out graph)
 make-graph
 graph-add-edge!
 graph-nodes graph-edges graph-neighbours
 graph-n-nodes graph-n-edges
 induced-edges)

(struct graph (adj) #:mutable #:transparent)

(define (make-graph)
  (graph (make-hash)))

(define (ensure-node! g n)
  (define adj (graph-adj g))
  (unless (hash-has-key? adj n)
    (hash-set! adj n (mutable-set))))

(define (graph-add-edge! g u v)
  (ensure-node! g u)
  (ensure-node! g v)
  (set-add! (hash-ref (graph-adj g) u) v)
  (set-add! (hash-ref (graph-adj g) v) u))

(define (graph-nodes g)
  (sort (hash-keys (graph-adj g)) symbol<?))

(define (graph-edges g)
  ;; Canonical form: (sort u v) with symbol<?, deduplicated.
  (define seen (mutable-set))
  (define out '())
  (for ([(u nbrs) (in-hash (graph-adj g))])
    (for ([v (in-set nbrs)])
      (define key (if (symbol<? u v) (cons u v) (cons v u)))
      (unless (set-member? seen key)
        (set-add! seen key)
        (set! out (cons key out)))))
  out)

(define (graph-neighbours g n)
  (hash-ref (graph-adj g) n (lambda () (mutable-set))))

(define (graph-n-nodes g) (length (graph-nodes g)))
(define (graph-n-edges g) (length (graph-edges g)))

(define (induced-edges g members)
  ;; All edges of g where both endpoints are in `members`.
  (define member-set (if (set? members) members (list->set members)))
  (for/list ([e (in-list (graph-edges g))]
             #:when (and (set-member? member-set (car e))
                         (set-member? member-set (cdr e))))
    e))
