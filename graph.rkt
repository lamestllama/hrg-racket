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
 induced-edges
 connected-subset?
 enumerate-connected-subsets)

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

(define (connected-subset? g nodes)
  (cond
    [(null? nodes) #t]
    [else
     (define ns (list->set nodes))
     (let bfs ([reached (set (car nodes))] [stack (list (car nodes))])
       (cond
         [(null? stack) (= (set-count reached) (length nodes))]
         [else
          (define n (car stack))
          (define new
            (for/list ([m (in-set (graph-neighbours g n))]
                       #:when (and (set-member? ns m)
                                   (not (set-member? reached m))))
              m))
          (bfs (set-union reached (list->set new))
               (append (cdr stack) new))]))]))

(define (enumerate-connected-subsets g k #:among [among #f])
  ;; Enumerate all connected node-subsets of size k. If `among` is a
  ;; set, restrict to subsets all of whose nodes are in `among`. Each
  ;; subset is canonical: built starting from its lex-smallest node
  ;; and only extending to neighbours strictly greater than the
  ;; starting node OR connected through the existing subset, deduped
  ;; via a `seen` hash.
  (define out '())
  (define seen (mutable-set))
  (define avail (cond [(not among) (list->set (graph-nodes g))]
                      [else among]))
  (define (extend members frontier)
    (cond
      [(= (length members) k)
       (define key (sort members symbol<?))
       (unless (set-member? seen key)
         (set-add! seen key)
         (set! out (cons key out)))]
      [else
       (for ([n (in-set frontier)]
             #:when (set-member? avail n))
         (define new-members (cons n members))
         (define new-frontier
           (for/set ([m (in-set
                         (set-union frontier
                                    (graph-neighbours g n)))]
                     #:when (and (set-member? avail m)
                                 (not (member m new-members))))
             m))
         (extend new-members new-frontier))]))
  (for ([start (in-set avail)])
    (extend (list start)
            (for/set ([m (in-set (graph-neighbours g start))]
                      #:when (set-member? avail m))
              m)))
  out)
