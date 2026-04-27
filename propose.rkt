#lang racket/base
;; Merge-based template proposals with tentacle-aware canonical
;; equivalence. For each pair of adjacent instances (A, B), form the
;; merged candidate (A ∪ B), compute its tentacle-set in G, then
;; produce the canonical template (n, tentacle-indices, edges) under
;; tentacle-aware WL. Group candidates by canonical fingerprint, rank
;; by frequency.

(require racket/set racket/list)
(require "graph.rkt" "grammar.rkt" "canonical.rkt")

(provide propose-templates subgraph-as-template)

(define (subgraph-as-template G nodes tentacle-set)
  ;; Build (n tentacle-indices edges). Nodes are sorted into canonical
  ;; (WL) order with tentacle vs private-interior distinguished by
  ;; initial colour. Edges are remapped into positional indices.
  (define n (length nodes))
  (define node-set (list->set nodes))
  (define adj-of
    (lambda (v)
      (for/set ([m (in-set (graph-neighbours G v))]
                #:when (set-member? node-set m))
        m)))
  (define initial
    (for/hash ([v (in-list nodes)])
      (values v (if (set-member? tentacle-set v) 1 0))))
  (define final-colour
    (let loop ([colour initial] [r 0])
      (cond
        [(>= r 6) colour]
        [else
         (define raw
           (for/hash ([v (in-list nodes)])
             (define mine (hash-ref colour v))
             (define nbrs
               (sort
                (for/list ([m (in-set (adj-of v))]
                           #:when (hash-has-key? colour m))
                  (hash-ref colour m))
                <))
             (values v (cons mine nbrs))))
         (define unique
           (sort (remove-duplicates (hash-values raw))
                 (lambda (a b)
                   (< (equal-hash-code a) (equal-hash-code b)))))
         (define id-of
           (for/hash ([c (in-list unique)] [i (in-naturals)])
             (values c i)))
         (define next
           (for/hash ([(v c) (in-hash raw)])
             (values v (hash-ref id-of c))))
         (cond
           [(equal? colour next) next]
           [else (loop next (+ r 1))])])))
  (define sorted-nodes
    (sort nodes
          (lambda (a b)
            (cond
              [(< (hash-ref final-colour a) (hash-ref final-colour b)) #t]
              [(> (hash-ref final-colour a) (hash-ref final-colour b)) #f]
              [else (symbol<? a b)]))))
  (define index-of
    (for/hash ([v (in-list sorted-nodes)] [i (in-naturals)])
      (values v i)))
  (define tentacle-indices
    (sort
     (for/list ([v (in-list sorted-nodes)] [i (in-naturals)]
                #:when (set-member? tentacle-set v))
       i)
     <))
  (define edges
    (sort
     (for/list ([e (in-list (induced-edges G nodes))])
       (define i (hash-ref index-of (car e)))
       (define j (hash-ref index-of (cdr e)))
       (cond [(<= i j) (cons i j)] [else (cons j i)]))
     (lambda (a b)
       (cond [(< (car a) (car b)) #t]
             [(> (car a) (car b)) #f]
             [else (< (cdr a) (cdr b))]))))
  (list n tentacle-indices edges))

(define (already-known? fp known)
  (set-member? known fp))

(define (propose-templates G cover library
                            #:max-size [max-size 6]
                            #:min-edges [min-edges 1])
  ;; Merge-based: for each pair of cover-instances that share a
  ;; cross-instance edge in G, form the union as a candidate template.
  ;; Group by tentacle-aware canonical fingerprint, exclude library
  ;; fingerprints, rank by frequency.
  (define inst-of-node
    (for/hash ([inst (in-list cover)] [idx (in-naturals)]
               #:when #t
               [n (in-set (instance-interior inst))])
      (values n idx)))
  (define adj-pairs (mutable-set))
  (for ([e (in-list (graph-edges G))])
    (define ia (hash-ref inst-of-node (car e) #f))
    (define ib (hash-ref inst-of-node (cdr e) #f))
    (when (and ia ib (not (= ia ib)))
      (define key (if (< ia ib) (cons ia ib) (cons ib ia)))
      (set-add! adj-pairs key)))
  (define known-fps
    (for/set ([t (in-list library)])
      (template-fingerprint t)))
  (define histogram (make-hash))
  (for ([pair (in-set adj-pairs)])
    (define A (list-ref cover (car pair)))
    (define B (list-ref cover (cdr pair)))
    (define merged-nodes
      (set->list (set-union (instance-interior A)
                            (instance-interior B))))
    (define n (length merged-nodes))
    (when (and (<= n max-size)
               (>= (length (induced-edges G merged-nodes))
                   min-edges))
      (define tents (compute-tentacle-set G merged-nodes))
      (define t (subgraph-as-template G merged-nodes tents))
      (define fp (template-fingerprint t))
      (cond
        [(already-known? fp known-fps) (void)]
        [(hash-has-key? histogram fp)
         (define entry (hash-ref histogram fp))
         (hash-set! histogram fp
                    (cons (+ 1 (car entry)) (cdr entry)))]
        [else
         (hash-set! histogram fp (cons 1 t))])))
  (define ranked
    (sort (hash-values histogram)
          (lambda (a b)
            (cond
              [(> (car a) (car b)) #t]
              [(< (car a) (car b)) #f]
              [else (> (car (cdr a)) (car (cdr b)))]))))
  (for/list ([entry (in-list ranked)])
    (cdr entry)))
