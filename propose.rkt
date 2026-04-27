#lang racket/base
;; Propose new templates by enumerating connected k-subsets of the
;; current singletons, grouping them by WL-canonical fingerprint,
;; ranking by frequency. Returns a list of candidate templates in
;; descending frequency order; the bootstrap loop tries each in turn.
;;
;; A template here is `(list n-interior edges)`. To turn an enumerated
;; subgraph into a template, we relabel its nodes 0..n-1 in the order
;; given by WL refinement (canonical position); the resulting edge
;; list is then position-indexed.

(require racket/set racket/list)
(require "graph.rkt" "grammar.rkt" "canonical.rkt")

(provide propose-templates)

(define (subgraph-as-template G nodes)
  ;; Build a template from a concrete induced subgraph. Use WL
  ;; refinement to get a canonical ordering, then number nodes
  ;; 0..n-1 in that order. Edges are remapped accordingly.
  (define n (length nodes))
  (define node-set (list->set nodes))
  (define adj-of
    (lambda (v)
      (for/set ([m (in-set (graph-neighbours G v))]
                #:when (set-member? node-set m))
        m)))
  (define-values (final-colour _)
    (let-values
        ([(c)
          (let loop ([colour (for/hash ([v (in-list nodes)]) (values v 0))]
                     [r 0])
            (cond
              [(>= r 6) colour]
              [else
               (define raw
                 (for/hash ([v (in-list nodes)])
                   (define mine (hash-ref colour v))
                   (define nbr-cols
                     (sort
                      (for/list ([m (in-set (adj-of v))]
                                 #:when (hash-has-key? colour m))
                        (hash-ref colour m))
                      <))
                   (values v (cons mine nbr-cols))))
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
                 [else (loop next (+ r 1))])]))])
      (values c #f)))
  ;; Sort nodes by (colour, then symbol) for canonical order.
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
  (list n edges))

(define (already-known? fp known)
  (set-member? known fp))

(define (propose-templates G cover library
                            #:max-size [max-size 6]
                            #:min-edges [min-edges 1])
  ;; Merge-based proposal. For each pair of instances in the current
  ;; cover that share at least one cross-instance edge in G, form the
  ;; merged candidate (A ∪ B), compute its WL-canonical fingerprint,
  ;; and tally. Group candidates by fingerprint; rank by frequency.
  ;; This is O(|graph edges|) per round — vastly faster than the
  ;; previous O(C(N,k)) enumeration, and well-directed because every
  ;; candidate is anchored on existing structure.
  (define n-insts (length cover))
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
      (define t (subgraph-as-template G merged-nodes))
      (define fp (template-fingerprint t))
      (cond
        [(already-known? fp known-fps) (void)]
        [(hash-has-key? histogram fp)
         (define entry (hash-ref histogram fp))
         (hash-set! histogram fp
                    (cons (+ 1 (car entry)) (cdr entry)))]
        [else
         (hash-set! histogram fp (cons 1 t))])))
  ;; Rank by frequency descending; break ties by larger size.
  (define ranked
    (sort (hash-values histogram)
          (lambda (a b)
            (cond
              [(> (car a) (car b)) #t]
              [(< (car a) (car b)) #f]
              [else
               (> (car (cdr a)) (car (cdr b)))]))))
  (for/list ([entry (in-list ranked)])
    (cdr entry)))
