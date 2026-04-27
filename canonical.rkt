#lang racket/base
;; WL-canonical fingerprints for small induced subgraphs, with
;; tentacle-aware initial colouring.
;;
;; A template now carries a tentacle-position set: the subset of its
;; n positions whose corresponding host-graph nodes are tentacles
;; (have edges crossing the instance boundary). WL refinement starts
;; with initial colour 1 for tentacle positions, 0 for private-
;; interior positions, so two subgraphs with the same structure but
;; different tentacle patterns produce different fingerprints —
;; matching Python's `make_boundary_fn` semantics.
;;
;; Template format: (list n tentacle-indices edges)
;; Fingerprint:    (list n sorted-colour-multiset sorted-edge-multiset)

(require racket/set racket/list)
(require "graph.rkt")

(provide induced-canonical-fingerprint
         template-fingerprint
         template-fingerprint-equal?
         compute-tentacle-set)

(define (compute-tentacle-set G nodes)
  ;; A tentacle is an interior node with at least one host-graph
  ;; neighbour outside this nodeset.
  (define ns (list->set nodes))
  (for/set ([v (in-list nodes)]
            #:when
            (for/or ([m (in-set (graph-neighbours G v))])
              (not (set-member? ns m))))
    v))

(define (sorted-list-of-ints xs) (sort xs <))

(define (sorted-pair p)
  (cond [(<= (car p) (cadr p)) p]
        [else (list (cadr p) (car p))]))

(define (compare-pair a b)
  (cond [(< (car a) (car b)) #t]
        [(> (car a) (car b)) #f]
        [else (< (cadr a) (cadr b))]))

(define (wl-refine adj-of nodes initial-colour rounds)
  (let loop ([colour initial-colour] [r 0])
    (cond
      [(>= r rounds) colour]
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
         [else (loop next (+ r 1))])])))

(define (induced-canonical-fingerprint G nodes tentacle-set)
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
  (define final (wl-refine adj-of nodes initial 6))
  (define cs (sorted-list-of-ints (hash-values final)))
  (define es
    (sort
     (for/list ([e (in-list (induced-edges G nodes))])
       (sorted-pair (list (hash-ref final (car e))
                          (hash-ref final (cdr e)))))
     compare-pair))
  (list n cs es))

(define (template-fingerprint template)
  ;; template = (list n tentacle-indices edges)
  (define n (car template))
  (define tents (cadr template))
  (define edges (caddr template))
  (define node-list (build-list n (lambda (i) i)))
  (define tents-set (list->set tents))
  (define adj
    (let ([h (make-hash)])
      (for ([i (in-list node-list)]) (hash-set! h i (mutable-set)))
      (for ([e (in-list edges)])
        (set-add! (hash-ref h (car e)) (cdr e))
        (set-add! (hash-ref h (cdr e)) (car e)))
      h))
  (define adj-of (lambda (v) (hash-ref adj v (mutable-set))))
  (define initial
    (for/hash ([v (in-list node-list)])
      (values v (if (set-member? tents-set v) 1 0))))
  (define final (wl-refine adj-of node-list initial 6))
  (define cs (sorted-list-of-ints (hash-values final)))
  (define es
    (sort
     (for/list ([e (in-list edges)])
       (sorted-pair (list (hash-ref final (car e))
                          (hash-ref final (cdr e)))))
     compare-pair))
  (list n cs es))

(define (template-fingerprint-equal? a b)
  (equal? a b))
