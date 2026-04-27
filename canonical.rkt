#lang racket/base
;; WL-canonical fingerprints for small induced subgraphs. Two
;; subgraphs with the same fingerprint are WL-indistinguishable and
;; (for graphs up to ~10 vertices) almost always isomorphic.
;;
;; A fingerprint is a 3-tuple:
;;   (n  sorted-colour-multiset  sorted-edge-multiset)
;; where colours come from running WL refinement on the induced
;; subgraph until stable.

(require racket/set racket/list)
(require "graph.rkt")

(provide induced-canonical-fingerprint
         template-fingerprint
         template-fingerprint-equal?)

(define (sorted-list-of-ints xs)
  (sort xs <))

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
       ;; Sort unique values by equal-hash-code so id assignment is
       ;; deterministic across runs and across structurally-identical
       ;; subgraphs (otherwise the same WL-equivalent motif can get
       ;; different colour IDs and thus different fingerprints).
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

(define (induced-canonical-fingerprint G nodes)
  (define n (length nodes))
  (define node-set (list->set nodes))
  (define adj-of
    (lambda (v)
      (for/set ([m (in-set (graph-neighbours G v))]
                #:when (set-member? node-set m))
        m)))
  (define initial (for/hash ([v (in-list nodes)]) (values v 0)))
  (define final (wl-refine adj-of nodes initial 6))
  (define cs (sorted-list-of-ints (hash-values final)))
  (define es
    (sort
     (for/list ([e (in-list (induced-edges G nodes))])
       (sorted-pair (list (hash-ref final (car e))
                          (hash-ref final (cdr e)))))
     compare-pair))
  (list n cs es))

;; The fingerprint of a *template* (a stored, abstract subgraph) is
;; computed from its own node/edge spec. A template is represented as
;;     (list n-interior edges)
;; where edges is a list of (i . j) pairs of indices into [0, n).
(define (template-fingerprint template)
  (define n (car template))
  (define edges (cadr template))
  (define node-list (build-list n (lambda (i) i)))
  (define adj
    (let ([h (make-hash)])
      (for ([i (in-list node-list)]) (hash-set! h i (mutable-set)))
      (for ([e (in-list edges)])
        (set-add! (hash-ref h (car e)) (cdr e))
        (set-add! (hash-ref h (cdr e)) (car e)))
      h))
  (define adj-of (lambda (v) (hash-ref adj v (mutable-set))))
  (define initial (for/hash ([v (in-list node-list)]) (values v 0)))
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
