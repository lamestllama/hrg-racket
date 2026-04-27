#lang racket/base
;; Grammar data: templates + cover.
;;
;; A `template` is a labelled subgraph signature: (interior, tentacles,
;; owned-edges). Two instances belong to the same template iff their
;; canonical forms match.
;;
;; A `cover` is a list of `instance` records partitioning the host
;; graph's nodes. Each instance points back at the template it
;; belongs to via `rule-id`.

(require racket/set racket/list racket/match)
(require "graph.rkt")

(provide
 (struct-out template)
 (struct-out instance)
 (struct-out grammar)
 trivial-cover
 leaf-absorbing-cover
 instance-canonical
 group-by-canonical
 grammar-from-cover)

(struct template (rule-id n-interior n-tentacles n-owned-edges canonical)
  #:transparent)

(struct instance (instance-id interior tentacles owned-edges rule-id)
  #:transparent)

(struct grammar (rules instances) #:transparent)

;; ---------------------------------------------------------------
;; Singleton cover (one instance per node). Useful as a baseline DL
;; reference; descent rarely improves it because every 2-node merge
;; has both endpoints as tentacles.
;; ---------------------------------------------------------------
(define (trivial-cover G)
  (for/list ([n (in-list (graph-nodes G))]
             [i (in-naturals)])
    (instance i (set n) (set) (set) -1)))

;; ---------------------------------------------------------------
;; Leaf-absorbing seed: every degree-1 node gets folded into its sole
;; neighbour's instance. The neighbour becomes a "centre" with the leaf
;; as private interior. Mirrors the spirit of Python's adaptive-WL
;; initial partition.
;; ---------------------------------------------------------------
(define (leaf-absorbing-cover G)
  (define nodes (graph-nodes G))
  (define deg-of
    (for/hash ([n (in-list nodes)])
      (values n (set-count (graph-neighbours G n)))))
  ;; Each non-leaf becomes a centre instance. Each leaf is appended to
  ;; the centre with the largest degree among its neighbours.
  (define centre-of (make-hash))     ; node → centre node (its own if non-leaf)
  (for ([n (in-list nodes)])
    (cond
      [(> (hash-ref deg-of n) 1)
       (hash-set! centre-of n n)]
      [else
       (define nbrs (sort (set->list (graph-neighbours G n))
                          (lambda (a b)
                            (> (hash-ref deg-of a 0)
                               (hash-ref deg-of b 0)))))
       (cond
         [(null? nbrs) (hash-set! centre-of n n)]   ; isolated
         [else (hash-set! centre-of n (car nbrs))])]))
  ;; Group nodes by their assigned centre.
  (define groups (make-hash))
  (for ([n (in-list nodes)])
    (define c (hash-ref centre-of n))
    (hash-update! groups c (lambda (xs) (cons n xs)) '()))
  ;; Build instances.
  (for/list ([(centre members) (in-hash groups)]
             [i (in-naturals)])
    (define member-set (list->set members))
    (define owned
      (list->set
       (for/list ([e (in-list (graph-edges G))]
                  #:when (and (set-member? member-set (car e))
                              (set-member? member-set (cdr e))))
         e)))
    (instance i member-set (set) owned -1)))

;; ---------------------------------------------------------------
;; Canonical form for an instance: (n-interior, sorted-deg-multiset,
;; n-tentacles). Coarse but enough to seed grouping; refined later by
;; WL or labelled colourings if needed.
;; ---------------------------------------------------------------
(define (instance-canonical G inst)
  (define nodes (set->list (instance-interior inst)))
  (define n (length nodes))
  (define degs
    (sort
     (for/list ([v (in-list nodes)])
       (for/sum ([u (in-set (graph-neighbours G v))]
                 #:when (set-member? (instance-interior inst) u))
         1))
     <))
  (list n degs (set-count (instance-tentacles inst))))

;; ---------------------------------------------------------------
;; Group instances by canonical key, return hash of canonical → list of
;; instances. Then assign rule-ids in canonical-order.
;; ---------------------------------------------------------------
(define (group-by-canonical G insts)
  (define groups (make-hash))
  (for ([inst (in-list insts)])
    (define key (instance-canonical G inst))
    (hash-update! groups key
                  (lambda (xs) (cons inst xs))
                  '()))
  groups)

(define (compute-tentacles G inst all-instances)
  ;; A tentacle is an interior node of `inst` that has at least one
  ;; neighbour belonging to a *different* instance.
  (define node->iidx
    (for/hash ([other (in-list all-instances)]
               [idx (in-naturals)]
               #:when #t
               [n (in-set (instance-interior other))])
      (values n idx)))
  (define this-iidx
    (for/or ([other (in-list all-instances)] [idx (in-naturals)]
             #:when (eq? other inst))
      idx))
  (for/set ([n (in-set (instance-interior inst))]
            #:when
            (for/or ([m (in-set (graph-neighbours G n))])
              (define mi (hash-ref node->iidx m #f))
              (and mi (not (= mi this-iidx)))))
    n))

(define (grammar-from-cover G cover)
  ;; First pass: stamp every instance with its tentacle set.
  (define cover-with-tents
    (for/list ([inst (in-list cover)])
      (struct-copy instance inst
                   [tentacles (compute-tentacles G inst cover)])))
  (define groups (group-by-canonical G cover-with-tents))
  (define canon-list (sort (hash-keys groups)
                           (lambda (a b) (< (car a) (car b)))))
  (define rid-of-canon
    (for/hash ([c (in-list canon-list)] [i (in-naturals)])
      (values c i)))
  (define rules
    (for/list ([c (in-list canon-list)])
      (define rep (car (hash-ref groups c)))
      (template (hash-ref rid-of-canon c)
                (set-count (instance-interior rep))
                (set-count (instance-tentacles rep))
                (set-count (instance-owned-edges rep))
                c)))
  (define labelled-insts
    (for/list ([inst (in-list cover-with-tents)])
      (define rid (hash-ref rid-of-canon (instance-canonical G inst)))
      (struct-copy instance inst [rule-id rid])))
  (grammar rules labelled-insts))
