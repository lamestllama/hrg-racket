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
 instance-canonical
 group-by-canonical
 grammar-from-cover)

(struct template (rule-id n-interior n-tentacles n-owned-edges canonical)
  #:transparent)

(struct instance (instance-id interior tentacles owned-edges rule-id)
  #:transparent)

(struct grammar (rules instances) #:transparent)

;; ---------------------------------------------------------------
;; Trivial cover: every node is a singleton instance owning all its
;; incident edges as tentacles. Used as the seed for self-modification.
;; ---------------------------------------------------------------
(define (trivial-cover G)
  (for/list ([n (in-list (graph-nodes G))]
             [i (in-naturals)])
    (instance i (set n) (set) (set) -1)))

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

(define (grammar-from-cover G cover)
  (define groups (group-by-canonical G cover))
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
    (for/list ([inst (in-list cover)])
      (define rid (hash-ref rid-of-canon (instance-canonical G inst)))
      (struct-copy instance inst [rule-id rid])))
  (grammar rules labelled-insts))
