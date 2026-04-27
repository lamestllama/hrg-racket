#lang racket/base
;; Local moves on a cover. For the bootstrap loop we only need merge:
;; pick two instances whose interiors share at least one host edge, and
;; fuse them into a single instance. Owned-edges of the merged instance
;; = induced edges over the union. Tentacles are recomputed at scoring
;; time (see grammar-from-cover); we don't pre-store them on instances.

(require racket/set racket/list)
(require "graph.rkt" "grammar.rkt")

(provide adjacent-instances try-merge)

(define (adjacent-instances G insts)
  ;; Pairs (i, j) where instance i and instance j share at least one
  ;; cross-instance edge in G. Returns indices into insts, i<j.
  (define node->iidx
    (for/hash ([inst (in-list insts)] [idx (in-naturals)]
               #:when #t
               [n (in-set (instance-interior inst))])
      (values n idx)))
  (define seen (mutable-set))
  (define out '())
  (for ([e (in-list (graph-edges G))])
    (define ia (hash-ref node->iidx (car e) #f))
    (define ib (hash-ref node->iidx (cdr e) #f))
    (when (and ia ib (not (= ia ib)))
      (define key (if (< ia ib) (cons ia ib) (cons ib ia)))
      (unless (set-member? seen key)
        (set-add! seen key)
        (set! out (cons key out)))))
  out)

(define (try-merge G insts ia ib)
  ;; Replace insts[ia], insts[ib] with their merge; return a new list.
  (define a (list-ref insts ia))
  (define b (list-ref insts ib))
  (define merged-interior (set-union (instance-interior a)
                                     (instance-interior b)))
  (define merged-owned
    (list->set
     (for/list ([e (in-list (graph-edges G))]
                #:when (and (set-member? merged-interior (car e))
                            (set-member? merged-interior (cdr e))))
       e)))
  (define new-inst
    (instance (instance-instance-id a)
              merged-interior
              (set)        ; tentacles recomputed at scoring time
              merged-owned
              -1))
  (append (for/list ([inst (in-list insts)] [idx (in-naturals)]
                     #:unless (or (= idx ia) (= idx ib)))
            inst)
          (list new-inst)))
