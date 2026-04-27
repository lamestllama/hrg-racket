#lang racket/base
;; Greedy template-matching with tentacle-aware canonical fingerprints.
;; A candidate node-subset matches a template iff their tentacle-aware
;; canonical fingerprints are equal — i.e. the candidate has the same
;; structure AND the same cross-boundary tentacle pattern that the
;; template prescribes.

(require racket/set racket/list)
(require "graph.rkt" "grammar.rkt" "canonical.rkt")

(provide recognise-cover)

(define (template-size t) (car t))
(define (template-tentacle-indices t) (cadr t))
(define (template-edges t) (caddr t))

(define (find-template-matches G template avail-nodes-set)
  (define want-fp (template-fingerprint template))
  (define n (template-size template))
  (define cands (enumerate-connected-subsets G n #:among avail-nodes-set))
  (for/list ([nodes (in-list cands)]
             #:when
             (let ([tents (compute-tentacle-set G nodes)])
               (equal? (induced-canonical-fingerprint G nodes tents)
                       want-fp)))
    nodes))

(define (template-cmp a b)
  (cond
    [(> (template-size a) (template-size b)) #t]
    [(< (template-size a) (template-size b)) #f]
    [else (> (length (template-edges a)) (length (template-edges b)))]))

(define (recognise-cover G library)
  (define sorted-lib (sort library template-cmp))
  (define covered (mutable-set))
  (define instances '())
  (define iid 0)

  (for ([template (in-list sorted-lib)])
    (define avail
      (for/set ([v (in-list (graph-nodes G))]
                #:when (not (set-member? covered v)))
        v))
    (when (>= (set-count avail) (template-size template))
      (define matches (find-template-matches G template avail))
      (for ([m (in-list matches)]
            #:when (andmap (lambda (v) (not (set-member? covered v))) m))
        (define interior (list->set m))
        (define owned (list->set (induced-edges G m)))
        (set! instances
              (cons (instance iid interior (set) owned -1) instances))
        (set! iid (+ iid 1))
        (for ([v (in-list m)])
          (set-add! covered v)))))

  (for ([v (in-list (graph-nodes G))]
        #:unless (set-member? covered v))
    (set! instances
          (cons (instance iid (set v) (set) (set) -1) instances))
    (set! iid (+ iid 1)))

  (reverse instances))
