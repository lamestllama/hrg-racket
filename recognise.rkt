#lang racket/base
;; Build a cover of G by greedy matching against a structural template
;; library. A template is `(list n-interior edges)` where `edges` is a
;; list of (i . j) pairs into [0, n). Two subgraphs match iff they
;; have the same WL-canonical fingerprint.
;;
;; Greedy match order: largest templates first, ties broken by
;; descending edge-count. After all templates have claimed their
;; non-overlapping matches, leftover nodes become singletons.

(require racket/set racket/list)
(require "graph.rkt" "grammar.rkt" "canonical.rkt")

(provide recognise-cover)

(define (template-size t) (car t))
(define (template-edges t) (cadr t))

(define (template-fingerprint-cached t cache)
  (cond
    [(hash-has-key? cache t) (hash-ref cache t)]
    [else
     (define fp (template-fingerprint t))
     (hash-set! cache t fp)
     fp]))

(define (find-template-matches G template avail-nodes-set)
  ;; Enumerate connected k-subsets within avail-nodes-set, keep those
  ;; whose induced canonical fingerprint matches the template's.
  (define want-fp (template-fingerprint template))
  (define n (template-size template))
  (define cands (enumerate-connected-subsets G n #:among avail-nodes-set))
  (for/list ([nodes (in-list cands)]
             #:when (equal? (induced-canonical-fingerprint G nodes)
                            want-fp))
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
