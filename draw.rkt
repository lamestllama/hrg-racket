#lang racket/base
;; Draw rules and compositions to PNG via Graphviz.
;;
;; Each rule is rendered from its abstract spec (n, tentacles, edges) —
;; positional node ids, tentacle nodes drawn in red double-circles,
;; private-interior nodes in light blue.
;;
;; Each composition is rendered from a concrete pair of adjacent
;; instances in the final cover: src instance on the left, tgt
;; instance on the right (each as a clustered subgraph), with bridge
;; edges between them.
;;
;; If `dot` isn't on PATH, we still emit .dot files; PNG just won't be
;; produced and we report that.

(require racket/set racket/list racket/format racket/system racket/file)
(require "graph.rkt" "grammar.rkt")

(provide draw-rule
         draw-composition
         find-compositions)

(define DOT-EXE (find-executable-path "dot"))

(define (string-join strs sep)
  (cond [(null? strs) ""]
        [(null? (cdr strs)) (car strs)]
        [else (apply string-append
                     (car strs)
                     (for/list ([s (in-list (cdr strs))])
                       (string-append sep s)))]))

(define (write-dot+png dot-text out-path)
  (define dot-path (string-append out-path ".dot"))
  (define png-path (string-append out-path ".png"))
  (call-with-output-file dot-path #:exists 'replace
    (lambda (out) (display dot-text out)))
  (cond
    [DOT-EXE
     (system* DOT-EXE "-Tpng" "-o" png-path dot-path)
     png-path]
    [else dot-path]))

;; ---------- rule drawing ----------------------------------------
(define (draw-rule template title out-path)
  (define n (car template))
  (define tents-set (list->set (cadr template)))
  (define edges (caddr template))
  (define node-decls
    (for/list ([i (in-range n)])
      (cond
        [(set-member? tents-set i)
         (format "  n~a [label=\"~a\", style=filled, fillcolor=\"#f8b4b4\", shape=doublecircle];"
                 i i)]
        [else
         (format "  n~a [label=\"~a\", style=filled, fillcolor=\"#cce5ff\", shape=circle];"
                 i i)])))
  (define edge-decls
    (for/list ([e (in-list edges)])
      (format "  n~a -- n~a;" (car e) (cdr e))))
  (define dot-text
    (string-append
     "graph rule {\n"
     (format "  label=~v;\n  labelloc=t;\n  fontname=Helvetica;\n  node [fontname=Helvetica];\n" title)
     (string-join node-decls "\n") "\n"
     (string-join edge-decls "\n") "\n"
     "}\n"))
  (write-dot+png dot-text out-path))

;; ---------- composition drawing ---------------------------------
;; Given the final cover, find which (rule_a, rule_b) pairs co-occur
;; as adjacent instances in G. For each, return a sample (src, tgt,
;; bridge-edges) triple suitable for drawing.

(define (find-compositions G cover)
  ;; Return a list of comp-records, one per (rule_a, rule_b) pair
  ;; observed in the cover. Each record is:
  ;;   (list (rule_a . rule_b)
  ;;         (list inst-a inst-b bridge-edges) ;; sample
  ;;         total-records-count)
  (define inst-of-node
    (for/hash ([inst (in-list cover)] [idx (in-naturals)]
               #:when #t
               [v (in-set (instance-interior inst))])
      (values v idx)))
  (define pairs (make-hash))   ; (a . b) → list of (ia ib bridge-edge)
  (for ([e (in-list (graph-edges G))])
    (define ia (hash-ref inst-of-node (car e) #f))
    (define ib (hash-ref inst-of-node (cdr e) #f))
    (when (and ia ib (not (= ia ib)))
      (define A (list-ref cover ia))
      (define B (list-ref cover ib))
      (define ra (instance-rule-id A))
      (define rb (instance-rule-id B))
      (when (and (>= ra 0) (>= rb 0))
        (define key (cond [(<= ra rb) (cons ra rb)] [else (cons rb ra)]))
        (define src-id (cond [(<= ra rb) ia] [else ib]))
        (define tgt-id (cond [(<= ra rb) ib] [else ia]))
        (hash-update! pairs key
                      (lambda (xs) (cons (list src-id tgt-id e) xs))
                      '()))))
  (for/list ([(key entries) (in-hash pairs)])
    (define count (length entries))
    ;; Pick the first as exemplar; gather all bridge edges between
    ;; that exemplar's two instances.
    (define sample (car entries))
    (define src-id (car sample))
    (define tgt-id (cadr sample))
    (define bridges
      (for/list ([entry (in-list entries)]
                 #:when (and (= (car entry) src-id)
                             (= (cadr entry) tgt-id)))
        (caddr entry)))
    (list key (list src-id tgt-id bridges) count)))

(define (draw-composition G cover comp-record out-path)
  (define key (car comp-record))
  (define sample (cadr comp-record))
  (define count (caddr comp-record))
  (define src-id (car sample))
  (define tgt-id (cadr sample))
  (define bridges (caddr sample))
  (define src (list-ref cover src-id))
  (define tgt (list-ref cover tgt-id))
  (define src-nodes (sort (set->list (instance-interior src)) symbol<?))
  (define tgt-nodes (sort (set->list (instance-interior tgt)) symbol<?))
  (define src-edges (set->list (instance-owned-edges src)))
  (define tgt-edges (set->list (instance-owned-edges tgt)))
  (define (escape-id s) (format "\"~a\"" s))
  (define src-cluster
    (string-append
     "  subgraph cluster_src {\n"
     (format "    label=\"R~a (src)\"; style=filled; color=\"#cce5ff\";\n" (car key))
     (string-join
      (for/list ([n (in-list src-nodes)])
        (format "    ~a [label=\"\", shape=circle, style=filled, fillcolor=white];"
                (escape-id n)))
      "\n") "\n"
     (string-join
      (for/list ([e (in-list src-edges)])
        (format "    ~a -- ~a;" (escape-id (car e)) (escape-id (cdr e))))
      "\n") "\n"
     "  }\n"))
  (define tgt-cluster
    (string-append
     "  subgraph cluster_tgt {\n"
     (format "    label=\"R~a (tgt)\"; style=filled; color=\"#fde2b8\";\n" (cdr key))
     (string-join
      (for/list ([n (in-list tgt-nodes)])
        (format "    ~a [label=\"\", shape=circle, style=filled, fillcolor=white];"
                (escape-id n)))
      "\n") "\n"
     (string-join
      (for/list ([e (in-list tgt-edges)])
        (format "    ~a -- ~a;" (escape-id (car e)) (escape-id (cdr e))))
      "\n") "\n"
     "  }\n"))
  (define bridge-decls
    (for/list ([e (in-list bridges)])
      (format "  ~a -- ~a [color=red, penwidth=2.0];"
              (escape-id (car e)) (escape-id (cdr e)))))
  (define dot-text
    (string-append
     "graph composition {\n"
     "  rankdir=LR;\n"
     (format "  label=\"R~a ↔ R~a (~a uses)\";\n" (car key) (cdr key) count)
     "  labelloc=t;\n  fontname=Helvetica;\n"
     src-cluster
     tgt-cluster
     (string-join bridge-decls "\n") "\n"
     "}\n"))
  (write-dot+png dot-text out-path))
