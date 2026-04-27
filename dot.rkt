#lang racket/base
;; Minimal DOT loader. Reads node declarations and undirected
;; edge lines (`a -- b;` or with attribute brackets), ignoring
;; subgraphs/clusters. Quoted and bare identifiers both supported.

(require racket/port racket/string racket/match)
(require "graph.rkt")

(provide load-dot)

(define ID-RX
  ;; Either a quoted string (no escaped quotes inside, simple) or a bare
  ;; identifier (alphanumeric + . _ -)
  #px"\"([^\"]+)\"|([A-Za-z0-9_.\\-]+)")

(define EDGE-RX
  ;; Captures both endpoints, undirected (-- or ->), trailing optional [..];
  #px"(?:\"([^\"]+)\"|([A-Za-z0-9_.\\-]+))\\s*(?:--|->)\\s*(?:\"([^\"]+)\"|([A-Za-z0-9_.\\-]+))")

(define (load-dot path)
  (define G (make-graph))
  (with-input-from-file path
    (lambda ()
      (for ([line (in-lines)])
        (define stripped (string-trim line))
        (cond
          [(or (string=? stripped "")
               (regexp-match? #px"^//" stripped)
               (regexp-match? #px"^#" stripped)
               (regexp-match? #px"^(graph|digraph|subgraph)\\b" stripped)
               (regexp-match? #px"^\\}" stripped)
               (regexp-match? #px"^\\{" stripped)
               (regexp-match? #px"^[A-Za-z]+\\s*=" stripped))
           (void)]
          [(regexp-match EDGE-RX stripped)
           =>
           (lambda (m)
             (define u (or (list-ref m 1) (list-ref m 2)))
             (define v (or (list-ref m 3) (list-ref m 4)))
             (graph-add-edge! G (string->symbol u) (string->symbol v)))]
          [else (void)]))))
  G)
