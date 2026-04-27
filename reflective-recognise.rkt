#lang racket/base
;; Reflective recognition: load library.rkt by dynamic-require, walk
;; its namespace to discover R<n> procedures, *apply* each procedure
;; to integer ghost-arguments to recover the template's edge-list,
;; and feed the resulting data into the canonical-fingerprint matcher.
;;
;; The reflection is in three places:
;;   (1) dynamic-require / namespace-mapped-symbols  (in reflect.rkt)
;;   (2) procedure application as data extraction    (in reflect.rkt)
;;   (3) calling recognise-cover with the recovered  (here)
;;
;; The recogniser itself is unchanged from recognise.rkt — what
;; changed is *where the templates come from*. They are no longer
;; passed in as data; they are read out of the running program.

(require "graph.rkt" "grammar.rkt"
         "recognise.rkt" "reflect.rkt")

(provide reflective-recognise-cover)

(define (reflective-recognise-cover G)
  (define library (reflectively-loaded-library))
  (recognise-cover G library))
