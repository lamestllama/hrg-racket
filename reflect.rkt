#lang racket/base
;; Reflective discovery of the template library. Loads library.rkt
;; dynamically (so we don't need to import it at compile time of this
;; module), then walks its namespace looking for `R<n>` bindings —
;; each one is a procedure that, given n graph-nodes, returns the
;; edge list its template would impose on them.
;;
;; This is the "the program reads itself" piece. The grammar IS the
;; source file, and recognition is just calling those procedures.

(require racket/runtime-path racket/contract)

(define-runtime-path library-path "library.rkt")

(provide discovered-templates rerequire-library
         reflectively-loaded-library)

(define (rerequire-library)
  ;; Force a fresh re-load after bootstrap.rkt rewrites library.rkt.
  ;; Plain `dynamic-require` caches; we need rerequire to pick up the
  ;; updated source.
  (dynamic-require 'racket/rerequire 'dynamic-rerequire)
  ((dynamic-require 'racket/rerequire 'dynamic-rerequire) library-path))

(define (discovered-templates)
  ;; Returns a list of (cons symbol procedure) pairs, one per R<n>
  ;; binding in library.rkt, sorted by arity descending.
  (dynamic-require library-path 0)
  (define lib-ns (module->namespace library-path))
  (define names
    (for/list ([sym (in-list (namespace-mapped-symbols lib-ns))]
               #:when (regexp-match? #rx"^R[0-9]+$"
                                     (symbol->string sym)))
      sym))
  (define entries
    (for/list ([sym (in-list names)])
      (define proc (namespace-variable-value sym #t #f lib-ns))
      (cons sym proc)))
  (sort entries
        (lambda (a b)
          (define aa (procedure-arity (cdr a)))
          (define ba (procedure-arity (cdr b)))
          (cond
            [(> aa ba) #t]
            [(< aa ba) #f]
            [else (symbol<?
                   (string->symbol
                    (substring (symbol->string (car a)) 1))
                   (string->symbol
                    (substring (symbol->string (car b)) 1)))]))))

;; ---------------------------------------------------------------
;; Recover template *data* — (list n-interior edge-pairs) — by
;; APPLYING each discovered procedure to integer indices 0..n-1.
;; The procedure body is, by construction, a list of (cons n_i n_j)
;; forms; applying it with integer ghost-nodes yields exactly the
;; template's positional edge-list. This is reflection actually
;; doing work: we read the grammar by *running* the source.
;; ---------------------------------------------------------------
(define (recover-template-data proc)
  (define arity (procedure-arity proc))
  (define ghost-args (for/list ([i (in-range arity)]) i))
  (define ghost-edges (apply proc ghost-args))
  (define normalized
    (for/list ([e (in-list ghost-edges)])
      (define a (car e)) (define b (cdr e))
      (cond [(<= a b) (cons a b)] [else (cons b a)])))
  (list arity normalized))

(define (reflectively-loaded-library)
  (for/list ([entry (in-list (discovered-templates))])
    (recover-template-data (cdr entry))))
