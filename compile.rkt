#lang racket/base
;; Compile a template-data list into a list of in-memory procedures
;; via `eval`. No file I/O, no #lang module — just s-expressions
;; turned into runnable functions in a fresh namespace.
;;
;; Each template is compiled to a procedure of arity n:
;;     (lambda (n0 n1 ... n_{k-1})
;;        (list (cons n_i n_j) (cons n_p n_q) ...))
;; The procedure can then be applied to any n graph-nodes (or to
;; integer ghosts) to recover the template's edge list — same
;; reflection mechanism as library.rkt's procedures, but the source
;; never touches disk.

(require racket/list)

(provide compile-templates
         apply-template-to-ghosts
         compiled-templates-summary)

(define (template->lambda-sexp template index)
  ;; Template is now (list n tentacle-indices edges). We emit a
  ;; procedure that returns BOTH the edge list AND the tentacle nodes
  ;; (positionally selected from the args), so a caller can recover
  ;; everything by application — no need to also track tentacle
  ;; metadata externally.
  (define n (car template))
  (define tents (cadr template))
  (define edges (caddr template))
  (define args (build-list n (lambda (i) (string->symbol (format "n~a" i)))))
  (define edge-form
    (cons 'list
          (for/list ([e (in-list edges)])
            (list 'cons
                  (list-ref args (car e))
                  (list-ref args (cdr e))))))
  (define tent-form
    (cons 'list
          (for/list ([i (in-list tents)])
            (list-ref args i))))
  `(lambda ,args
     (list 'edges ,edge-form 'tentacles ,tent-form)))

(define (compile-templates library)
  ;; Returns a list of (cons name proc), one per template, eval'd in a
  ;; fresh racket/base namespace. No host-program leakage, no file.
  (define ns (make-base-namespace))
  (parameterize ([current-namespace ns])
    (for/list ([t (in-list library)] [i (in-naturals)])
      (define sym (string->symbol (format "R~a" i)))
      (define sexp (template->lambda-sexp t i))
      (define proc (eval sexp ns))
      (cons sym proc))))

(define (apply-template-to-ghosts proc)
  ;; Apply with integer ghost-args 0..n-1. The procedure returns
  ;;   (list 'edges <edge-pairs> 'tentacles <tentacle-positions>)
  ;; so we can recover the full template (n, tentacles, edges) by
  ;; running it. This is the in-memory reflection round-trip.
  (define arity (procedure-arity proc))
  (define ghosts (build-list arity values))
  (define result (apply proc ghosts))
  (define edges (cadr (member 'edges result)))
  (define tents (cadr (member 'tentacles result)))
  (define normalized-edges
    (for/list ([e (in-list edges)])
      (define a (car e)) (define b (cdr e))
      (cond [(<= a b) (cons a b)] [else (cons b a)])))
  (list arity tents normalized-edges))

(define (compiled-templates-summary entries)
  (for/list ([entry (in-list entries)])
    (cons (car entry) (apply-template-to-ghosts (cdr entry)))))
