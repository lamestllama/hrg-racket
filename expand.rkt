#lang racket/base
;; Reflective expansion CLI. Given a DOT file and a current
;; library.rkt, load the library's templates by reflection
;; (`dynamic-require` + namespace inspection), recognise instances of
;; each template in the graph by literal procedure application, and
;; print the resulting cover and DL.
;;
;; This is the recognition path that mirrors the user's vision: the
;; templates ARE the Racket source, and matching is just calling the
;; functions the source defines.
;;
;; Usage:  racket expand.rkt examples/templated.dot

(require racket/cmdline)
(require "dot.rkt" "graph.rkt" "grammar.rkt" "score.rkt"
         "reflective-recognise.rkt" "reflect.rkt")

(define dot-path
  (command-line #:program "expand"
                #:args (path) path))

(define G (load-dot dot-path))
(printf "loaded ~a: ~a nodes, ~a edges~n"
        dot-path (graph-n-nodes G) (graph-n-edges G))

(define templates (discovered-templates))
(printf "discovered ~a templates from library.rkt:~n" (length templates))
(for ([entry (in-list templates)])
  (printf "  ~a (arity ~a)~n"
          (car entry) (procedure-arity (cdr entry))))

(define cover (reflective-recognise-cover G))
(define gram (grammar-from-cover G cover))
(define dl (principled-dl G gram))
(printf "~nreflective cover: ~a rules, ~a instances, DL=~a~n"
        (length (grammar-rules gram))
        (length (grammar-instances gram))
        (real->decimal-string (hash-ref dl 'total-dl) 1))
(for ([r (in-list (sort (grammar-rules gram) >
                         #:key (lambda (r)
                                 (template-n-interior r))))])
  (printf "  R~a: interior=~an, tentacles=~an, owned=~ae~n"
          (template-rule-id r)
          (template-n-interior r)
          (template-n-tentacles r)
          (template-n-owned-edges r)))
