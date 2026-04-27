#lang racket/base
;; Bootstrap loop, fully in-memory by default. The "library" is a
;; Racket value — a list of template s-expressions of shape
;;     (list n tentacle-indices edges)
;; that grows by one entry per accepted proposal. At any point the
;; library can be COMPILED to runnable procedures via `compile.rkt`,
;; which evals each spec in a fresh make-base-namespace — no file
;; involved. Persistence (--load / --save) is opt-in.
;;
;; Usage:
;;   racket bootstrap.rkt examples/elecnet.dot
;;   racket bootstrap.rkt --load templates.dat --save templates.dat <dot>
;;   racket bootstrap.rkt --max-size 6 --verbose <dot>

(require racket/cmdline racket/file racket/list racket/format)
(require "dot.rkt" "graph.rkt" "grammar.rkt" "score.rkt"
         "recognise.rkt" "propose.rkt" "canonical.rkt"
         "compile.rkt")

(define max-size (make-parameter 6))
(define load-path (make-parameter #f))
(define save-path (make-parameter #f))

(define dot-path
  (command-line #:program "bootstrap"
                #:once-each
                [("--max-size") n "Largest template size to consider"
                                (max-size (string->number n))]
                [("--load") p "Load library from path before running"
                            (load-path p)]
                [("--save") p "Save library to path after running"
                            (save-path p)]
                #:args (path) path))

(define (cover-dl G cover)
  (hash-ref (principled-dl G (grammar-from-cover G cover)) 'total-dl))

(define G (load-dot dot-path))
(define library
  (cond [(and (load-path) (file-exists? (load-path)))
         (with-input-from-file (load-path) read)]
        [else '()]))
(printf "loaded ~a: ~a nodes, ~a edges; library starts at ~a templates~n"
        dot-path (graph-n-nodes G) (graph-n-edges G) (length library))

(define-values (final-library final-dl)
  (let outer ([library library] [round 0] [accepted 0])
    (define cover (recognise-cover G library))
    (define dl (cover-dl G cover))
    (printf "~nround ~a: ~a templates, ~a instances, DL=~a~n"
            round (length library) (length cover)
            (real->decimal-string dl 1))
    (define cands (propose-templates G cover library #:max-size (max-size)))
    (printf "  ~a novel candidates~n" (length cands))
    (let try-each ([cs cands])
      (cond
        [(null? cs)
         (printf "  no candidate improved DL; stopping~n")
         (values library dl)]
        [else
         (define t (car cs))
         (define trial-lib (append library (list t)))
         (define trial-cover (recognise-cover G trial-lib))
         (define trial-dl (cover-dl G trial-cover))
         (cond
           [(< trial-dl dl)
            (printf "  ACCEPT ~v: ~a → ~a (Δ~a)~n"
                    t
                    (real->decimal-string dl 1)
                    (real->decimal-string trial-dl 1)
                    (real->decimal-string (- trial-dl dl) 1))
            (outer trial-lib (+ round 1) (+ accepted 1))]
           [else
            (try-each (cdr cs))])]))))

(printf "~nfinal: ~a templates, DL=~a~n"
        (length final-library)
        (real->decimal-string final-dl 1))

;; --- in-memory eval round-trip ----------------------------------
;; Every accepted template is now an s-expression in `final-library`.
;; Compile them to procedures by `eval` in a fresh namespace and apply
;; each with integer ghost-args; the returned (edges, tentacles) pair
;; round-trips the template. No file involvement.
(printf "~n--- in-memory eval round-trip ---~n")
(define compiled (compile-templates final-library))
(printf "compiled ~a templates in a fresh namespace:~n" (length compiled))
(for ([entry (in-list compiled)] [t (in-list final-library)])
  (define name (car entry))
  (define proc (cdr entry))
  (define recovered (apply-template-to-ghosts proc))
  (define same? (equal? recovered t))
  (printf "  ~a (~an, tentacles=~v, edges=~v) round-trip ~a~n"
          name (car recovered) (cadr recovered) (caddr recovered)
          (if same? "OK" "MISMATCH")))

(when (save-path)
  (call-with-output-file (save-path) #:exists 'replace
    (lambda (out) (write final-library out) (newline out)))
  (printf "~nsaved final library to ~a~n" (save-path)))
