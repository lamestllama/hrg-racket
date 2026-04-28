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

(require racket/cmdline racket/file racket/list racket/format racket/set)
(require "dot.rkt" "graph.rkt" "grammar.rkt" "score.rkt"
         "recognise.rkt" "propose.rkt" "canonical.rkt"
         "compile.rkt" "draw.rkt")

(define max-size (make-parameter 6))
(define load-path (make-parameter #f))
(define save-path (make-parameter #f))
(define draw-dir (make-parameter #f))
(define min-private (make-parameter 1))
(define min-private-frac (make-parameter 0.5))
(define use-sexpr-dl (make-parameter #f))

(define dot-path
  (command-line #:program "bootstrap"
                #:once-each
                [("--max-size") n "Largest template size to consider"
                                (max-size (string->number n))]
                [("--load") p "Load library from path before running"
                            (load-path p)]
                [("--save") p "Save library to path after running"
                            (save-path p)]
                [("--draw") d "Render rules + compositions into directory d"
                            (draw-dir d)]
                [("--min-private") n "Min private-interior nodes per template (default 1)"
                                   (min-private (string->number n))]
                [("--min-private-frac") f "Min private/n ratio per template (default 0.5)"
                                        (min-private-frac (string->number f))]
                [("--sexpr-dl") "Score with literal s-expression description length"
                                (use-sexpr-dl #t)]
                [("--canonical") mode "Canonical mode: tentacle-aware (default) or internal-only"
                                 (current-canonical-mode (string->symbol mode))]
                #:args (path) path))

(define (cover-dl G lib cover)
  (cond
    [(use-sexpr-dl)
     (hash-ref (sexpr-principled-dl G lib cover) 'total-dl)]
    [else
     (hash-ref (principled-dl G (grammar-from-cover G cover)) 'total-dl)]))

(define G (load-dot dot-path))
(define library
  (cond [(and (load-path) (file-exists? (load-path)))
         (with-input-from-file (load-path) read)]
        [else '()]))
;; Single shared cache: enumerate-connected-subsets + WL fingerprint
;; for each k get computed at most once across the whole bootstrap
;; run, and reused by every recognise-cover call.
(define rcache (make-recogniser-cache))
(printf "loaded ~a: ~a nodes, ~a edges; library starts at ~a templates~n"
        dot-path (graph-n-nodes G) (graph-n-edges G) (length library))

(define-values (final-library final-dl)
  (let outer ([library library] [round 0] [accepted 0])
    (define cover (recognise-cover G library #:cache rcache))
    (define dl (cover-dl G library cover))
    (printf "~nround ~a: ~a templates, ~a instances, DL=~a~n"
            round (length library) (length cover)
            (real->decimal-string dl 1))
    (define cands
      (propose-templates G cover library
                         #:max-size (max-size)
                         #:min-private (min-private)
                         #:min-private-frac (min-private-frac)))
    (printf "  ~a novel candidates~n" (length cands))
    ;; Best-improvement acceptance: score every candidate, accept the
    ;; one whose extended library reaches the lowest DL. Tiebreakers
    ;; prefer larger templates first, then more edges, then more
    ;; private interior — so when two candidates both drop DL by the
    ;; same amount we pick the more-structural one. No more first-
    ;; improvement coin-flip on the candidate ordering.
    (define scored
      (for/list ([t (in-list cands)])
        (define trial-lib (append library (list t)))
        (define trial-cover (recognise-cover G trial-lib #:cache rcache))
        (define trial-dl (cover-dl G trial-lib trial-cover))
        (list trial-dl t trial-lib)))
    (define improving
      (sort
       (filter (lambda (s) (< (car s) dl)) scored)
       (lambda (a b)
         (define da (car a)) (define db (car b))
         (define ta (cadr a)) (define tb (cadr b))
         (cond
           [(< da db) #t]
           [(> da db) #f]
           ;; Tie on DL — prefer larger template, more edges, more private
           [(> (car ta) (car tb)) #t]
           [(< (car ta) (car tb)) #f]
           [(> (length (caddr ta)) (length (caddr tb))) #t]
           [(< (length (caddr ta)) (length (caddr tb))) #f]
           [else (> (- (car ta) (length (cadr ta)))
                    (- (car tb) (length (cadr tb))))]))))
    (cond
      [(null? improving)
       (printf "  no candidate improved DL; stopping~n")
       (values library dl)]
      [else
       (define best (car improving))
       (define best-dl (car best))
       (define best-t (cadr best))
       (define best-lib (caddr best))
       (printf "  ACCEPT ~v: ~a → ~a (Δ~a, best of ~a improving)~n"
               best-t
               (real->decimal-string dl 1)
               (real->decimal-string best-dl 1)
               (real->decimal-string (- best-dl dl) 1)
               (length improving))
       (outer best-lib (+ round 1) (+ accepted 1))])))

(printf "~nfinal: ~a templates, DL=~a~n"
        (length final-library)
        (real->decimal-string final-dl 1))

;; Show how many instances each template actually contributes to the
;; final cover, alongside its specification. Lots of instances per
;; template = the grammar is genuinely repeating; few instances = the
;; library is bloated.
(let* ([final-cover (recognise-cover G final-library #:cache rcache)]
       [gram (grammar-from-cover G final-cover)]
       [count-by-rule (make-hash)])
  (for ([i (in-list (grammar-instances gram))])
    (hash-update! count-by-rule (instance-rule-id i) add1 0))
  (printf "~ncover composition:~n")
  (for ([r (in-list (sort (grammar-rules gram) >
                          #:key (lambda (r)
                                  (hash-ref count-by-rule
                                            (template-rule-id r) 0))))])
    (define rid (template-rule-id r))
    (define cnt (hash-ref count-by-rule rid 0))
    (printf "  R~a: ~ax  (~an / ~at tents / ~ae)~n"
            rid cnt
            (template-n-interior r)
            (template-n-tentacles r)
            (template-n-owned-edges r))))

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

(when (draw-dir)
  (define d (draw-dir))
  (unless (directory-exists? d) (make-directory* d))
  (printf "~n--- rendering rules + compositions into ~a ---~n" d)
  (for ([t (in-list final-library)] [i (in-naturals)])
    (define title (format "R~a — ~an, tentacles=~a, ~ae"
                          i (car t) (cadr t) (length (caddr t))))
    (define out (build-path d (format "R~a" i)))
    (draw-rule t title (path->string out))
    (printf "  rule R~a → ~a.png~n" i out))
  (define final-cover (recognise-cover G final-library))
  (define final-gram (grammar-from-cover G final-cover))
  ;; Re-stamp cover instances with rule_ids from final-gram.
  (define labelled-cover (grammar-instances final-gram))
  (define cov-out (build-path d "coverage"))
  (draw-coverage G labelled-cover
                 (format "coverage — ~a templates, ~a instances, DL=~a"
                         (length (grammar-rules final-gram))
                         (length labelled-cover)
                         (real->decimal-string final-dl 1))
                 (path->string cov-out))
  (printf "  coverage → ~a.png~n" cov-out)
  (define comps (find-compositions G labelled-cover))
  (printf "~n  ~a composition pair(s) found~n" (length comps))
  (for ([c (in-list comps)] [i (in-naturals)])
    (define key (car c))
    (define count (caddr c))
    (define out (build-path d (format "C~a_R~a-R~a" i (car key) (cdr key))))
    (draw-composition G labelled-cover c (path->string out))
    (printf "  composition R~a↔R~a (~a uses) → ~a.png~n"
            (car key) (cdr key) count out)))
