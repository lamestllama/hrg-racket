#lang racket/base
;; DL primitives shared by every cost function. Pure math; no graph
;; access. Mirrors the helpers in /Users/eric/hrg/hrg/_engine.py so the
;; same numbers come out the other end.

(require racket/math)

(provide log2 log-star log2-binom log2-factorial kt-code-length)

(define (log2 x) (/ (log x) (log 2)))

(define (log-star n)
  ;; log* n: iterated log2, summing logs of intermediate values until
  ;; the value drops to 1 or below.
  (let loop ([k n] [total 0.0])
    (cond
      [(<= k 1) total]
      [else
       (define lk (log2 k))
       (loop (inexact->exact (floor lk))
             (+ total lk))])))

(define (log2-binom n k)
  ;; log2(C(n, k)) via Stirling-ish exact factorial when n is small;
  ;; falls back to lgamma for large n.
  (cond
    [(or (< k 0) (> k n)) -inf.0]
    [(or (= k 0) (= k n)) 0.0]
    [else
     (- (log2-factorial n)
        (log2-factorial k)
        (log2-factorial (- n k)))]))

(define (log2-factorial n)
  (cond
    [(<= n 1) 0.0]
    [else
     ;; sum_{i=2}^{n} log2(i)
     (for/sum ([i (in-range 2 (+ n 1))])
       (log2 i))]))

(define (kt-code-length counts max-count)
  ;; Krichevsky-Trofimov adaptive code over an alphabet of size
  ;; max-count+1 for a sequence of counts. Same formula as Python's
  ;; _kt_code_length.
  (define K (apply + counts))
  (define alphabet (max (+ max-count 1) 2))
  (define n (length counts))
  (cond
    [(= n 0) 0.0]
    [else
     (- (log2-binom (+ K alphabet -1) K)
        (* (- alphabet 1) 0.5 (log2 (max n 1))))]))
