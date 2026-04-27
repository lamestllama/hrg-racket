#lang racket/base
;; Description length OVER s-expressions. The grammar lives as Racket
;; values in memory, so the most honest MDL objective is "bits to
;; write down those values" using a uniform prefix-free encoding.
;;
;; Encoding rules:
;;   - Integer k         : Elias-gamma, ~ 2 * (1 + integer-length(k+1))
;;   - Symbol            : log2(vocabulary-size) bits
;;   - Empty list        : 1 bit
;;   - Pair (a . b)      : 1 bit (cons marker) + dl(a) + dl(b)
;;
;; current-symbol-bits is parameterised — for graph-node symbols set it
;; to log2(|V(G)|).

(require racket/math racket/list)

(provide
 sexpr-dl
 integer-dl
 current-symbol-bits)

(define current-symbol-bits (make-parameter 8.0))

(define (integer-dl k)
  (define m (+ 1 (abs k)))
  (define len (max 1 (integer-length m)))
  (* 2.0 len))

(define (sexpr-dl e)
  (cond
    [(null? e) 1.0]
    [(integer? e) (integer-dl e)]
    [(symbol? e) (current-symbol-bits)]
    [(pair? e)
     (+ 1.0 (sexpr-dl (car e)) (sexpr-dl (cdr e)))]
    [else
     (error 'sexpr-dl "cannot encode ~a" e)]))
