#lang racket

(require racket/trace)
(require rackunit)

; Count Valid Matching of Parentheses

(define (find_catalan_number n [accumulator 1] [counter 2]) (
    cond ([eq? n 0] 1)
         ([eq? n 1] 1)
         ([eq? counter n] (* accumulator ( / (+ counter n) counter)))
         (else (find_catalan_number n (* accumulator ( / (+ counter n) counter)) (+ counter 1)))
))


(trace find_catalan_number)
(find_catalan_number 4)