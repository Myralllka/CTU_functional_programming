#lang racket

(require racket/trace)

(define (rbp n) (
    cond ([= n 0] 1)
         ([= n 1] 2)
         ([= n 3] 5)
         (else (+ (rbp (- n 1)) (rbp (- n 2))))
))

(map rbp '(0 1 2 10 20 40))