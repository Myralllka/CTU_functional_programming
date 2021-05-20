#lang racket
(require racket/trace)

(define (get-ij i j A) (
    list-ref (list-ref A i) j
))

(define (put-ij i j A el) (
    list-set A i (list-set (list-ref A i) j el)
))

(define (cons_mat size) 
    (define row (build-list size (lambda (x) 0)))(
    build-list size (lambda (x) row)
))

(define (sq el) (
    * el el
))

(define (create_cholesky A) 
    (define size (length A))
    (define (sum_elements i j L [k 0] [res 0]) (
        cond 
            ([= k j] res)
            (else (sum_elements i j L (+ k 1) (+ res (* (get-ij j k L) (get-ij i k L)))))
    ))
    (trace sum_elements)
    (trace sq)
    (trace get-ij)
    ; (print size)
    (define (construct A [i 0] [j 0] [L (cons_mat size)]) (
        cond 
            ([= i size] L)
            ([= j size] (construct A (+ i 1) 0 L))
            ([= i j] (construct A i (+ j 1) (put-ij i j L (sqrt (- (get-ij j j A) (sum_elements i j L))))))
            ([< i j] (construct A i (+ j 1) (put-ij i j L 0)))
            (else (construct A i (+ j 1) (put-ij i j L (/ (- (get-ij i j A) (sum_elements i j L)) (get-ij j j L)))))
    ))
    (trace construct)
    (
    construct A
))
(trace create_cholesky)

; (create_cholesky '((1 2)(2 5)))
(create_cholesky (list (list 4 12 -16) (list 12 37 -43) (list -16 -43 98)))