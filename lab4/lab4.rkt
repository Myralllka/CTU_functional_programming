#lang racket

(require racket/trace)
; Ex 1. permutations.

(define l1 '((2 3) (3 2)))

(define (interleave el)
  (lambda (lst)
    (if (null? lst)
        (list (list el))
        (cons (cons el lst)
              (map ((curry cons) (car lst))
                   ((interleave el) (cdr lst))))
        )
    )
  )

(define (permutations lst)
  (if (null? lst)
      '(())
      (apply append
             (map (interleave(car lst)) (permutations(cdr lst))))
      )
  )

; Ex 2. bool tree

; Task 1
;Write a function (sub-seq lst) taking a list lst and returning a list of all its sublists/subsequences. E.g.
;(sub-seq '(1 2 3)) =>
;  (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

(define (sub-seq lst)
  (if (null? lst)
      '(())
      (let ([el (car lst)]
            [rest-sub-seq (sub-seq (cdr lst))])
        (append rest-sub-seq
                (map ((curry cons) el) rest-sub-seq))
        )
      )
  )

(trace sub-seq)