#lang racket

;(sort '(1 2 6 7 5 4 3 2) (lambda (x y)(> x y)))
;(filter (lambda (x)(= x 1)) '(1 3 4 5 4 1 1 1))
;(map (lambda (x)(* x x)) '(1 2 3 4 5))



;(string-downcase "aasASA")
;(string->list (string-downcase "aAaA"))


;ex 1: reverse a list

(define (my-reverse lst [acc '()])
  (if (null? lst)
      acc
      (my-reverse(cdr lst) (cons (car lst) acc))
      )
  )

;ex 2 prerequisites
;
;(define (letter-frequencies str)
;  (define lower (string-downcase str))
;  (define (filter-alphabetic str_list [acc '()])
;    (filter(lambda(x)(char-alphabetic? x)) str_list))
;  (filter-alphabetic(string->list lower))
;  )


;(letter-frequencies "AasdD")
;(trace my-reverse)
;(my-reverse (list 1 2 3 4))

;Task 1. find average

(define (average lst)
 (define (sum lst [sm 0])
    (if (= (length lst) 0)
        0
        (+ (car lst)  (sum (cdr lst) sm) )
        )
    )
  (/ (sum lst) (length lst))
  )

;(average '(3 3 3 3 6))

;Task 2. split list on n parts

;(define (split-list n lst)
  
 ; )
