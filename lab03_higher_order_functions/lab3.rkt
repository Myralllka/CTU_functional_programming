#lang racket

;Ex 1

(define (mult-all-pairs lst1 lst2)
  (apply   ; flatten the result
   append
   (map
    (lambda (x) (map ((curry *) x) lst2)) ; multiply all elements of lst2 by x
    lst1)                                 ; do it for each element x in lst1
   )
  )
 
(define (f-all-pairs f lst1 lst2)
  (apply
   append
   (map
    (lambda (x) (map ((curry f) x) lst2))
    lst1)
   )
  )

; Ex 2
(define (get-coef m) (car m)) ; first component
(define (get-exp m) (cadr m)) ; second component


(define (add-mon m1 m2)
  (list (+ (get-coef m1) (get-coef m2)) ; sum coefficients
        (get-exp m1))                   ; keep exponent
  )
 
(define (mult-mon m1 m2)
  (list (* (get-coef m1) (get-coef m2)) ; multiply coefficients
        (+ (get-exp m1) (get-exp m2)))  ; sum exponents
  )


(define (add-mon-pol mon pol)
  (define (same-exp? m) (= (get-exp mon) (get-exp m))) ; #t if m has the same exponent as mon
  (define same-mon (filter same-exp? pol))             ; list containing the monomial of the same exponent or empty list
  (define rest (filter (compose not same-exp?) pol))   ; remaining monomials of different exponents
  (if (null? same-mon)                                 
      (cons mon rest)
      (cons (add-mon mon (car same-mon)) rest))
  )

(define (normalize p)
  (define (non-zero-coef? m) (not (= 0 (get-coef m)))) 
  (sort
   (filter non-zero-coef? p)
   (lambda (p1 p2) (< (get-exp p1) (get-exp p2))))
  )
 
(define (add-pol p1 p2)
  (normalize (foldl add-mon-pol p1 p2))
  )
 
(define (mult-pol p1 p2)
  (normalize (foldl add-mon-pol '() (f-all-pairs mult-mon p1 p2)))
  )

