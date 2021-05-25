#lang racket

(require racket/trace)

; Creates an empty list
(define new-list '(() ()))
; (define (new-list) (
    ; (list (list) (list))
; ))

; Moves the focus to the left. If this is not possible the program is allowed to crash.
(define (move-left zl) 
    (define f (first zl))
    (define s (second zl)) (
        list (cdr f) (append (list (car f)) s)
))

; Moves the focus to the right. If this is not possible the program is allowed to crash
(define (move-right zl) 
    (define f (first zl))
    (define s (second zl)) (
        list (cons (car s) f) (cdr s)
))

; Rewinds a zipper list so that the focus is on the beginning of the list.
(define (rewind zl) 
    (define f (first zl))
    (define s (second zl)) (
        list '() (append (reverse f) s)
))

; Returns a number that represents the index the zipper list is foccused on. For the example above this would be 3.
(define (index zl) 
    (define f (first zl))
    (define s (second zl)) (
        length f
))

; moves the focus of zl to the n-th index of the list. If n is bigger than the size of the list the program may crash.
(define (move-to n zl) 
    (define f (first zl))
    (define s (second zl)) (
        cond ([< n (index zl)] (move-to n (move-left zl)))
             ([= n (index zl)] zl)
             (else (move-to n (move-right zl)))
))

; Returns the element of the list that is currently to the right of the focus. If there is no element to the right of the focus the program may crash.
(define (get zl) 
    (define f (first zl))
    (define s (second zl)) (
        car s
))

; Updates the element to the right of the focus. If there is no element to the right of the focus, the program may crash.
(define  (update x zl)
    (define f (first zl))
    (define s (second zl)) (
        list f (cons x (cdr s))
))

; Insert a new element to the right of the focus.
(define (insert x zl) 
    (define f (first zl))
    (define s (second zl)) (
        list f (cons x s)
))

; ; Deletes the element to the right of the focus. If there is no element to the right, the program may crash.
(define (delete zl) 
    (define f (first zl))
    (define s (second zl))(
        list f (cdr s)
))

(define t1(insert 1 (insert 2 (insert 3 (insert 4 (insert 5 new-list))))))

(define t2(delete (move-right (move-right (move-right (move-right(insert 102 (move-right(update 101 (move-left (move-left (move-left(update 100 (move-right (move-right (move-right t1))))))))))))))))

(define t3(move-left (move-left t2)))

(define t4 (get t3))
; 3

(define t5(rewind t2))
; (() (101 102 2 3 100))

(define t6(index t3))
; 3

(define t7(move-to 1 t3))
; ((101) (102 2 3 100))

(display t7)