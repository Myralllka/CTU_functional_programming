#lang racket

(require racket/trace)

; (provide cheap-flight)

(define (nodes gr) (car gr))
(define (edges gr) (cadr gr))
(define (cost edge) (caddr edge))


; takes a starting node a , a destination node b , a
; graph gr , and returns a list containing the cheapest path from start to destination, as well as the
; total cost. If there is no path from a to b return #f .

(define (cheapflight a b gr) (
    rest-sub-seq gr
))


; (define (cheaper? x y) (< (cadr x) (cadr y)))