#lang racket

(require racket/trace)
(require rackunit)

; Queue using Two Stacks, LIFO using FIFO's

; Stack
; ush that adds a new element v to a stack st (represented as a list):
(define (push v st)(
     cons v st
))

(define (pop st)(
    cdr st
))

; Queue

; enqueue, dequeue, dispatch () methods

(define (inStackOutStack ist ost)(
    list ist ost
))

(define (enqueue el q) (
    list (push el (first q)) (second q)
))

(define (dispatch q) (
    cond ([null? (first q)] q)
         (else (dispatch (inStackOutStack (pop (first q)) (push (caar q) (second q)))))
))

(define (dequeue q) (
    cond ([and (null? (first q)) (null? (second q))] q)
         ([null? (second q)](dequeue (dispatch q)))
         (else (inStackOutStack (first q) (pop (second q))))
))

; Ex 1. Stacks
(define emptyStack '())
(define stack0
 (push 0 emptyStack))
(define stack01
 (push 1 stack0))
(define completeStack
 (push 7 (push 6 (push 5 (push 4 (push 3 (push 2 stack01)))))))
(define stack42
 (push 7 (push 6 (push 5 (push 42 (pop (pop (pop (pop completeStack)))))))))
(define inStack
 (push 7 (push 6 (push 5 (push 4 emptyStack)))))
(define outStack
 (push 3 (push 2 (push 1 (push 0 emptyStack)))))
(define listOfStacks
 (list emptyStack stack0 stack01 completeStack stack42 inStack outStack))

; (map displayln listOfStacks)

; Example 2. Queues
(define q1
    (inStackOutStack inStack outStack))
(define q2
    (inStackOutStack emptyStack outStack))
(define q3
    (inStackOutStack inStack emptyStack))
(define listOfQueues
    (list q1 q2 q3))

(map (lambda (q) (displayln (dequeue (dequeue (dequeue (dequeue (dequeue q))))))) listOfQueues)
