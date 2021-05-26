#lang racket

(require plot)

;;; Lecture 5 - Streams

(time (stream-fold + 0 (in-range 1000000)))
(time (foldl + 0 (range 1000000)))

(define ps (stream-cons (/ 1 0) (/ 1 0)))
;(stream-first ps)
(stream-rest ps)

;;; Explicit definitions of streams
; naturals
(define (nats n)
  (stream-cons n (nats (+ n 1))))

; decimal representation for num/den from (0,1)
(define (expand num den)
  (stream-cons
   (quotient (* num 10) den)
   (expand (remainder (* num 10) den) den)))

(stream->list (stream-take (expand 1 7) 10))
(stream->list (stream-take (expand 1 2) 10))
(stream->list (stream-take (expand 1 3) 10))

;;; Implicit definitions
; constant stream
(define ones (stream-cons 1 ones))
(stream->list (stream-take ones 10))

; cyclic streams
(define ab (stream-cons 'a (stream-cons 'b ab)))
(stream->list (stream-take ab 10))

(define abc (stream* 'a 'b 'c abc))
(stream->list (stream-take abc 10))

; stream-append arguments are not delayed
; we can redefine it to define cyclic streams
(define (stream-append s1 s2)
  (if (stream-empty? s1)
      (force s2)
      (stream-cons (stream-first s1) (stream-append (stream-rest s1) s2))
      )
  )
(define week-days '(mon tue wed thu fri sat sun))
(define stream-days (stream-append week-days (delay stream-days)))

; summing infinite streams
(define (add-streams s1 s2)
  (stream-cons (+ (stream-first s1)
                  (stream-first s2))
               (add-streams (stream-rest s1)
                            (stream-rest s2)))
  )

; natural numbers defined implicitly
(define nats2 (stream-cons 0 (add-streams ones nats2)))
(stream->list (stream-take nats2 10))

; scaling a stream by a constant
(define (scale-stream s c)
  (stream-map ((curry *) c) s))

(define (integral integrand initial-value dt)
  (define int
    (stream-cons
     initial-value
     (add-streams (scale-stream integrand dt) int)))
  int)

; stream of sin(x) sampled with steps of size 0.001
(define sin-stream (stream-map sin (scale-stream nats2 0.001))) 
(stream-ref (integral sin-stream -1 0.001) 3141) ; integral of sin x from 0 to pi

; integral with delayed integrand
(define (delayed-integral delayed-integrand initial-value dt)
  (define int
    (stream-cons
     initial-value
     (let ((integrand (force delayed-integrand)))
       (add-streams (scale-stream integrand dt) int))))
  int)

; solver of y'(t) = f(y(t)) with the initial condition y0
(define (solve f y0 dt)
  (define y (delayed-integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

; y' = y, y(0)=1 => y(t) = e^t
(define sol (solve identity 1 0.001))
(stream-ref sol 1000) ; Euler number e
(define ys (stream->list (stream-take sol 1000)))
(define xs (map ((curry *) 0.001) (range 1000)))
(plot (lines (map list xs ys)))

;;; Queues
; Naive implementation of a queue by a single list
(define naive-head car)
(define naive-tail cdr)
(define (naive-snoc x q)
  (append q (list x)))

; enqueue a list of elements
(define (naive-snoc-seq lst q)
  (foldl naive-snoc q lst)
  )

; dequeue n elements
(define (naive-tail-seq n q)
  (foldl (lambda (x y) (naive-tail y)) q (range n)))

;;; Better queue with two lists
; queue = (frontal list f, rear list r) - enqueuing to r, dequeuing from f 
(define (make-queue f r) (list f r))
(define get-f car)
(define get-r cadr)

(define empty-queue
  (make-queue '() '()))

(define (queue-empty? q)
  (null? (get-f q))
  )

; checks if frontal list is empty and reverses the rear list if this is the case
(define (check q)
  (if (null? (get-f q))
      (make-queue (reverse (get-r q)) '())
      q))

(define (snoc x q)
  (check (make-queue (get-f q) (cons x (get-r q)))))

(define head (compose car get-f))

(define (tail q)
  (check (make-queue (cdr (get-f q))
                     (get-r q))))

; enqueue a list of elements
(define (snoc-seq lst q)
  (foldl snoc q lst))

; dequeue n elements
(define (tail-seq n q)
  (foldl (lambda (x y) (tail y)) q (range n)))

; performance tests naive versus better
; 10000 elements enqueued and then dequeued
(time (begin (naive-tail-seq 10000 (naive-snoc-seq (range 10000) '())) 'done))
(time (begin (tail-seq 10000 (snoc-seq (range 10000) empty-queue)) 'done))

;;; Breath first search

; graph from Lecture 2
(define g '((1 6) (1 2) (2 1) (1 3) (3 2) (2 4) (2 5) (3 4) (4 1) (4 5)))

; Get successor of a given vertex
(define (get-successors vertex edges)
  (map cadr (filter (lambda (e) (eqv? (car e) vertex)) edges))
  )

; Filter out already visited nodes
(define (filter-visited path lst)
  (filter (lambda (v) (not (member v path))) lst)
  )

; breadth first search
(define (bfs start terminal edges)
  (define q (snoc (list start) empty-queue)) ; initialize queue
  (define (iter q)                          ; iterate over queue of partial paths (each path ordered reversely)
    (displayln q)
    (if (queue-empty? q)  
        #f                                  ; if queue empty, there is no path
        (let ([path (head q)])              ; otherwise take the partial path from the queue
          (if (eqv? (car path) terminal)    ; does it end in terminal?
              (reverse path)                ; if yes, return result reversely ordered
              (let* (
                     [succ (filter-visited path (get-successors (car path) edges))]  ; otherwise take successors
                     [ext-paths (map ((curryr cons) path) succ)]                     ; extend path by all successors
                     )
                (iter (snoc-seq ext-paths (tail q)))  ; enqueue extended partial paths and dequeue path and iterate
                )
              )
          )
        )
    )
  (iter q)
  )

(bfs 1 5 g)

; function generating random digraph with a given probability (0-100) for an edge 
(define (generate-random-digraph num-of-nodes prob)
  (for*/list ([i (in-range num-of-nodes)]
              [j (in-range num-of-nodes)]
              #:when (< (random 100) prob))
    (list i j)
    )
  )
