#lang racket

(require "hw2.rkt")


(define test1
'(
    (define (start)
        (rect 100 200 1000 1000 "fill:blue")
        (rect 1000 0 1000 1000 "fill:blue")
        (rect 2000 0 1000 1000 "fill:blue")
    )
)   
)

(define test2
'(
(define (circles x r)
    (when (> r 10)
        (circle x 200 r "fill:white;stroke:green")
        (circles (+ x (floor (/ r 2.0))) (floor (/ r 2)))
    )
    (when (> r 10)
        (circle 200 x r "fill:white;stroke:red")
        (circles (+ x (floor (/ r 2.0))) (floor (/ r 2)))
    )
)
)
)

(define test3 
'(
(define (recur-circ x y r)
    (circle x y r "fill:lightgreen;opacity:0.5;stroke:black;stroke-width:2")
    (when (> r 15)
        (recur-circ (+ x r) y (floor (/ r 2)))
        (recur-circ (- x r) y (floor (/ r 2)))
        (recur-circ x (+ y r) (floor (/ r 2)))
        (recur-circ x (- y r) (floor (/ r 2)))
    )
)
)
)

; (display (execute 400 400 '() '(line 10 20 30 40 "stroke:black;stroke-width:5")))
; (display (execute 400 400 '() '(circle 200 200 (floor (/ 200 3)) "fill:red")))


; (display (execute 400 400 test2 '(circles 200 200)))
; (display(execute 400 400 test3 '(recur-circ 200 200 100)))

(define tree-prg
    '(
        (define (draw x1 y1 x2 y2 len angle)
            (if (> len 30)
                (line x1 y1 x2 y2 "stroke:black;stroke-width:2;opacity:0.9")
                (line x1 y1 x2 y2 "stroke:green;stroke-width:3;opacity:0.9")
            )
            (when (> len 20)
                (recur-tree x2 y2 (floor (* len 0.7)) angle)
                (recur-tree x2 y2 (floor (* len 0.7)) (+ angle 0.3))
                (recur-tree x2 y2 (floor (* len 0.7)) (- angle 0.6))
            )
        )
        (define (recur-tree x1 y1 len angle)
            (draw x1 y1 (+ x1 (* len (cos angle))) (+ y1 (* len (sin angle))) len angle )
        )
    )
)


; (display (execute 400 400 test1 '(start)))
(display (execute 400 400 tree-prg '(recur-tree 200 400 100 (* 3.14 1.5))))