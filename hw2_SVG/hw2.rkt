#!/usr/bin/racket

#lang racket

(require racket/trace)
(require rackunit)
(require racket/match)
(require (planet williams/describe/describe))
; (provide execute)

(define result "")

(define (svg_general pars input_env) (match-define (list w h) pars) (
    set! result (string-append result (format "<svg width=\"~a\" height=\"~a\">" w h))
))

(define (svg_circle pars input_env) (match-define (list x y r style) pars) (
    set! result (string-append result (format "<circle cx=\"~a\" cy=\"~a\" r=\"~a\" style=\"~a\"/>" 
        ((evaluate_num_expression input_env ) (get_el_from_env x input_env))
        ((evaluate_num_expression input_env ) (get_el_from_env y input_env))
        ((evaluate_num_expression input_env ) (get_el_from_env r input_env)) style))
))

(define (svg_rect pars input_env) (match-define (list x y width height style) pars) (
    set! result (string-append result (format "<rect x=\"~a\" y=\"~a\" width=\"~a\" height=\"~a\" style=\"~a\"/>" 
        ((evaluate_num_expression input_env ) (get_el_from_env x input_env))
        ((evaluate_num_expression input_env ) (get_el_from_env y input_env))
        ((evaluate_num_expression input_env ) (get_el_from_env width input_env))
        ((evaluate_num_expression input_env ) (get_el_from_env height input_env)) style))
))

(define (svg_line pars input_env) (match-define (list x1 y1 x2 y2 style) pars) (
    set! result (string-append result (format "<line x1=\"~a\" y1=\"~a\" x2=\"~a\" y2=\"~a\" style=\"~a\"/>" 
        ((evaluate_num_expression input_env ) (get_el_from_env x1 input_env))
        ((evaluate_num_expression input_env ) (get_el_from_env y1 input_env))
        ((evaluate_num_expression input_env ) (get_el_from_env x2 input_env))
        ((evaluate_num_expression input_env ) (get_el_from_env y2 input_env)) style))
))

(define (evaluate_bool_expression env) (
    lambda (lst) (
        cond
            ([empty? lst] (error ("evaluate_bool_expression: empty list input")))
            ([number? lst] lst)
            ([equal? (car lst) '= ] (= ((evaluate_num_expression env) (get_el_from_env (second lst) env)) ((evaluate_num_expression env) (get_el_from_env (third lst) env))))
            ([equal? (car lst) '> ] (> ((evaluate_num_expression env) (get_el_from_env (second lst) env)) ((evaluate_num_expression env) (get_el_from_env (third lst) env))))
            ([equal? (car lst) '< ] (< ((evaluate_num_expression env) (get_el_from_env (second lst) env)) ((evaluate_num_expression env) (get_el_from_env (third lst) env))))
            (else (error "evaluate_bool_expression: unimplemented expression"))
    )
))

(define (evaluate_num_expression env) (
    lambda (lst) (
        cond
            ([empty? lst] (error ("evaluate_num_expression: empty list input")))
            ([number? lst] lst)
            ([symbol? lst] lst)
            ([string? lst] lst)
            ([number? (car lst)] (car lst))
            ([equal? (car lst) '+ ] (+ ((evaluate_num_expression env) (get_el_from_env (second lst) env)) ((evaluate_num_expression env) (get_el_from_env (third lst) env))))
            ([equal? (car lst) '- ] (- ((evaluate_num_expression env) (get_el_from_env (second lst) env)) ((evaluate_num_expression env) (get_el_from_env (third lst) env))))
            ([equal? (car lst) '/ ] (/ ((evaluate_num_expression env) (get_el_from_env (second lst) env)) ((evaluate_num_expression env) (get_el_from_env (third lst) env))))
            ([equal? (car lst) '* ] (* ((evaluate_num_expression env) (get_el_from_env (second lst) env)) ((evaluate_num_expression env) (get_el_from_env (third lst) env))))
            ([equal? (car lst) 'cos] (cos ((evaluate_num_expression env) (get_el_from_env (second lst) env))))
            ([equal? (car lst) 'sin] (sin ((evaluate_num_expression env) (get_el_from_env (second lst) env))))
            ([equal? (car lst) 'floor] (floor ((evaluate_num_expression env) (get_el_from_env (second lst) env))))
            (else (error "evaluate_num_expression: unimplemented expression"))
    )
))

(define (map_num_of_values mp key [res 0])
(
    cond
        ([empty? mp] res)
        ([equal? key (caar mp)] (map_num_of_values (cdr mp) key (+ res 1)))
        (else (map_num_of_values (cdr mp) key res))
))

; (define (find_last val env) (
;     cond
;         ([empty? env] '())
;         ([equal? (car (last env)) val] (car (last env)))
;         (else (find_last val (drop-right env 1)))
; ))

; return new updated envieronment
(define (update_env_value key val env) 
; (print " key: ")
; (print key)
; (print " val: ")
; (print val)
; (print " env: ")
; (print env)
; (display "\n")
    (
    cond 
        ([empty? env] env)
        ([= (map_num_of_values env key) 0] (
            append env (list(list key val))
        ))
        ([= (map_num_of_values env key) 1] (
            map (lambda (x) (cond ([equal? (first x) key] (list key val))(else x))) env
        ))
        (else env)
))

(define (get_el_from_env el in_env) (
    cond 
        ([not(symbol? el)] el)
        ([empty? in_env] '(()))
        ([equal? (caar in_env) el] (cdar in_env))
        (else (get_el_from_env el (cdr in_env)))
))

(define (expand_env lst env)(
    map (lambda (x) (cond ([symbol? x] (car (get_el_from_env x env))) (else x))) lst
))

(define (evaluate_function lst environment)
    (define proc (cond ([empty? lst] '()) (else (get_el_from_env (caar lst) environment))))
    (cond
        ([empty? lst] '())
        (else (
            cond
                ([equal? (caar lst) 'if] (123))
                ([equal? (caar lst) 'when] (
                    cond 
                        ([(evaluate_bool_expression environment) (map (evaluate_num_expression environment)  (second (car lst)))] 
                            (evaluate_function (cddar lst) environment))
                ))
                ([empty? proc] (error ("evaluate_function: empty expr")))    
                (else (begin0
                            ((car proc) (expand_env (cdar lst) environment) environment)
                            (evaluate_function (cdr lst) environment)
                        ))
                
        ))
    )
)

(define (make_new_function input_lst [input_env '()]) 
    (define (expand_env lst pars env) (
        cond
            ([or (empty? lst) (not (list? lst))] (append env '()))
            (else (expand_env (cdr lst) (cdr pars) (update_env_value (car lst) ((evaluate_num_expression env) (car pars)) env)))
    ))
    (lambda (args env) (evaluate_function (cdr input_lst) (expand_env (cdar input_lst) args env))
))

(define (run_prg lst [env '()]) (
    cond
        ; if there are few functions to define
        ([empty? lst] env)
        ([> (length lst) 1] (run_prg (cdr lst) (append env (list(list (caadar lst) (make_new_function (cdr (car lst)) env))))))
        ; if only one
        ([eqv? (caar lst) 'define] (append env (list(list (caadar lst) (make_new_function (cdar lst) env)))))
        (else (error "unimplemented"))
))

; (trace get_el_from_env)
; (trace evaluate_function)
; (trace evaluate_num_expression)
; (trace make_new_function)
; (trace run_prg)

(define test1
    '(
        (define (start x y)
            (rect x y 1000 1000 "fill:blue")
            (rect 1000 0 1000 1000 "fill:blue")
            (rect 2000 0 1000 1000 "fill:blue")
        )
        ; (define (finish x y)
        ;     (start x y)
        ;     (rect (* 5 x) (* 5 y) 100 100 "fill:green")
        ;     (rect 100 0 100 100 "fill:green")
        ;     (rect 200 0 100 100 "fill:green")
        ; )
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
))

(define test2
'(
    (define (circles x r)
        (when (> r 10)
            (circle x 200 r "fill:white;stroke:green")
            (circles (+ x (floor (/ r 2.0))) (floor (/ r 2)))
        )
    )
)
)

; (define default_env (list (list 'rect svg_rect) (list 'circle svg_circle) (list 'line svg_line)))
; (define expr '(circle 200 200 (floor (/ 200 3)) "fill:red"))
; (map (lambda (x) (evaluate_num_expression x default_env)) '(200 200 (floor (/ 200 3)) "fill:red"))
; (expretion_to_run (map (evaluate_num_expression default_env) (cdr expr)) default_env)
; (define ev (run_prg test1 default_env))
; (evaluate_function '(start 5 7) ev)
; (expand_env '(rect x 10 100 100 "fill:red") ev)
; ((car (get_el_from_env "start" ev)) '(100 200) ev)
; (print result)
; (car (get_el_from_env "circle" ev))
; (run_prg test1)

;prg is an SVGen program consisting of function definitions
; expr is an expression to be evaluated (typically, it is a function call of a function defined in prg)
(define (execute width height prg expr)
    (define default_env (list (list 'rect svg_rect) (list 'circle svg_circle) (list 'line svg_line)))
    (svg_general (list width height) default_env) 
    (define ev (run_prg prg default_env))
    (define expretion_to_run (car (get_el_from_env (car expr) ev)))
    ; (trace expretion_to_run)
    (begin
     (cond   
        ([empty? expretion_to_run] (error ("unimplemented yet")))
        ([= (length expr) 1] (expretion_to_run expr ev))
        ([not (= (length expr) 1)] (expretion_to_run (map (evaluate_num_expression ev) (cdr expr)) ev))
        (else (error ("wtf"))))
    (string-append result "</svg>")
))
; (trace execute)
; (display result)
;;;;;;;;;;;;;;;;;;;;;;;;; TESTS ;;;;;;;;;;;;;;;;;;;;;;;;;

; (display (execute 400 400 '() '(line 10 20 30 40 "stroke:black;stroke-width:5")))
; (display (execute 400 400 '() '(circle 200 200 (floor (/ 200 3)) "fill:red")))





; (display (execute 400 400 test1 '(start 121 213)))
; (display (execute 400 400 test2 '(circles 200 200)))
(display(execute 400 400 test3 '(recur-circ 200 200 100)))

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
            (draw x1
                y1
                (+ x1 (* len (cos angle)))
                (+ y1 (* len (sin angle)))
                len
                angle
            )
        )
    )
)
; (display (execute 400 400 tree-prg '(recur-tree 200 400 100 (* 3.14 1.5))))