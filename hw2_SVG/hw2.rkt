#!/usr/bin/racket

#lang racket

(require racket/trace)
(require rackunit)
(require racket/match)
(require xml)

(provide execute)


;prg is an SVGen program consisting of function definitions
; expr is an expression to be evaluated (typically, it is a function call of a function defined in prg)
(define (execute width height prg expr)
(define my_result "")
(define (svg_general pars input_env) (match-define (list w h) pars) (
    set! my_result (string-append my_result (format "<svg width=\"~a\" height=\"~a\">" w h))
))
(define (svg_circle pars input_env) (match-define (list x y r style) pars) (
    set! my_result (string-append my_result (format "<circle cx=\"~a\" cy=\"~a\" r=\"~a\" style=\"~a\"/>" 
        ((evaluate_num_expression input_env ) (get_el_from_env x input_env))
        ((evaluate_num_expression input_env ) (get_el_from_env y input_env))
        ((evaluate_num_expression input_env ) (get_el_from_env r input_env)) style))
))
(define (svg_rect pars input_env) (match-define (list x y width height style) pars) (
    set! my_result (string-append my_result (format "<rect x=\"~a\" y=\"~a\" width=\"~a\" height=\"~a\" style=\"~a\"/>" 
        ((evaluate_num_expression input_env ) (get_el_from_env x input_env))
        ((evaluate_num_expression input_env ) (get_el_from_env y input_env))
        ((evaluate_num_expression input_env ) (get_el_from_env width input_env))
        ((evaluate_num_expression input_env ) (get_el_from_env height input_env)) style))
))
(define (svg_line pars input_env) (match-define (list x1 y1 x2 y2 style) pars) (
    set! my_result (string-append my_result (format "<line x1=\"~a\" y1=\"~a\" x2=\"~a\" y2=\"~a\" style=\"~a\"/>" 
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

; return new updated envieronment
(define (update_env_value key val env)
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
            ([equal? (caar lst) 'if] (begin0
                (if  [(evaluate_bool_expression environment) (map (evaluate_num_expression environment)  (second (car lst)))]
                    (evaluate_function (list (third (car lst))) environment)
                    (evaluate_function (list (fourth (car lst))) environment))
                (evaluate_function (cdr lst) environment)
            ))
            ([equal? (caar lst) 'when] (begin0
                (cond 
                    ([(evaluate_bool_expression environment) (map (evaluate_num_expression environment)  (second (car lst)))] 
                        (evaluate_function (cddar lst) environment)))
                (evaluate_function (cdr lst) environment)
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
        (else (error ("Unknown error"))))
    (string-append my_result "</svg>")
))