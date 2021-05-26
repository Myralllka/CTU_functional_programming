#lang racket

(require racket/trace)
(provide nGramFreq)


(define (update_map key inp_map) (
    cond ([null? inp_map] (cons (list key 1) '())) ; add element if doesn`t exists
         ([equal? (caar inp_map) key] (cons (list key (+ (first (cdar inp_map)) 1)) (cdr inp_map)))
         (else (cons (car inp_map) (update_map key (cdr inp_map))))
))

; (trace update_map)
(define (split_array_on_n n lst (res '())) (
    cond ([< (length lst) n] res)
         (else (split_array_on_n n (cdr lst) (cons (take lst n) res)))
))

; (update_map "asddd" '(("as" 1) ("asd" 2)))

(define (count_pairs pairs (res '())) (
    cond ([null? pairs] res)
         (else (count_pairs (cdr pairs) (update_map (car pairs) res)))
))

(trace count_pairs)

(define (nGramFreq n str)
    (define str_lst (string->list str))  
    (define pairs (split_array_on_n n str_lst))
    (define elements (map list->string pairs))
    (define ngrams (count_pairs elements))
        (sort (sort ngrams  #:key first string<?) #:key second >)

)
