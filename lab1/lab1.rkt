#lang racket

; Exercises

(define (copy-str n str)
  (if (<= n 0)
      ""
      (string-append str (copy-str(- n 1) str))
  )
)

(define (my-even? n)
  (cond
    ((<= n 0) #true)
    ((= n 1) #false)
    (else (my-even? (- n 2)))
    )
)

(define (lower char)
  (define diff (- (char->integer #\a)(char->integer #\A)))
  (integer->char (+ (char->integer char) diff))
)

(define (integer->string i)
  (string (integer->char i))
)

(define (consecutive-chars first last)
  (define first-index (char->integer first))
  (define last-index (char->integer last))
  (define step (if (< first-index last-index) 1 -1))
  (define (iter k)
    (if (= k last-index)
        (integer->string k)
        (string-append (integer->string k)
                       (iter (+ k step)))
        )
    )
  (iter first-index "")
)

;Tasks

;1) num-of-digits
(define (num-of-digits n)
  (cond
    ((= 0 (quotient n 10)) 1 )
    (else (+ 1 (num-of-digits (quotient n 10))))
  )
)

;2)

(define (num->str n [radix 10])
  (define rem (remainder n radix))
  (define initial (if (< rem 10)
                      (char->integer #\0)
                      (- (char->integer #\A) 10)))
  (define rem-str
    (string (integer->char (+ initial rem))))
  (if (< n radix)
      rem-str
      (string-append (num->str (quotient n radix) radix) rem-str))
    
  )

(num->str 12 16)
