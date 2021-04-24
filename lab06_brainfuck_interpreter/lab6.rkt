#lang racket
(require 2htdp/image)

(require racket/trace)
(require rackunit)

; Task 1

(define (make-tape left val right)
    (list left val right))
 
(define (fresh-tape size)
    (make-tape '() 0 (make-list (- size 1) 0)))
 
(define (get msg)
    (cond
        ([eqv? msg 'left] car)
        ([eqv? msg 'val] cadr)
        ([eqv? msg 'right] caddr)
        (else (error "unknown message"))
    )
)

; takes an operation + (resp. -) and a tape and returns a new tape where the active 
; number is increased (resp. decreased).
(define (change op tape)
    (list ((get 'left) tape) (op ((get 'val) tape) 1) ((get 'right) tape))
)
 

(define tape (make-tape '(3 2 1) 4 '(5 6 7))) 

;;; tape
;;; (change - tape)
;;; (change + tape)

; takes a direction 'left (resp. 'right) and a tape and returns a 
; new tape where the pointer moves left (resp. right).
(define (move dir tape)
    (define left ((get 'left) tape))
    (define right ((get 'right) tape))
    (define val ((get 'val) tape))
    (
        cond ([eqv? dir 'left] (make-tape (drop-right left 1) (car(reverse left)) (append (list val) right)))
            ([eqv? dir 'right] (make-tape (append left (list val)) (car right) (cdr right)))
            (else (error "Wrong direction"))
    )
)

;;; (move 'left tape)
;;; (move 'right tape)

; Task 2
 
;;; ; constant defining the size of tape
;;; (define SIZE 10)
 
;;; ; constructor of the object tape of a given size
;;; (define (make-tape size)
;;;   (define tape (make-vector size 0))   ; initialize fresh tape
;;;   (define ptr 0)                       ; pointer points to the first element
 
;;;   (define (change op vec ptr)
;;;     (vector-set! tape ptr (op (vector-ref tape ptr) 1)))
 
;;;   (define (move op ptr)
;;;     (let ([new-ptr (op ptr 1)])
;;;       (if (or (< new-ptr 0) (> new-ptr size))
;;;           (error "Moving outside tape")
;;;           new-ptr)))
 
;;;   (lambda (msg)
;;;     (cond
;;;       ([eqv? msg 'tape] tape)
;;;       ([eqv? msg 'plus] (change + tape ptr))
;;;       ([eqv? msg 'minus] (change - tape ptr))
;;;       ([eqv? msg 'left] (set! ptr (move - ptr)))
;;;       ([eqv? msg 'right] (set! ptr (move + ptr)))
;;;       ([eqv? msg 'dot] (vector-ref tape ptr))
;;;       ([eqv? msg 'comma] (lambda (val) (vector-set! tape ptr val)))
;;;       ([eqv? msg 'reset] (vector-fill! tape 0) (set! ptr 0))
;;;       )
;;;     ))
 
;;; ; defines a global tape used by the interpreter
;;; (define tape (make-tape SIZE))
 
;;; ; evaluates comma command, i.e., (car input) -> tape[ptr]
;;; (define (eval-comma prg input)
;;;   (cond
;;;     ([null? input] (error "Empty input"))
;;;     (else ((tape 'comma) (car input))
;;;           (eval-prg prg (cdr input)))))  ; recursive call preocessing further commands
 
;;; ; evaluates all the commands beside comma
;;; (define (eval-cmd cmd prg input)
;;;   (cond
;;;     ([eqv? cmd '+] (tape 'plus)) 
;;;     ([eqv? cmd '-] (tape 'minus)) 
;;;     ([eqv? cmd '<] (tape 'left)) 
;;;     ([eqv? cmd '>] (tape 'right)) 
;;;     ([eqv? cmd '*] (printf "~a " (tape 'dot))) 
;;;     (else (error "Unknown command")))
;;;   (eval-prg prg input)   ; recursive call preocessing further commands
;;;   )
 
;;; (define (eval-cycle cycle prg input)
;;;   (if (= (tape 'dot) 0)                         ; is cycle is finished? 
;;;       (eval-prg prg input)                      ; if yes, recursive call preocessing further commands
;;;       (let ([new-input (eval-prg cycle input)]) ; otherwise evaluate cycle code
;;;         (eval-cycle cycle prg new-input)        ; and execute the cycle again       
;;;         )
;;;       ))
 
;;; (define (eval-prg prg input)
;;;   (if (null? prg)                 ; are all commands processed?
;;;       input                       ; if yes, return remaining input
;;;       (let ([cmd (car prg)]
;;;             [rest (cdr prg)])
;;;         (cond
;;;           ([eqv? cmd '@] (eval-comma rest input))      
;;;           ([list? cmd] (eval-cycle cmd rest input))
;;;           (else (eval-cmd cmd rest input))))
;;;       ))
 
;;; ; executes the given program with the given input
;;; (define (run-prg prg input)
;;;   (tape 'reset)              ; fill tape by zeros
;;;   (eval-prg prg input)       ; evaluate program
;;;   (printf "done~n")
;;;   )
