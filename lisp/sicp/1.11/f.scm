(define (f n)
  (if (< n 3) 
    n 
    (+ (f (- n 1))
       (* 2 (f (- n 2)))
       (* 3 (f (- n 3))))))

(define (f-iter n) 
  (define (iter a b c count) 
    (cond ((= count 0) 0) 
          ((= count 1) b)
          ((= count 2) c)
          (else (iter b c (+ c (* 2 b) (* 3 a)) (- count 1)))))
  (iter 0 1 2 n)) 

; ```scheme
; (f-iter 5)
; (iter 0 1 2 5)                           ; a = f(0), b = f(1), c = f(2)
; (iter 1 2 (+ 2 (* 2 1) (* 3 0) (- 5 1)))
; (iter 1 2 4 4)                           ; a = f(1), b = f(2), c = f(3)
; (iter 2 4 (+ 4 (* 2 2) (* 3 1) (- 4 1)))
; (iter 2 4 11 3)                          ; a = f(2), b = f(3), c = f(4)
; (iter 4 11 (+ 11 (* 2 4) (* 3 2) (- 3 1)))
; (iter 4 11 25 2)                         ; a = f(3), b = f(4), c = f(5)
; ```