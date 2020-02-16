; Implementation of a table like data structure in Scheme.

; Construct a dictionary.
(define (make-dict)
    (cons "dict" ()))

; Add a key, value pair to the dictionary.
; TODO: overwrite existing keys since you can keep appending the same keys and slowing down get-dict.
(define (put-dict dict key value)
    (cond ((null? (cdr dict)) (list (cons key value)))
          (else (append (cdr dict) (list (cons key value))))))

; Get the value for a key in the dictionary.
(define (get-dict dict key)
    (cond ((null? dict) "not found")
          ((equal? (caar dict) key) (cdar dict))
          (else (get-dict (cdr dict) key))))

(define dict (put-dict (put-dict (make-dict) "orange" "green") "test" "success"))