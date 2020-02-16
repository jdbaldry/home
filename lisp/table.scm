; Implementation of a table like data structure in Scheme.

; Construct a dictionary from a single key, value pair.
(define (make-dict key value)
    (list (cons key value)))

; Add a key, value pair to the dictionary.
; TODO: overwrite existing keys since you can keep appending the same keys and slowing down get-dict.
(define (put-dict dict key value)
    (cond ((null? dict) (list (cons key value)))
          (else (append dict (list (cons key value))))))

; Get the value for a key in the dictionary.
(define (get-dict dict key)
    (cond ((null? dict) "not found")
          ((equal? (caar dict) key) (cdar dict))
          (else (get-dict (cdr dict) key))))

(define dict (put-dict (make-dict "test" "success") "orange" "green"))