; Implementation of a table like data structure in Scheme.

; Construct a dictionary.
(define (make-dict)
    (cons "dict" ()))

; Add a key, value pair to the dictionary.
; TODO: overwrite existing keys since you can keep appending the same keys and slowing down get-dict.
(define (put-dict dict key value)
    ; TODO: signal an actual error.
    (if (is-dict? dict)
            (cons "dict" (append (cdr dict) (list (cons key value))))
            "wrong type"))

; Get the value for a key in the dictionary.
(define (get-dict dict key)
    ; TODO: signal an actual error.
    (if (is-dict? dict)
            (cond ((null? (cdr dict)) "not found")
                ((equal? (caadr dict) key) (cdadr dict))
                (else (get-dict (cddr dict) key)))
            "wrong type"))

(define dict (put-dict (put-dict (make-dict) "orange" "green") "test" "success"))