; Implementation of a table like data structure in Scheme.

; Construct a dictionary.
(define (make-dict)
    (cons "dict" ()))

; Predicate checking for dict type.
(define (is-dict? object)
    (equal? "dict" (car object)))

; Add a key, value pair to the dictionary.
; TODO: overwrite existing keys since you can keep appending the same keys and slowing down get-dict.
(define (dict/put! dict key value)
    ; TODO: signal an actual error.
    (if (is-dict? dict)
            (cons "dict" (append (cdr dict) (list (cons key value))))
            "wrong type"))

; Get the value for a key in the dictionary.
(define (dict/get dict key)
    ; TODO: signal an actual error.
    (if (is-dict? dict)
            (cond ((null? (cdr dict)) "not found")
                ((equal? (caadr dict) key) (cdadr dict))
                (else (dict/get (cons "dict" (cddr dict)) key)))
            "wrong type"))

(define dict (dict/put! (dict/put! (make-dict) "test" "success") "orange" "green"))
; scheme --batch-mode --load table.scm --eval '(write-line (dict/get dict "test"))' --eval '(exit)'
; "success"
; scheme --batch-mode --load table.scm --eval '(write-line (dict/get dict "orange"))' --eval '(exit)'
; "green"