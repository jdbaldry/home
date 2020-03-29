; Procedures for performing unit tests in R5RS Scheme.

(define (fail test-name want got)
  (display "; Test failed: ")
  (display test-name)
  (newline)
  (display "; Want: ")
  (display want)
  (newline)
  (display "; Got : ")
  (display got)
  (newline))

; assert-equal compares two values and displays an test error message if they do not match.
(define (assert-equal test-name want got)
  (if (not(= want got))
      (fail test-name want got)))

; assert-equal-values traverses two lists comparing the values.
(define (assert-equal-values test-name want got)
  (cond ((not (= (length want) (length got)))
          (display "; Test failed: ")
          (display test-name)
          (newline)
          (display "; Lists must be of equal length")
          (newline)
          (display "; Want: ")
          (display want)
          (newline)
          (display "; Got : ")
          (display got)
          (newline))
        ((and (null? want ) (null? got)) #t)
        ((not (= (car want) (car got))) (fail test-name want got))
        (else (assert-equal-values test-name (cdr want) (cdr got)))))
              