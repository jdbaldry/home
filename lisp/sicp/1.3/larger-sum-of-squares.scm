(define (sq x) (* x x))
(define (larger-sum-of-squares a b c)
  (if (> a b)
    (if (> b c)
      (+ (sq a) (sq b))
      (+ (sq a) (sq c))
    )
    (if (> a c)
      (+ (sq a) (sq b))
      (+ (sq b) (sq c)))))