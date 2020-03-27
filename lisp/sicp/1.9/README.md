# 1.9

Each of the following two procedures defines a method for adding two positive integers in terms of the procedures `inc`, which increments its argument by 1, and `dec`, which decrements its argument by 1.

```scheme
(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))
```

## Answer

`inc`:

```scheme
(+ 4 5)
(if (= 4 0) 5 (inc (+ (dec 4) 5))) ; special form if statement is evaluated.
(if #f 5 (inc (+ (dec 4) 5))) ; since #f, alternate is evaluated.
(inc (+ (dec 4) 5))
(inc (+ 3 5))
(inc (if (= 3 0) 5 (inc (+ (dec 3) 5))))
(inc (if #f 5 (inc (+ (dec 3) 5))))
(inc (inc (+ (dec 3) 5)))
(inc (inc (+ 2 5)))
(inc (inc (if (= 2 0) 5 (inc (+ (dec 2) 5)))))
(inc (inc (if #f 5 (inc (+ (dec 2) 5)))))
(inc (inc (inc (+ (dec 2) 5))))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (if (= 1 0) 5 (inc (+ (dec 1) 5))))))
(inc (inc (inc (if #f 5 (inc (+ (dec 1) 5))))))
(inc (inc (inc (inc (+ (dec 1) 5)))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc (if (= 0 0) 5 (inc (+ (dec 0) 5)))))))
(inc (inc (inc (inc (if #t 5 (inc (+ (dec 0) 5)))))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9
```

`dec`:

```scheme
(+ 4 5)
(if (= 4 0) 5 (+ (dec 4) (inc 5))) ; special form if statement is evaluated.
(if #f 5 (+ (dec 4) (inc 5))) ; since #f, alternate is evaluated.
(+ (dec 4) (inc 5))
(+ 3 6)
(if (= 3 0) 6 (+ (dec 3) (inc 6)))
(if #f 6 (+ (dec 3) (inc 6)))
(+ (dec 3) (inc 6))
(+ 2 7)
(if (= 2 0) 7 (+ (dec 2) (inc 7)))
(if #f 7 (+ (dec 2) (inc 7)))
(+ (dec 2) (inc 7))
(+ 1 8)
(if (= 1 0) 8 (+ (dec 1) (inc 8)))
(if #f 8 (+ (dec 1) (inc 8)))
(+ (dec 1) (inc 8))
(+ 0 9)
(if (= 0 0) 9 (+ (dec 0) (inc 9)))
(if #t 9 (+ (dec 0) (inc 9)))
9
```

`inc` is a recursive procedure as can be seen by its shape. `dec` is an iterative procedure.
