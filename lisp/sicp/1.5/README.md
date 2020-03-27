# 1.5

Ben Bitdiddle has invented a test to determine whether the interpreter he is faced with is using applicative-order evaluation or normal-order evaluation. He defines the following two procedures:

```scheme
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))
```

Then he evaluates the expression

```scheme
(test 0 (p))
```

What behavior will Ben observe with an interpreter that uses applicative-order evaluation? What behavior will he observe with an interpreter that uses normal-order evaluation? Explain your answer. (Assume that the evaluation rule for the special form if is the same whether the interpreter is using normal or applicative order: The predicate expression is evaluated first, and the result determines whether to evaluate the consequent or the alternative expression.)

# Answer

With **applicative-order** evaluation arguments are evaluated and then applied.
Ben's procedure will be caught in an evaluation loop, endlessly evaluating `(p)`. The evaluation would look as follows:

```scheme
(test 0 (p))
(if (= x 0) 0 (p)) ; evaluating (p) returns (p)
(if (= x 0) 0 (p)) ; evaluating (p) returns (p)
(if (= x 0) 0 (p)) ; evaluating (p) returns (p)
...
```

With **normal-order** evaluation expressions are fully evaluated and then reduced. Ben will see the value `0` returned. The evaluation would look as follows:

```scheme
(test 0 (p))
(if (= x 0) 0 (p))
(if #t 0 (p))
0
```
