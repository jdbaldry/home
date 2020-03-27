# 1.4

Observe that our model of evaluation allows for combinations whose operators are compound expressions. Use this observation to describe the behavior of the following procedure:

```scheme
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
```

The `if` statement returns either the `-` or `+` operator which is then used as the operator applied to the operands `a` and `b`.
