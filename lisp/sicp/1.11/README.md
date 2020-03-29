# 1.11

A function f is defined by the rule that f(n) = n if n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n> 3. Write a procedure that computes f by means of a recursive process. Write a procedure that computes f by means of an iterative process.

```
f(1) = 1
f(2) = 2
f(3) = f(2) + 2f(1) + 3f(0)
     = 2 + 2
     = 4
f(4) = f(3) + 2f(2) + 3f(1)
     = 4 + 4 + 3
     = 11
```
