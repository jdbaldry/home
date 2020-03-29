# 1.12

The following pattern of numbers is called Pascal's triangle.

```
     1
    1 1
   1 2 1
  1 3 3 1
 1 4 6 4 1
...
```

The numbers at the edge of the triangle are all 1, and each number inside the triangle is the sum of the two numbers above it. 1Write a procedure that computes elements of Pascal's triangle by means of a recursive process.

## Answer

From [Wikipedia: Pascal's Triangle](https://en.wikipedia.org/wiki/Pascal%27s_triangle):

> The entry in the nth row and kth column of Pascal's triangle is denoted (n k). For example, the unique nonzero entry in the topmost row is (0 0) = 1. With this notation, the construction of Pascal's Triangle is:

```
( n )   (n - 1)   (n - 1)
      =         +
( k )   (k - 1)   (  k  )
```

From [Wikipedia: Binomial Coefficient](https://en.wikipedia.org/wiki/Binomial_coefficient):

It is the coefficient of the x^k term in the polynomial expansion of the binomial power (1 + x)^n, and it is given by the formula:

```
( n )       n!
      =  --------
( k )    k!(n-k)!
```
