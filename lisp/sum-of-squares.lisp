#!/usr/bin/env clisp
(defun sum-of-squares 
    (x y)
    (defun square
        (x) 
        (* x x))
    (+
        (square x) 
        (square y)))

(print 
    (sum-of-squares 10 10))
(print 
    (sum-of-squares 3 4))