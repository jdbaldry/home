(load "../unit-test.scm")
(load "f.scm")

(assert-equal 1 '(f 1))
(assert-equal 2 '(f 2))
(assert-equal 4 '(f 3))
(assert-equal 11 '(f 4))
(assert-equal 25 '(f 5))
(assert-equal 59 '(f 6))


(assert-equal 1 '(f-iter 1))
(assert-equal 2 '(f-iter 2))
(assert-equal 4 '(f-iter 3))
(assert-equal 11 '(f-iter 4))
(assert-equal 25 '(f-iter 5))
(assert-equal 59 '(f-iter 6))

