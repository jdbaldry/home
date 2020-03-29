(load "../unit-test.scm")
(load "pascals-triangle.scm")
;   k  0  1  2  3  4  5  6
; n
; 0    1
; 1    1  1
; 2    1  2  1
; 3    1  3  3  1
; 4    1  4  6  4  1
; 5    1  5 10  10 5  1
; 6    1  6 15  20 15 6  1


(assert-equal "n=0, k=0 should return 1" 1 (pt-element 0 0))
(assert-equal "n=1, k=1 should return 1" 1 (pt-element 1 1))
(assert-equal "n=2, k=1 should return 2" 2 (pt-element 2 1))
(assert-equal "n=6, k=3 should return 20" 20 (pt-element 6 3))
(assert-equal-values "n=1 should return (1 1)" (list 1 1) (pt 1))
(assert-equal-values "n=2 should return (1 2 1)" (list 1 2 1) (pt 2))
(assert-equal-values "n=3 should return (1 3 3 1)" (list 1 3 3 1) (pt 3))
(assert-equal-values "n=4 should return (1 4 6 4 1)" (list 1 4 6 4 1) (pt 4))