#lang racket


(define a (box 5))
(box? a)
a


(set-box! a 10)
a

(unbox a)


(split-at '(1 2 3 4 5 6) 2)	