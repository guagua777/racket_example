#lang racket


; car is equal to v 
(assoc 3 (list (list 1 2) (list 3 4) (list 5 6)))
(assoc 3 '((1 2) (3 4) (5 6)))


;(assv v lst)

(assv 3 '((1 2) (3 4) (5 6)))

;; assq