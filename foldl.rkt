#lang racket

;(foldl proc init lst ...+) 


;; (cons 1 '()) => '(1)
;; (cons 2 '(1)) => '(1 2)
(foldl cons '() '(1 2 3 4))


;; (+ 1 0)
;; (+ 2 1)
(foldl + 0 '(1 2 3 4))