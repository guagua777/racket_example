#lang racket

(define h #hash((a . "apple") (b . "banana")))
(for/list ([(k v) (in-dict h)])
    (format "~a = ~s" k v))