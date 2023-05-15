#lang racket
(for/lists (firsts seconds)
             ([pr '((1 . 2) (3 . 4) (5 . 6))])
  ;; 第一个元素构成一个list，第二个元素构成一个list
    (values (car pr) (cdr pr)))

(append* '((c) (d)))