#lang racket
#lang racket

;(define-values (l1 l2 l3)
;  (for/lists (l1 l2 l3)
;             ([i '(1 2 3)]
;              [j "abc"]
;              ;#:when (odd? i)
;              [k #(#t #f #t)])
;    (values i j k)))
;
;l3
;
;(append* l3)

(define-values (l1 l2 l3)
  (for/lists (l1 l2 l3)
             ([i '(1 2 3)]
              [j "abc"]
              ;#:when (odd? i)
              [k '((10) (20) (30))])
    (values i j k)))

l3

(append* l3)
