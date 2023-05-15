#lang racket



;(for/fold ([accum-id init-expr] ... maybe-result) (for-clause ...)
;  body-or-break ... body)
; 
;maybe-result = | #:result result-expr

;; 本质上还是for，只是给for循环加了参数
;; accum-id 为参数名

(for/fold ([sum 0] ;; accum-id init-expr 初始值
           [rev-roots null])
          ([i '(1 2 3 4)]) ;; for-clause 循环集合
  ;(values (+ sum i) (cons (sqrt i) rev-roots))) ;; body
  (values (+ sum i) (cons (* i i) rev-roots))) ;; body 循环体


(define for-fold-pro
  (lambda (sum)
    (for ([i '(1 2 3 4)])
      (set! sum (+ sum i)))
    sum))

(define for-fold-pro2
  (lambda (sum rev-roots)
    (for ([i '(1 2 3 4)])
      (set! sum (+ i sum))
      (set! rev-roots (cons i rev-roots)))
    (values sum rev-roots)))
    

(for-fold-pro2 0 '())

(for-fold-pro 10)

(set 'a)


(for/list ([x (in-list '(3 1 4))])
    `(,x ,(* x x)))

(for/list ([x '(3 1 4)])
    `(,x ,(* x x)))

(for/lists (l1 l2 l3)
             ([i '(1 2 3)]
              [j "abc"]
              #:when (odd? i)
              [k #(#t #f)])
    (values i j k))

(define-values (a b c)
  (for/lists (l1 l2 l3)
             ([i '(1 2 3)]
              [j "abc"]
              #:when (odd? i)
              [k #(#t #f)])
    (values i j k)))
a
b
c




