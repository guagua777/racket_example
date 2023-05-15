#lang racket

;The primary purpose of both the while loop and set! is
;to cause side effects, so they do not give a meaningful result value.

;The (Begin es body) expression evaluates the subexpressions es
;for their effects and then evaluates and returns the result from body.

(let ([sum 0])
  (let ([i 5])
    ;; begin里面包含while和sum
    ;; begin的最后一个exp为body
    (begin
      ;; while的result为void
      (while (> i 0)
             (begin
               (set! sum (+ sum i))
               (set! i (- i 1))))
      sum)))


(let ([x2 10])
  (let ([y3 0])
    (+ (+ (begin
            (set! y3 (read))
            (get! x2))
          (begin
            (set! x2 (read))
            (get! y3)))
       (get! x2))))