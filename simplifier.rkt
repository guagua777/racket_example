#lang racket

(define recur
  (lambda (simp)
    ;; 返回一个接受form的函数
    ;; 与macro里面的转化为一个接受form的函数
    ;; form是同一个
    (lambda (form)

      (define loop
        (lambda (reduced-form)
          (let ([final-form (simp reduced-form)])
            (if (equal? reduced-form final-form)
                final-form
                

      
