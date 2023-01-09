#lang racket

;; https://www.it.uu.se/edu/course/homepage/avfunpro/ht13/lectures/Racket-3-Macros.pdf

;(define or
;  (lambda (x y)
;    (if x
;        x
;        y)))

;(or 3 (/ 2 0))

;;==========
;(define-syntax-rule (or x y)
;  (if x
;      x
;      y))
;
;(or 3 (/ 2 0))
;(or (begin (display 3) 3) (/ 2 0))

;;==============
;(define-syntax-rule (or x y)
;  (let ([z x])
;    (if z
;        z
;        y)))
;
;(or 3 (/ 2 0))
;(or (begin (display 3) 3) (/ 2 0))
;
;(define-syntax-rule (swap! x y)
;  (let ([tmp x])
;    (set! x y)
;    (set! y tmp)))

;;===============
;(define-syntax or
;  (syntax-rules ()
;    [(or) #f]
;    [(or x) x]
;    [(or x y)
;     (let ([z x])
;       (if z
;           z
;           y))]
;    [(or x y ...)
;     (or x (or y ...))]))

(define-syntax or
  (syntax-rules ()
    [(_) #f]
    [(_ x) x]
    [(_ x y)
     (let ([z x])
       (if z
           z
           y))]
    [(_ x y ...)
     (or x (or y ...))]))

;; syntax-rules: defines several rules by pattern matching

(or 3 (/ 2 0))
(or (begin (display 3) 3) (/ 2 0))

(define-syntax while
  (syntax-rules (do)
    [(_ cond do body ...)
     (let loop () ;;()为初始化参数
       (when cond
         body ...
         (loop)))]))

(define test-while
  (lambda (x)
    (while (> x 0) do
           (displayln x)
           (set! x (- x 1)))))

;; 永远无法展开完成，死循环
;(define-syntax while-wrong
;  (syntax-rules (do)
;    [(_ cond do body ...)
;     (when cond
;       body ...
;       (while-wrong cond do body ...))]))
;
;(define test-while-wrong
;  (lambda (x)
;    (while-wrong (> x 0) do
;                 (displayln x)
;                 (set! x (- x 1)))))

;The macro are expanded before runtime.
;In this case, the expansion never completes.

(define fib
  (lambda (x)
    (case x
      [(0 1) 1]
      [else (+ (fib (- x 1))
               (fib (- x 2)))])))

(define-syntax-rule (define-memo (f args ...) body ...)
  ;;转换之后的样子
  ;;The macro are expanded before runtime
  ;; 展开之后的样子
  (define f
    (let ([memo (make-hash)])
      (lambda (args ...)
        (cond
          [(hash-has-key? memo (list args ...))
           (hash-ref memo (list args ...))]
          [else
           (let ([res (begin body ...)])
             (hash-set! memo (list args ...) res)
             res)])))))

(define-memo (fib/m x)
  ;(lambda (x)
    (display x)
    (case x
      [(0 1) 1]
      [else (+ (fib/m (- x 1))
               (fib/m (- x 2)))]))


(define-syntax define-m
  (syntax-rules ()
    [(_ f
        (lambda (args ...)
          body ...))
     (define f
       (let ([memo (make-hash)])
         (lambda (args ...)
           (cond
             [(hash-has-key? memo (list args ...))
              (hash-ref memo (list args ...))]
             [else
              (let ([res (begin body ...)])
                (hash-set! memo (list args ...) res)
                res)]))))]))

(define-m fib/m2
  (lambda (x)
    (display x)
    (case x
      [(0 1) 1]
      [else (+ (fib/m2 (- x 1))
               (fib/m2 (- x 2)))])))


;(define s
;  (simplifier
;   [(+ ,x 0) -> ,x]
;   [(* ,x 1) -> ,x]
;   [(+ ,x ,x) -> (* ,x 2)]))


;; 没有 ->
;(lambda (form)
;  (match form
;    ((+ ,x 0) ,x) ((* ,x 1) ,x) ((+ ,x ,x) (* ,x 2)) (else form)))

;; -> is part of the macro definition, not a variable.
(define-syntax simplifier1
  (syntax-rules (->)
    [(_ (x -> y) ...) ;; (x -> y)代表多个规则
     ;; 展开后的样子
     ;; 返回一个函数，form为函数的参数
;     (begin
;     (printf " lambda is ~a ~n " '(lambda (form)
;                         (match form
;                           [x y]
;                           ...
;                           [else form])))             
     (lambda (form)
       ;(printf "=== ~a ~n" form)
       (match form
         ;; 合在一起构成`(+ ,x 0)
         ;; 可参考match (quasiquote qp)
         ;; 匹配x，值是y，匹配`(+ ,x 0)，值为,x
         [`x `y] ;; [x^ y^]为什么就不行，['x 'y]也不可以，因为x和y中会有,
         ...
         [else form]))
     ;)
     ]))

;(define s
;  (simplifier [(+ ,x 0) -> ,x] [(* ,x 1) -> ,x] [(+ ,x ,x) -> (* ,x 2)]))

(define s1
  (simplifier1
   [(+ ,x 0) -> ,x]
   [(* ,x 1) -> ,x]
   [(+ ,x ,x) -> (* ,x 2)]))

(s1 '(+ 10 0))
;(s '(+ (+ y 0) (* y 1)))

; match quasi quote
;
;> (match '(1 2 3)
;    [`(1 ,a ,(? odd? b))
;     (list a b)])
;
;
;'(2 3)
;> (match '(1 2 4)
;    [`(1 ,a ,(? odd? b))
;     (list a b)])
; match: no matching clause for '(1 2 4)
;

;; recursive
;; until fix-point


;; recur接收一个函数，返回一个函数
;(define recur
;  (lambda (simplify)
;    (lambda (form)
;      (let loop ([reduced-form (cond
;                                 [(list? form)
;                                  ;; 对list中每个元素进行化简
;                                  (map (recur simplify) form)]
;                                 [else form])])
;        (let ([final-form (simplify reduced-form)])
;          (if (equal? final-form reduced-form)
;              final-form
;              (loop final-form)))))))

;;返回一个递归的simplify
(define recur
  (lambda (simplify)
    (lambda (form)
      
      (define loop
        (lambda (reduced-form)
          (let ([final-form (simplify reduced-form)])
            (if (equal? final-form reduced-form)
                final-form
                (loop final-form)))))

      (let ([reduced-form (cond
                            [(list? form)
                             ;(printf "form is ~a ~n" form)
                             (map (recur simplify) form)]
                             ;(map simplify form)]
                            [else form])])
        (loop reduced-form)))))
          
(define-syntax simplifier
  (syntax-rules (->)
    [(_ (x -> y) ...)
     ;;展开为一个递归的化简
     (recur (lambda (form)
              (match form
                [`x
                 `y]
                ...
                [else form])))]))

(define s
  (simplifier
   [(+ ,x 0) -> ,x]
   [(* ,x 1) -> ,x]
   [(+ ,x ,x) -> (* ,x 2)]))

(s '(+ (+ y 0) (* y 1)))
(s '(+ (+ (+ y 0) 0) (* y 1)))


