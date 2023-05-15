#lang racket

;; (class superclass-expr class-clause ...)

(define book-class%
  (class object%
    (field (pages 5))
    (define/public (letters)
      (* pages 500))
    (super-new)))

;(define 类名
;  (class 父类
;    (类成员1)
;    (类成员2)
;    ;; 创建父类对象
;    ;; super-new are bound to forms to initialize fields in the superclass (see Creating Objects);
;    (super-new)))


(define (describe obj)
  (printf "Hello ~a\n" obj))

(define table%
  (class object%
    (define/public (describe-self)
      (describe this))
    (super-new)))

(send (new table%) describe-self)


(define account%
  (class object%
    (super-new)
    (init-field balance)
    (define/public (add n)
      (new this% [balance (+ n balance)]))))
(define savings%
  (class account%
    (super-new)
    (inherit-field balance)
    (define interest 0.04)
    (define/public (add-interest)
      (send this add (* interest balance)))))

(define num
  5)
(define identity
  'a)
(define func
  (lambda (x y)
    (+ x y)))
(define clazz%
  (class object% ;; father-class
    (super-new)
    (define/public class-func
      (lambda (x y)
        (+ x y)))))
;; (send (new clazz%) class-func 7 8)



