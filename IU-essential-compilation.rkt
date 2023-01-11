#lang racket

;; https://github.com/IUCompilerCourse
;; https://wphomes.soic.indiana.edu/jsiek/

;; 类名+字段
(struct Int (value))
(define eight (Int 8))
(struct Prim (op args))
(define neg-eight (Prim '- (list eight)))
(define rd (Prim 'read '()))
(define ast1_1 (Prim '+ (list rd neg-eight)))
(struct Program (info body)) ;; info is '()

;(struct Read ())
;(struct Add (left right))
;(struct Neg (value))

(require racket/fixnum)
(fx+ 10)
;(fx+ 'a)
;(fx+ "asdfas")
(fx+ 10 12 22)

exp ::= (Int int)
exp ::= (Prim 'read ())
exp ::= (Prim '- (exp)) ;; 这样也可以(Prim '- exp)， 
exp ::= (Prim '+ (exp exp))
exp ::= (Prim '- (exp exp))
LInt ::= (Program '() exp)

(Int 8)
(Prim '- ((Int 8)))
(Prim '+ ((Prim 'read ()) (Prim '- ((Int 8)))))

(Program '() (Prim '+ (list (Var 'b) (Var 'a))))

(define src-primitives
  '(read + -))

(define (parse-exp e)
  (match e
    [(? symbol?) (Var e)]
    [(? fixnum?) (Int e)]
    [(? boolean?) (Bool e)]
    [`(void) (Void)]
    [`(let ([,x ,rhs]) ,body)
     (Let x (parse-exp rhs) (parse-exp body))]
    [`(if ,cnd ,thn ,els)
     (If (parse-exp cnd) (parse-exp thn) (parse-exp els))]
    [`(project ,e ,t)
     (Project (parse-exp e) t)]
    [`(,op ,es ...)
     #:when (set-member? src-primitives op)
     (Prim op (for/list ([e es]) (parse-exp e)))]
    ))

(define (parse-program p)
  (match p
    [`(program ,info ,body)
     (Program info (parse-exp body))]
    ))

;> (read-program "/Users/zhoujun/Documents/cs/iu-compilation/code/course-compiler/tests/int_test_3.rkt")
;(Program
; '()
; (Prim
;  '-
;  (list (Prim '+ (list (Prim 'read '()) (Prim '- (list (Int 5))))))))
;>

(define E1 (Int 42)) 
(define E2 (Prim 'read '()))  
(define E3 (Prim '- (list E1)))
(define E4 (Prim '+ (list E3 (Int 5)))) 

;> (pe-Lint (Program '() E4))
;(Program '() (Int -37))


;type ::= Integer
;exp ::= int | (read) | (- exp) | (+ exp exp) | (- exp exp)
;LInt ::= exp

;type ::= Integer
;exp ::= (Int int) | (Prim 'read ())
;| (Prim '- (exp)) | (Prim '+ (exp exp)) | (Prim '- (exp exp))
;LInt ::= (Program ’() exp)


(match ast1_1
  [(Prim op (list child1 child2))
   (print op)])

(define (leaf arith)
  (match arith
    [(Int n) #t]
    [(Prim 'read '()) #t]
    [(Prim '- (list e1)) #f]
    [(Prim '+ (list e1 e2)) #f]
    [(Prim '- (list e1 e2)) #f]))

(leaf (Prim 'read '()))
(leaf (Prim '- (list (Int 8))))
(leaf (Int 8))

(define (is_exp ast)
  (match ast
    [(Int n) #t]
    [(Prim 'read '()) #t]
    [(Prim '- (list e)) (is_exp e)]
    [(Prim '+ (list e1 e2))
     (and (is_exp e1) (is_exp e2))]
    [(Prim '- (list e1 e2))
     (and (is_exp e1) (is_exp e2))]
    [else #f]))

(define (is_Lint ast)
  (match ast
    [(Program '() e) (is_exp e)]
    [else #f]))

;;(define ast1_1 (Prim '+ (list rd neg-eight)))

(is_Lint (Program '() ast1_1)
(is_Lint (Program '()
                  (Prim '* (list (Prim 'read '())
                                 (Prim '+ (list (Int 8)))))))

(interp-Lint (Program '() (Prim '+ (list (Int 10) (Int 32)))))

(interp-Lint (Program '() ast1_1))

(define (interp-exp e)
  (match e
    [(Int n) n]
    [(Prim 'read '())
     (define r (read))
     (cond
       [(fixnum? r) r]
       [else
        (error 'interp-exp' "expecte an int" r)])]
    [(Prim '- (list e))
     (define v (interp-exp e))
     (fx- 0 v)]
  ...))

(define (interp-Lint p)
  (match p
    [(Program '() e)
     (interp-exp e)]))


(define pe-neg
  (lambda (r)
    (match r
      [(Int n)
       (Int (fx- 0 n))]
      [else
       (Prim '- (list r))])))

(define pe-add
  (lambda (r)
    ...))

(define pe-exp
  (lambda (e)
    (match e
      ...)))

(define pe-Lint
  (lambda (p)
    ...))

;; partial evaluation
(define (test-pe p)
  (assert "testing pe-Lint"
          (equal? (interp-Lint p) (interp-Lint (pe-Lint p)))))
(test-pe (parse-program `(program () (+ 10 (- (+ 5 3))))))
(test-pe (parse-program `(program () (+ 1 (+ 3 1)))))
(test-pe (parse-program `(program () (- (+ 3 (- 5))))))


