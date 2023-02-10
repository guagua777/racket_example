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

;exp ::= (Int int)
;exp ::= (Prim 'read ())
;exp ::= (Prim '- (exp)) ;; 这样也可以(Prim '- exp)， 
;exp ::= (Prim '+ (exp exp))
;exp ::= (Prim '- (exp exp))
;LInt ::= (Program '() exp)

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

;;parse-program This function takes an S-expression representation of an abstract
;;syntax tree and converts it into the struct-based representation.
;; 将具体语法转换为抽象语法
(define (parse-program p)
  (match p
    [`(program ,info ,body)
     (Program info (parse-exp body))]
    ))

;; read-program This function takes a file path and parses that file (it must be a
;; Racket program) into an abstract syntax tree
;
;> (read-program "/Users/xxxxxx/tests/int_test_3.rkt")
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

(is_Lint (Program '() ast1_1))
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

;assert This function takes two parameters, a string (msg) and Boolean (bool),
;and displays the message msg if the Boolean bool is false.
(define assert
  (lambda (msg b)
    (if (not b)
	(begin
	  (display "ERROR: ")
	  (display msg)
	  (newline))
	(void))))

;; August 25 VIDEO

;; 具体语法
;exp ::= int | (read) | (- exp) | (+ exp exp) | (- exp exp)
;R0 ::= exp

;; 抽象语法
;exp ::= (Int int)
;      | (Prim 'read '()) 
;      | (Prim '- (list exp))
;      | (Prim '+ (list exp exp))
;R0 ::= (Program '() exp)

(struct Int (value))
(struct Prim (op arg*))
(struct Read ())
(struct Add (left right))
(struct Neg (value))

(define E1 (Int 42))
(define E2 (Prim 'read '()))
(define E3 (Prim '- (list E1)))
(define E4 (Prim '+ (list E3 (Int 5))))
(define E5 (Prim '+ (list E2 (Prim '- (list E2)))))

(define (list-max ls)
  (foldl max 0 ls))

(define (height e)
  (match e
    [(Int n) 1]
    [(Prim op e*)
     (add1 (list-max (map height e*)))]
    ))

(height E1)
(height E2)
(height E3)
(height E4)
(height E5)

(let ([var exp]) exp)
(let ([x (+ 12 20)]) (+ 10 x))
(let ([x 32])
  (+ (let ([x 10])
       x)
     x))

(let ([x1 32])
  (+ (let ([x2 10])
       x2)
     x1))

(let ([x (read)])
  (let ([y (read)])
    (+ x (- y))))

;; Lvar具体语法
;type ::= Integer
;exp ::= int | (read) | (- exp) | (+ exp exp) | (- exp exp)
;exp ::= var | (let ([var exp]) exp)
;LVar ::= exp

;; Lvar抽象语法
;type ::= Integer
;exp ::= (Int int)
;      | (Prim 'read ())
;      | (Prim '- (exp))
;      | (Prim '+ (exp exp))
;      | (Prim '- (exp exp))
;exp ::= (Var var) | (Let var exp exp)
;LVar ::= (Program ’() exp)


;; 使用类的方式定义方法
(define ((interp_Lint env) e)
  (match e
    [(Prim '- (list e1))
     (fx- 0 ((interp_Lint env) e1))]
    ...))

;; interp_Lvar 中调用了 interp_Lint
(define ((interp_Lvar env) e)
  (match e
    [(Var x)
     (dict-ref env x)]
    [(Let x e body)
     (define v ((interp_Lvar env) e))
     (define env^ (dict-set env x v))
     ((interp_Lvar env^) body)]
    [else ((interp_Lint env) e)]))

;;不能覆盖 Lint中包含Lvar中语法的情况
(Let 'y (Int 10) (Prim '- (list (Var 'y))))

;; open recursion

;To make our interpreters extensible we need something called open recursion, in
;which the tying of the recursive knot is delayed until the functions are composed.
;Object-oriented languages provide open recursion via method overriding

(define interp-Lint-class
  (class object%
    (define/public ((interp_exp env) e)
      (match e
        [(Prim '- (list e))
         (fx- 0 ((interp_exp env) e))]
        ...))
    ...))

(define interp-Lvar-class
  (class interp-Lint-class
    (define/override ((interp_exp env) e)
      (match e
        [(Var x)
         (dict-ref env x)]
        [(Let x e body)
         (define v ((interp_exp env) e))
         (define env^ (dict-set env x v))
         ((interp_exp env^) body)]
        [else
         (super (interp_exp env) e)]))
    ...
    ))

(define e0 (Let 'y (Int 10) (Prim '- (list (Var 'y)))))
((send (new interp-Lvar-class) interp-exp '()) e0)

;;association list

(define ages
  '((jane . 25)
    (sam . 24)
    (kate . 45)))

(for/list ([(k v) (in-dict ages)])
  (cons k (add1 v)))

;We use the AT&T syntax expected
;by the GNU assembler. A program begins with a main label followed by a sequence of instructions.

;program counter

;;x86语法
;reg ::= rsp | rbp | rax | rbx | rcx | rdx | rsi | rdi | r8 | r9 | r10 | r11 | r12 | r13 | r14 | r15
;arg ::= $int | %reg | int(%reg)
;instr ::= addq arg,arg | subq arg,arg | negq arg | movq arg,arg | pushq arg | popq arg | callq label | retq | jmp label | label: instr
;x86Int ::= .globl main
;       main:
;            instr ...

;; 寄存器
;; 参数 ::= 整形字面量 | 寄存器中的值 | 栈中的值
;; 命令 ::=
;; 入口 ::=

;; integer constant (called an immediate value)

; 例子 (+ 10 32)
;   .globl main    ;; 入口
;main:             ;; 固定符
;   movq $10, %rax ;; 具体命令
;   addq $32, %rax
;   retq

;; retq Pops the return address and jumps to it. 跳转到返回地址
;; retq returns from a procedure to its caller 从当前函数返回到调用者
;; addq s, d
;; movq s, d
;; callq L Pushes the return address and jumps to label L. 存储当前的返回地址，并跳转到标签L
;; jmp L Jump to label L.

;; The last letter q indicates that these instructions operate on quadwords, which are 64-bit values

;; The register rsp is called the stack pointer
;; and contains the address of the item at the top of the stack

;; 要做什么：从callee返回到caller
;; 如何实现：callee中保存caller的地址
;; 返回地址后面紧跟跳转指令
;; In the context of a procedure call, the return address is the location of the instruction
;; that immediately follows the call instruction on the caller side

;; 在跳转之前，将返回地址存储到栈指针中
;; callq, pushes the return address onto the stack prior to jumping to the procedure

;; The base pointer of the caller is stored immediately after the return address

;; pushq A rsp – 8→rsp; A→ ∗rsp rsp减8, 保存caller的rbp到rsp中

;; The instruction pushq %rbp first subtracts 8 from the stack pointer rsp and then saves the base pointer of the caller at address rsp on the stack.
;; The next instruction movq %rsp, %rbp sets the base pointer to the current stack pointer, which is pointing to the location of the old base pointer.

;Position  |  Contents
;---------------------------
;8(%rbp)   |  return address
;0(%rbp)   |  old rbp
;–8(%rbp)  |  variable 1
;–16(%rbp) |  variable 2
;...       |  ...
;0(%rsp)   |  variable n

;start:
;      movq $10, -8(%rbp) ;; 保存字面量10到栈中
;      negq -8(%rbp)
;      movq -8(%rbp), %rax ;; 从栈到寄存器中
;      addq $52, %rax
;      jmp conclusion
;
;      
;      .globl main ;; 固定标识
;main:             ;; 固定标识
;      pushq %rbp  ;; 1. 将rsp减8，2. 保存caller的rbp到rsp指向的栈中
;      movq %rsp, %rbp ;; 将rsp中的值保存到rbp寄存器中，用于保存callee的rbp
;      subq $16, %rsp
;      jmp start
;
;      
;conclusion:
;      addq $16, %rsp
;      popq %rbp ;; 两个操作，1. 将栈中的值保存到rbp寄存器中，2. rsp-8
;      retq


;reg ::= rsp | rbp | rax | rbx | rcx | rdx | rsi | rdi | r8 | r9 | r10 | r11 | r12 | r13 | r14 | r15
;arg ::= (Imm int) | (Reg reg) | (Deref reg int)
;instr ::= (Instr addq (arg arg))
;        | (Instr subq (arg arg))
;        | (Instr negq (arg))
;        | (Instr movq (arg arg))
;        | (Instr pushq (arg))
;        | (Instr popq (arg))
;        | (Callq label int)
;        | (Retq)
;        | (Jmp label)
;block ::= (Block info (instr ...)) ;; 多了一个block
;x86Int ::= (X86Program info ((label . block)...))

;atomic expressions

;atm ::= int | var
;exp ::= atm | (read) | (- atm) | (+ atm atm) | (- atm atm)
;stmt ::= var = exp;
;tail ::= return exp; | stmt tail
;CVar ::= (label: tail) ...

;A sequence of statements always ends with Return, a guarantee that is baked into the grammar rules for tail.

;atm ::= (Int int) | (Var var)
;exp ::= atm | (Prim 'read ()) | (Prim '- (atm)) | (Prim '+ (atm atm)) | (Prim '- (atm atm))
;stmt ::= (Assign (Var var) exp)
;tail ::= (Return exp) | (Seq stmt tail)
;CVar ::= (CProgram info ((label . tail) ...))


(define (interp-Cvar p)
  (send (new (interp-Cvar-mixin interp-Lvar-class)) interp-program p))

;; 跟对应的bnf一致
(define (interp-Cvar-mixin super-class)
  (class super-class
    (super-new)
    (inherit interp-exp)

    (define/public (interp-stmt env)
      (lambda (s)
        (match s
          [(Assign (Var x) e)
           (dict-set env x ((interp-exp env) e))]
          [else
           ...])))

    (define/public (interp-tail env)
      (lambda (t)
        (match t
          [(Return e)
           ((interp-exp env) e)]
          [(Seq s t2)
           ;; 这个地方注意下
           (define new-env ((interp-stmt env) s))
           ((interp-tail new-env) t2)])))

    (define/override (interp-program p)
      (match p
        ...))))

(define (uniquify-exp env)
  (lambda (e)
    (match e
      [(Var x)
       (Var (dict-ref env x))]
      [(Int n)
       (Int n)]
      [(Let x e body)
       (let* ([new-x (gensym x)]
              [new-env (dict-set env x new-x)])
         (Let new-x ((uniquify-exp env) e) ((uniquify-exp new-env) body)))]
      [(Prim op es)
       (Prim op (for/list ([e es]) ((uniquify-exp env) e)))])))


;; 移除复杂操作数
;; 操作的参数都是atm
;atm ::= (Int int) | (Var var)
;exp ::= atm | (Prim 'read ()) | (Prim '- (atm)) | (Prim '+ (atm atm)) | (Prim '- (atm atm))| (Let var exp exp)
;LmonVar ::= (Program ’() exp)

(for/list ([i '(1 2 3)])
    (printf "~a \n" i))

(define-values (x y z) (values 1 2 3))

(append* '(a) '(b) '((c) (d)))

(begin
    (define-values (a b c)
    (for/lists (l1 l2 l3)
             ([i '(1 2 3)]
              [j "abc"]
              [k "ABC"])
    (values i j k)))
    (printf "~a ~a ~a \n" a b c))

(for/lists (l1 l2 l3)
             ([i '(1 2 3)]
              [j "abc"]
              #:when (odd? i) ;;when后面的为子循环
              [k #(#t #f)])
    (values i j k))


(let ([x 10])
   (let ([y (+ 42 (- 100))])
     (+ x y)))

(Program
 '()
 (Let 'x1229018 (Int 10)
  (Let 'y1229019 (Let 'tmp1229020 (Prim '- (list (Int 100)))
                      (Prim '+ (list (Int 42) (Var 'tmp1229020))))
       (Prim '+ (list (Var 'x1229018) (Var 'y1229019))))))

(let ([x1229018 10])
  (let ([y1229019 (let ([tmp1229020 (- 100)])
                    (+ 42 tmp1229020))])
    (+ x1229018 y1229019)))


(remove-complex-opera* 
   (Program
 '()
 (Let
  'x1330363
  (Int 10)
  (Let
   'y1330364
   (Prim '+ (list (Int 42) (Prim '- (list (Int 100)))))
   (Prim '+ (list (Var 'x1330363) (Var 'y1330364)))))))
(Program
 '()
 (Let
  'x1330363
  (Int 10)
  (Let
   'y1330364
   (Let
    'tmp1346057
    (Prim '- (list (Int 100)))
    (Prim '+ (list (Int 42) (Var 'tmp1346057))))
   (Prim
    '+
    (list (Var 'x1330363) (Var 'y1330364))))))


(rco-atom (Prim '- (list (Int 100))))
(Var 'tmp1477744)
(list (cons 'tmp1477744 (Prim '- (list (Int 100)))))
(rco-atom (Prim '+ (list (Int 42) (Int 100))))
(Var 'tmp1493694)
(list (cons 'tmp1493694 (Prim '+ (list (Int 42) (Int 100)))))


(define (rco-exp e)
  (match e
    [(Var x) (Var x)]
    [(Int n) (Int n)]
    [(Let x rhs body)
     (Let x (rco-exp rhs) (roc-exp body))]
    [(Prim op es)
     (define-values (new-es sss)
       (for/lists (l1 l2) ([e es]) (rco-atom e)))
     ;;变量和表达式的对应关系
     (make-lets (append* sss) (Prim op new-es))]))

(define make-lets
  (lambda (var-exp e)
    (match var-exp
      [`() e]
      [`((,x . ,e^) . ,var-exp^)
       (Let x e^ (make-lets (var-exp^ e)))])))

(define roc-atom
  (lambda (e)
    (match e
      [(Var x) (Var x)]
      [(Int n) (Int n)]
      [(Let x rhs body)
       (define new-rhs (rco-exp rhs))
       (define-values (new-body body-ss) (rco-atom body))
       (values new-body (append `((,x . ,new-rhs)) body-ss))]
      [(Prim op es)
       (define-values (new-es sss)
         (for/lists (l1 l2) ([e es]) (roc-atom e)))
       (define ss (append* sss))
       (define tmp (gensym 'tmp))
       (values (Var tmp)
               (append ss `((,tmp . ,(Prim op new-es)))))])))

;; the notion of tail position
;1. In (Program () e), expression e is in tail position.
;2. If (Let x e1 e2) is in tail position, then so is e2.


;;移除复杂操作后
42
(Program '() (Int 42))

(+ 20 22)
(Program '() (Prim '+ (list (Int 20) (Int 22))))

(let ([x 41]) (+ x 1))
(Program '() (Let 'x2203899 (Int 41) (Prim '+ (list (Var 'x2203899) (Int 1)))))

;x = 41;
;tmp = (+ x 1);
;return tmp;


(let ([x 10])
   (let ([x (+ x 1)])
     x))
(Program
 '()
 (Let
  'x2203900
  (Int 10)
  (Let 'x2203901 (Prim '+ (list (Var 'x2203900) (Int 1))) (Var 'x2203901))))


(let ([x 10])
   (let ([y (+ 42 (- 100))])
     (+ x y)))
(Program
 '()
 (Let
  'x2203902
  (Int 10)
  (Let
   'y2203903
   (Let
    'tmp2203904
    (Prim '- (list (Int 100)))
    (Prim '+ (list (Int 42) (Var 'tmp2203904))))
   (Prim '+ (list (Var 'x2203902) (Var 'y2203903))))))

;; https://git.fracta.dev/enrico/eoc/src/branch/master/explicate-control.rkt

(program ((locals . (x.1 x.2 y)))
         ((start .
                 (seq (assign x.1 20)
                      (seq (assign x.2 22)
                           (seq (assign y (+ x.1 x.2))
                                (return y)))))))

;; 实际语法
(let ([x (let ([x 4])
            (+ x 1))])
    (+ x 2))

;; 抽象语法
(Program '() (Let 'x (Let 'x (Int 4)
                          (Prim '+ (list (Var 'x) (Int 1))))
                  (Prim '+ (list (Var 'x) (Int 2)))))

(define (uniquify-exp env)
  (lambda (e)
    (match e
      ;; prim中会有var
      ;; 将(Var x) 变成(Var x123)
      [(Var x) (Var (dict-ref env x))]
      [(Int n) (Int n)]
      ;; 变成(Let x345 ....)
      [(Let x e body)
       (define new-sym (gensym x))
       (define new-env (dict-set env x new-sym))
       (Let new-sym ((uniquify-exp env) e) ((uniquify-exp new-env) body))]
      [(Prim op es)
       (Prim op (for/list ([e es]) ((uniquify-exp env) e)))])))

(define (uniqufy p)
  (match p
    [(Program info e)
     (Program info ((uniquify-exp '()) e))]))

(define (remove-comlex-opera* p)
  (match p
    [(Program info e)
     (Program info (rco-exp e))]))


(define (rco-exp e)
  (match e
    [(Var x) (Var x)]
    [(Int n) (Int n)]
    [(Let x rhs body)
     (Let x (rco-exp rhs) (rco-exp body))]
    [(Prim op es)
     (define-values (new-es

                     ....))]))

(let ([x (+ 42 (- 10))])
  (+ x (+ 5 6)))

(let ([x (let ([tmp.1 (- 10)])
           (+ 42 tmp.1))])
  (let [(tmp.2 (+ 5 6))]
    (+ x tmp.2)))

;; 返回的是什么？
;; 以(+ 5 6)为例
(define (rco-atom e)
  (match e
    [(Var x) (values (Var x) '())]
    [(Int n) (values (Int n) '())]
    [(Let x rhs body)
     (define new-rhs (rco-exp rhs))
     (define-values (new-body body-ss) (rco-atom body))
     (values new-body (append `((,x . ,new-rhs)) body-ss))]
    [(Prim op es)
     (define-values (new-es sss)
       (for/lists (l1 l2)([e es]) (rco-atom e)))
     ;;组成一个list
     (define ss (append* sss))
     (define tmp (gensym 'tmp))
     ;; 返回 临时变量 变量和表达式的对应关系
     ;; 内部和外部反过来来了
     ;; (values (Var tmp.1) ((tmp.1 . (Prim + 5 6))))
     (values (Var tmp)
             (append ss `((,tmp . ,(Prim op new-es)))))]))

(define (rco-exp e)
  (match e
    [(Var x) (Var x)]
    [(Int n) (Int n)]
    [(Let x rhs body)
     (Let x (rco-exp rhs) (rco-exp body))]
    [(Prim op es)
     (define-values (new-es sss)
       (for/list (l1 l2) ([e es]) (rco-atom e)))
     (make-lets (append* sss) (Prim op new-es))]))

(define (make-lets bs e)
  (match bs
    [`() e]
    [`((,x . ,e^) . ,bs^)
     (Let x e^ (make-lets bs^ e))]))

(Program '() (Let 'x3575137 (Int 10)
                  (Let 'y3575138 (Let 'tmp3575139 (Prim '- (list (Int 100)))
                                      (Prim '+ (list (Int 42) (Var 'tmp3575139))))
                       (Prim '+ (list (Var 'x3575137) (Var 'y3575138))))))


(Block '() (list (Instr 'movq (list (Imm 42) (Reg 'rax)))
                 (Jmp 'conclusion)))

(define (remove-complex-op* p)
  (match p
    [(Program info e)
     (Program info (rco-exp e))]))

(define (rco-exp e)
  (match e
    [(Var x) (Var x)]
    [(Int n) (Int n)]
    [(Let x rhs body)
     ;; 将rhs和body中的complex变成let
     (Let x (rco-exp rhs) (rco-exp body))]
    [(Prim op es) ;; es为e的复数
     (define-values (atom-es var-exps)
       (for/lists (l1 l2) ([e^ es]) (rco-atom e^)))
     ;; 这个地方不可以再gensym了
     ;;(make-lets (append (append* var-exps) (Prim op atom-es)))]
     (make-lets (append* var-exps) (Prim op atom-es))]))

(define (rco-atom e)
  (match e
    ;; atom转换之后的表达式，与临时变量和对应的值
    [(Var x) (values (Var x) '())]
    [(Int n) (values (Int n) '())]
    [(Let x rhs body)
     (define new-rhs (rco-exp rhs))
     (define-values (new-body temp-exps) (rco-atom body))
     ;; 不是返回Let了
     (values new-body (append `((,x . ,new-rhs)) temp-exps))]
    [(Prim op es)
     (define-values (atom-exps var-exps)
       (for/lists (l1 l2) ([e^ es]) (rco-atom e^)))
     ;; (values (op atom-exps) (append* var-exps))]))
     ;; 需要将该op，变成临时变量
     (define tmp (gensym 'tmp))
     (values (Var tmp) ;;转换后的表达式
             (append (append* var-exps) `((,tmp . ,(Prim op atom-exps)))))]))

(define (make-lets bs e)
  (match bs
    [`() e]
    [`((,x . ,e^) . bs^)
     (Let x e^ (make-lets bs^ e))]))

(define (explicate-control p)
  (match p
    [(Program info body)
     (define-values (body-tail body-vars) (explicate-tail body))
     (CProgram info `((start . ,body-tail)))]))

(define (explicate-tail exp)
  (match exp
    [(Var x) (values (Return (Var x)) '())]
    [(Int n) (values (Return (Int n)) '())]
    [(Let x rhs body)
     ;; 需要对x进行赋值
     (define-values (body-tail body-vars) (explicate-tail body))
     ;;(define-values (rhs-tail rhs-vars) (explicate-tail rhs))
     ;;(define new-tail (explicate-assign x rhs-tail body-tail))
     ;;(values (new-tail (append rhs-vars body-vars)))]
     (define-values (x-tail rhs-vars) (explicate-assign x rhs body-tail))
     (values x-tail (cons x (append rhs-vars body-vars)))]
    [(Prim op es)
     (values (Return (Prim op es)) '())]))

;(define (explicate-assign v rhs c)
;  (match rhs
;    [(Var x)
;     (values (Seq (Assign (Var v) rhs) c))]
;    [(Int n)
;     (values (Seq (Assign (Var v) rhs) c))]
;    [(Let x rhs1 body)
;     ;;(define-values (rhs1-tail rhs1-vars) (explicate-tail rhs1))
;     ;;(define-values (body-tail body-vars) (explicate-tail body))
;     ;;(explicate-assign x body-tail rhs1-tail)]
;    [(Prim op es)
;     (values (Seq (Assign (Var v) (Prim op es)) c))]))

(define (explicate-assign v rhs c)
  (match rhs
    [(Var x)
     (values (Seq (Assign (Var v) rhs)) '())]
    [(Int n)
     (values (Seq (Assign (Var v) rhs)) '())]
    [(Let x rhs1 body)
     ;; 需要将body赋值给v
     (define-values (v-tail body-vars) (explicate-assign v body c))
     (define-values (x-tail rhs1-vars) (explicate-assign x rhs1 v-tail))
     ;;(values x-tail (append rhs1-vars body-vars))
     (values x-tail (cons x (append rhs1-vars body-vars)))]
    [(Prim op es)
     (values (Seq (Assign (Var v) (Prim op es))) '())]))

(define (select-instr p)
  (match p
    [(CProgram info (list (cons 'start t)))
     (X86Program info (list (cons 'start (Block '() (select-instr-tail t)))))]))

(define (select-instr-tail t)
  (match t
    [(Seq stmt t*)
     (append (select-instr-stmt stmt) (select-instr-tail t*))]
    ;; read单独处理
    [(Return (Prim 'read '()))
     (list (Callq 'read_int)
           (Jmp 'conclusion))]
    [(Return e)
     (append
      (select-instr-assign (Reg 'rax) e)
      (list (Jmp 'conclusion)))]))

(define (select-instr-stmt stmt)
  ;;Cvar中Assign为 (Assign (Var v) exp)
  (match stmt
    [(Assign (Var v) (Prim '+ (list (Var v1) a2)))
     #:when (equal? v v1)
     (list (Instr 'addq (list (select-instr-atm a2) (Var v))))]
    [(Assign (Var v) (Prim '+ (list a1 (Var v2))))
     #:when (equal? v v2)
     (list (Instr 'addq (list (select-instr-atm a1) (Var v))))]
    [(Assign v e)
     (select-instr-assign v e)]))

(define (select-instr-atm a)
  (match a
    [(Int i) (Imm i)]
    [(Var _) a]))

;;(Assign (Var v) exp)
(define (select-instr-assign v e)
  (match e
    [(Int i)
     (list (Instr 'movq (list (select-instr-atm e) v)))]
    [(Var _)
     (list (Instr 'movq (list (select-instr-atm e) v)))]
    [(Prim 'read '())
     (list (Callq 'read_int)
           (Instr 'movq (list (Reg 'rax) v)))]
    [(Prim '- (list a))
     (list (Instr 'movq (list (select-instr-atm a) v))
           (Instr 'negq (list v)))]
    [(Prim '+ (list a1 a2))
     (list (Instr 'movq (list (select-instr-atm a1) v))
           (Instr 'addq (list (select-instr-atm a2) v)))]))

;; info 为 (cons (cons 'locals let-binds) info)
(define (assign-homes p)
  (match p
    [(X86Program info (list (cons 'start es)))
     (define vars (cdr (car info)))
     (X86Program (list (cons 'stack-space (calc-stack-space vars)))
                 (list (cons 'start (assign-homes-block es (car info)))))]))

(define (calc-stack-space ls)
  (* 8 (length ls)))

(define (assign-homes-block b ls)
  (match b
    [(Block info es)
     (Block info (for/lists ([e es]) (assign-homes-instr e ls)))]))

(define (assign-homes-instr i ls)
  (match i
    [(Instr op (list e1))
     (Instr op (list (assign-homes-imm e1 ls)))]
    [(Instr op (list e1 e2))
     (Instr op (list (assign-homes-imm e1 ls) (assign-homes-imm e2 ls)))]
    [else i]))

(define (assign-homes-imm i ls)
  (match i
    [(Reg reg) (Reg reg)]
    [(Imm int) (Imm int)]
    [(Var v)
     (Deref 'rbp (* -8 (find-index v (cdr ls))))]))

;; 跟explicate-tail中let分支中rhs-vars 和 body-vars的顺序相关
(define (find-index v ls)
  (cond
    [(eq? v (car ls)) 1]
    [else
     (add1 (find-index v (cdr ls)))]))


(X86Program
 '((stack-space . 16))
 (list
  (cons
   'start
   (Block
    '()
    (list
     (Instr 'movq (list (Imm 42) (Deref 'rbp -8)))
     ;; patch instruction
     (Instr 'movq (list (Deref 'rbp -8) (Deref 'rbp -16)))
     (Instr 'movq (list (Deref 'rbp -16) (Reg 'rax)))
     (Jmp 'conclusion))))))

(define (patch-instructions p)
  (match p
    [(X86Program info B-list)
     (X86Program info (map
                       (lambda (x)
                         `(,(car x) . ,(patch-block (cdr x))))
                       B-list))]))

(define (patch-block b)
  (match b
    [(Block '() instrs)
     (Block '() (append-map patch-instr instrs))]))

(define (patch-instr i)
  (match i
    [(Instr op (list (Deref reg off) (Deref reg2 off2)))
     (list (Instr 'movq (list (Deref ref off) (Reg 'rax)))
           (Instr op (list (Reg 'rax) (Deref reg2 off2))))]
    [else (list i)]))

(let ([v 1])
  (let ([w 42])
    (let ([x (+ v 7)])
      (let ([y x])
        (let ([z (+ x w)])
          (+ z (- y)))))))

;locals-types:
;    x : Integer, y : Integer,
;    z : Integer, t : Integer,
;    v : Integer, w : Integer
;start:
;    movq $1, v
;    movq $42, w
;    movq v, x
;    addq $7, x
;    movq x, y
;    movq x, z ;; 完成该movq后，x不再被使用。且z只有在此之后被使用
;    addq w, z
;    movq y, t
;    negq t
;    movq z, %rax
;    addq t, %rax
;    jmp conclusion

;; 图着色
;; 无向图
;; 顶点代表变量
;; 边代表冲突

;; interfere

;; the interference graph
;; The writes performed by an instruction must not overwrite something in a live location

;; With the addition of the register allocator,
;; the callee-saved registers used by the register allocator must be saved in the prelude and restored in the conclusion.

;; In the prelude of the main function,
;; we push rbx onto the stack because it is a callee-saved register and it was assigned to a variable by the register allocator.

(X86Program
 ;; locals 与 local-types
 '((locals a166528 b166529)
   (locals-types (b166529 . Integer) (a166528 . Integer)))
 (list
  (cons
   'start
   (Block
    '()
    (list
     (Instr 'movq (list (Imm 42) (Var 'a166528)))
     (Instr 'movq (list (Var 'a166528) (Var 'b166529)))
     (Instr 'movq (list (Var 'b166529) (Reg 'rax)))
     (Jmp 'conclusion))))))

;locals-types:
;    x: Integer,
;    y: Integer,
;    z: Integer,
;    t: Integer,
;    v: Integer,
;    w: Integer
;start:
;    movq $1, v
;    movq $42, w
;    movq v, x ;;重点
;    addq $7, x
;    movq x, y
;    movq x, z ;;重点
;    addq w, z
;    movq y, t
;    negq t
;    movq z, %rax ;; 重点
;    addq t, %rax ;; 重点
;    jmp conclusion

;; In particular, the caller is responsible for freeing some registers prior to the function call for use by the callee.

;; The prelude subtracts 8 bytes from the rsp to make it 16-byte aligned

;; https://stackoverflow.com/questions/9268586/what-are-callee-and-caller-saved-registers#:~:text=Caller%2Dsaved%20registers%20(AKA%20volatile,not%20be%20preserved%20across%20calls.
;; https://stackoverflow.com/questions/9268586/what-are-callee-and-caller-saved-registers

;; caller view: 需要callee做的内容
;; callee view: 需要caller做的内容

;; a variable is live at a program point
;; if the current value in the variable might be used later in the program

;; interference can use a two-dimensional array data sturctue

(define (build-interference-block ast G)
  (match ast
    [(Block info ss)
     (define lives (dict-ref info 'lives))
     (define live-afters (cdr lives))
     (define new-ss
       (for/list ([inst ss] [live-after live-afters])
         ((build-interference-instr live-after G) instr)))
     (define new-info (dict-remove info 'lives))
     (Block new-info new-ss)]
    [else (error "error ast " ast)]))

(define (build-interference-instr live-after G)
  (lambda (ast)
    (match ast
      [(Instr 'movq (list s d))
       (for ([v live-after])
         (for ([d^ (free-vars d)])
           (cond
             [(equal? (Var v) s)
              (void)]
             [(equal? v d)
              (void)]
             [else
              (add-edge! G d v)])))
       ast]
      [(Callq f args*)
       (for ([v live-after])
         (for ([u caller-save-for-alloc])
           (cond
             ;; 同一个顶点
             [(equal? v u)
              (void)]
             [else
              ;; caller-save的寄存器可以随意分配，为什么还要加边？？
              ;; 应该是callee-save的吧？
              (add-edge! G u v)])))
       ast]
      [else
       (for ([v live-after])
         (for ([d (write-vars ast)])
           (cond
             [(equal? v d)
              (void)]
             [else
              (add-edge! G d v)])))
       ast])))
       
(define (free-vars arg)
  (match arg
    [(Var x) (set x)]
    [(Reg r) (set r)]
    [(Deref r i) (set r)]
    [(Imm n) (set)]
    [else (error "free-vars error" arg)]))

(define (build-interference-block ast G)
  (match ast
    [(Block info ss)
     (define lives (dict-ref info 'lives))
     (define live-afters (cdr lives))
     (define new-ss
       (for/list ([inst ss] [live-after live-afters])
         ((build-interference-instr live-after G) inst)))
     (define new-info (dict-remove info 'lives))
     (Block new-info new-ss)]
    [else (error "ast is error" ast)]))


;; 创建move图
(define (build-move-block ast MG)
  (match ast
    [(Block info ss)
     (define new-ss
       (if use-move-biasing
           (let ([nss (for/list ([inst ss])
                        ((build-move-graph-instr MG) inst))])
             nss)
           ss))
     (Block info new-ss)]))

(define (build-move-graph-instr G)
  (lambda (ast)
    (match ast
      [(Instr 'movq (list (Var s) (Var d)))
       (if use-move-biasing
           (add-edge! G s d)
           '())
       ast]
      [else ast])))


(define (allocate-registers ast)
  (match ast
    [(X86Program info Blocks)
     (define locals (dict-ref info 'locals))
     (define IG (dict-ref info 'conflicts))
     (define MG (dict-ref info 'move-graph))
     (define-values (color num-spills) (color-graph IG MG info))

     ;; 根据颜色分配寄存器或者是栈
     (define homes
       (for/hash ([x locals])
         ;; 获取分配的结果
         (define home (identify-home (num-used-callee locals color)
                                     (hash-ref color x)))
         ;; 返回对应关系
         (values x home)))
     ...]))

;;
;; unavail-colors
;; a -> {1, 2, 3}
;; b -> {2, -1}
;;
;; Q
;; Node(a) -> Node(b) -> Node(c) -> ...
;;
;; pq-node
;; a -> Node(a)
;; b -> Node(b)
;; c -> Node(c)
;;
;; color
;; a -> 1
;; b-> 2
;; c-> -8%(rbp)
;;

(define (color-graph IG MG info)
  (define locals (dict-ref info 'locals))
  (define num-spills (init-num-spills))
  (define unavail-colors (make-hash))
  (define (compare u v)
    (>= (set-count (hash-ref unavail-colors u))
        (set-count (hash-ref unavail-colors v))))
  (define Q (make-pqueue compare))
  (define pq-node (make-hash))
  (define color (make-hash))

  ;; 初始化，将已经存在的寄存器分配颜色
  (for ([x locals])
    (define adj-reg
      ;; 过滤出其中的寄存器
      ;; 过滤出x的邻接结点中的寄存器
      (filter (lambda (u) (set-member? registers u))
              (get-neighbors IG x)))
    ;; 对x的邻接寄存器染色
    (define adj-reg-colors-list (map register->color adj-reg))
    (define adj-colors (list->set adj-reg-colors-list))
    ;; 标注x的饱和度
    (hash-set! unavail-colors x adj-colors)
    ;; 将x放入队列中
    (define x-node (pqueue-push! Q x))
    (hash-set! pq-node x x-node))

  ;; 染色
  (while (> (pqueue-count Q) 0)
         ;; 取出最大饱和度的顶点
         (define v (pqueue-pop! Q))
         
         ;;找出v的move-relation中已经染色的顶点
         (define neighbors-move (get-neighbors MG v))
         (define neighbors-move-colors (map (lambda (k) (hash-ref color k -1))
                                            neighbors-move))
         (define colored-move (filter (lambda (x) (>= x 0))
                                      neighbors-move-colors))
         (define move-related (sort colored-move <))

         ;; 染色
         (define c (choose-color v (hash-ref unavail-colors v) move-related info))
         (set! num-spills (update-num-spills num-spills c))
         (hash-set! color v c)

         ;; 将该颜色添加到邻接结点的饱和度中
         (for ([u (in-neighbors IG v)])
           ;; 邻接顶点不是寄存器
           (when (not (set-member? registers u))
             (define u-saturations (dict-ref unavail-colors u))
             (define new-u-saturations (set-add u-saturations c))
             (hash-set! unavail-colors u new-u-saturations)
             ;;更新队列
             (pqueue-decrease-key! Q (hash-ref pq-node u)))))
  (values color num-spills))
  
(define (register->color r)
  (cond
    [(assq r (reg-colors))
     => (lambda (p) (cdr p))]
    [else -1]))

(define (choose-color v unavail-colors move-related info)
  (define n (num-registers-for-alloc))
  ;; 从move中选取颜色
  (define biased-selection
    (for/first ([c move-related]
                #:when (valid-color c v unavail-colors info))
      c))
    
  ;; 从最小选择颜色
  (define unbiased-selection
    (for/first ([c (in-naturals)]
                #:when (valid-color c v unavail-colors info))
      c))
  (cond
    [(and biased-selection
          ;; biased-selection要小于unbiased-selection
          (or (< biased-selection n) (>= unbiased-selection n)))
     biased-selection]
    [else unbiased-selection]))

(define (valid-color c v unavail-colors info)
  (not (set-member? unavail-colors c)))

;; 队列的实现


;;==============================
;; interp-Lint

(define interp-Lint-class
  (class object%
    (super-new)

    (define/public (interp-exp env)
      (lambda (e)
        (match e
          [(Int n) n]
          ...
          )))

    (define/public (interp-program p)
      (match p
        [(Program '() e)
         ...]))))

(define interp-Lvar-class
  (class interp-Lint-class
    (super-new)

    (define/override (interp-exp env)
      (lambda (e)
        (match e
          [(Var x) ...]
          [(Let x e body)
           ...]
          ;; 重点
          [else ((super interp-exp env) e)])))))

(define (interp-Lvar p)
  (send (new interp-Lvar-class) interp-program p))

(define interp-Lif-class
  (class interp-Lvar-class
    (super-new)

    (define/public (interp-op op)
      (match op
        ['+ fx+]
        ['- ...]
        ['read ...]
        ['not
         ;; 返回一个函数
         (lambda (v)
           (match v
             [#t #f]
             [#f #t]))]
        ['eq? ...]))

    (define/override ((interp-exp env) e)
      ;; 返回一个输入为e的函数
      ;; 先定义一个函数，然后使用该函数
      (define recur (interp-exp env))
      
      (match e
        [(Bool b) b]
        [...]
        [(Prim op args)
         (apply (interp-op op) (for/list ([e args]) (recur e)))]
        [else ((super interp-exp env) e)]))))

(define (interp-if p)
  (send (new interp-Lif-class) interp-program p))

;; =-===================
;; type check

(define type-check-Lvar-class
  (class object%
    (super-new)

    (define/public (operator-types)
      ;; 想想为什么不是((Integer . Integer) . Integer)
      ;; > (list 2 3)
      ;; '(2 3)
      ;; > (cons 2 3)
      ;; '(2 . 3)
      '((+ . ((Integer Integer) . Integer))
        (- . ((Integer Integer) . Integer))
        (read . (() . Integer))))

    (define/public (type-equal? t1 t2)
      (equal? t1 t2))

    (define/public (type-check-exp env)
      (lambda (e)
        (match e
          [(Var x) ;;(values ((Var x) (type-check-exp env x)))]
           (values ((Var x) (dict-ref env x)))]
          [(Int n) (values (Int n) 'Integer)]
          [(Let x e body)
           (define-values (e^ Te)
             ((type-check-exp env) e))
           (define type-new-env (dict-set env x Te))
           (define-values (b Tb)
             ((type-check-exp type-new-env) body))
           (values (Let x e^ b) Tb)]
          [(Prim op es)
           (define-values (new-es ts)
             (for/lists (exprs types) ([e^ es])
               ((type-check-exp env) e^)))
           (define op-type (type-check-op op ts e))
           (values (Prim op new-es) op-type)]
          [else (error "")])))))

(define type-check-Lif-class
  (class type-check-Lvar-class
    (super-new)

    (inherit check-type-equal?)

    (define/override (type-check-exp env)
      (lambda (e)
        (match e
          [...]
          ...)))))

;; cmpq: x<y cmp y, x
;; result is in EFLAGS
;; setcc d 取出EFLAGS中的值放入d
;; cc为：condition code cc

;(if (eq? x y)
;    ...)
;cmpq y, x
;sete A
;movzbq A, B
; Jcc Label
;

(let ([x (read)])
  (let ([y (read)])
    (if (if (< x 1) (eq? x 0) (eq? x 2))
        (+ y 2)
        (+ y 10))))

(let ([x (read)])
  (let ([y (read)])
    (if (< x 1)
        (if (eq? x 0)
            (+ y 2)
            (+ y 10))
        (if (eq? x 2)
            (+ y 2)
            (+ y 10)))))

(define (create_block tail)
  (match tail
    [(Goto label) (Goto label)]
    [else
     (let ([label (gensym 'block)])
       (set! basic-blocks (cons (cons label tail) basic-blocks))
       (Goto label))]))

(define (create_block tail)
  (match tail
    [(Goto label) (Goto lable)]
    [else
     (let ([label (gensym 'block)])
       (set! basic-blocks (cons
                           (cons lable tail)
                           basic-blocks))
       (Goto label))]))

(define (explicate_pred cnd thn els)
  (match cnd
    [(Var x) ___]
    [(Let x rhs body) ___]
    [(Prim 'not (list e)) ___]
    [(Prim op es) #:when (or (eq? op 'eq?) (eq? op '<))
                  (IfStmt (Prim op es) (create_block thn)
                          (create_block els))]
    [(Bool b) (if b thn els)]
    [(If cnd^ thn^ els^) ___]
    [else (error "explicate_pred unhandled case" cnd)]))

(define ...








    
           

    




  
