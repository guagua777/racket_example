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







                  


    


















  









