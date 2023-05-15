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

;; explicate_control中的cont是continuation

(define (explicate_pred cnd thn els)
  (match cnd
    [(Var x) ...]
    ...))

;; multi-graph

(define (create_block tail)
  (delay
    (define t (force tail))
    (match t
      [(Goto label) (Goto label)]
      [else
       (let ([label (gensym 'block)])
         (set! basic-blocks (cons (cons label t) basic-blocks))
         (Goto label))])))

(define (create_block tail)
  (delay
    (define t (force tail))
    (match t
      [(Goto label) (Goto label)]
      [else
       (let ([label (gensym 'block)])
         (set! basic-blocks (cons (cons label t) basic-blocks))
         (Goto label))])))

(define (transpose g)
  (define g^T (multigraph (make-hash)))
  (for ([u (in-vertices g)])
    (add-vertex! g^T u))
  (for ...))

;; 课堂笔记
;; what is the context of E2 and E3?

;; explicate-assign
(let ([x (if e1 e2 e3)])
  ...)

;; 重点
;; 自己手动推导一遍
;(explicate-tail
;  (let ([x (read)])
;    (let ([y (read)])
;      (if (if (< x 1) (eq? x 0)  (eq? x 2))
;          (+ y 2)
;          (+ y 10)))))
;  (explicate-tail (let ([y (read)]) ...)) => B1
;     (explicate-tail (if ... (+ y 2) (+ y 10))) => B2
;        (explicate-tail (+ y 2)) => B3 = return (+ y 2);
;        (explicate-tail (+ y 10)) => B4 = return (+ y 10);
;        (explicate-pred (if (< x 1) (eq? x 0) (eq? x 2) B3 B4)) => B2
;           put B3 and B4 in the CFG, get labels l3 l4.
;           (explicate-pred (eq? x 0) (goto l3) (goto l4)) => B6
;              B6 = if (eq? x 0)
;                     goto l3;
;                   else
;                     goto l4;
;           (explicate-pred (eq? x 2) (goto l3) (goto l4)) => B7
;              B7 = if (eq? x 2)
;                     goto l3;
;                   else
;                     goto l4;
;           (explicate-pred (< x 1) B6 B7) => B2
;              put B6 and B7 into CFG, get labels l6, l7.
;              B2 = if (< x 1)
;                     goto l6;
;                   else
;                     goto l7;
;     (explicate-assign (read) y B2) => (y = (read); B2) = B1
;  (explicate-assign (read) x B1) => x = (read); B1

;tail ::= ...
;      | goto label
;      | if (cmp atm atm)
;          goto label;
;        else
;          goto label;
;C1 ::= label1:
;         tail1
;       label2:
;         tail2
;       ...

;;; explicate-pred
;(explicate-pred #t B1 B2) => B1
;(explicate-pred #f B1 B2) => B2
;(explicate-pred (< atm1 atm2) B1 B2)
;  => if (< atm1 atm2)
;       goto l1;
;     else
;       goto l2;
;B1 B2 in CFG with l1 and l2;
;
;(explicate-pred (if e1 e2 e3) B1 B2) => B5
;    (explicate-pred e2 (goto l1) (goto l2)) => B3
;    (explicate-pred e3 (goto l1) (goto l2)) => B4
;    (explicate-pred e1 B3 B4) => B5
;
;;; explicate-tail
;(explicate-tail (if e1 e2 e3)) => B3
;    where (explicate-tail e2) => B1
;          (explicate-tail e3) => B2
;          (explicate-tail e1 B1 B2) => B3
;
;;; explicate-assign
;(explicate-assign (Int n) x B1)
;    => (Seq (Assign (Var x) (Int n))
;            B1)
;(explicate-assign (if e1 e2 e3) x B1) => B4
;    where we add B1 to the CFG with label l1
;    (explicate-assign e2 x (goto l1)) => B2
;    (explicate-assign e3 x (goto l1)) => B3
;    (explicate-pred e1 B2 B3) => B4
;
;;; example
;(explicate-tail
; (let ([x (read)])
;   (let ([y (read)])
;     (if (if (< x 1) (eq? x 0) (eq? x 2))
;         (+ y 2)
;         (+ y 10)))))
;
;(explicate-tail (let ([y (read)]) ...)) => B1
;  (explicate-tail (if ... (+ y 2) (+ y 10))) => B2
;    (explicate-tail (+ y 2)) => B3 = return (+ y 2);
;    (explicate-tail (+ y 10)) => B4 = return (+ y 10);
;    (explicate-pred (if (< x 1) (eq? x 0) (eq? x 2) B3 B4)) => B2 ;; 参考explicate-pred中对应的分支
;        put B3 and B4 in the CFG, get labels l3 l4
;        (explicate-pred (eq? x 0) (goto l3) (goto l4)) => B6
;        (explicate-pred (eq? x 2) (goto l3) (goto l4)) => B7
;        (explicate-pred (< x 1) B6 B7) => B2 
;    (explicate-assign (read) y B2) => (y = (read); B2) = B1
;(explicate-assign (read) x B1) => x = (read); B1


;atm ::= int | var
;exp ::= atm | (read) | (- atm) | (+ atm atm) | (- atm atm)
;stmt ::= var = exp;
;tail ::= return exp: | stmt tail
;atm ::= bool
;cmp ::= eq? | < | <= | > | >=
;exp ::= (not atm) | (cmp atm atm)
;tail ::= goto label;
;      | if (cmp atm atm) goto label; else goto label;
;Cif ::= (label: tail) ...

;; Cif ::= (CProgram info ((label . tail) ... ))

;; IF的recall中liveness部分

(define (uncover-live ast)
  (match ast
    [(X86Program info blocks)
     (X86Program info (uncover-live-CFG blocks))]))

(define (uncover-live-CFG cfg)
  (define G (CFG->graph cfg))
  (define CFG-hash (make-hash))
  (define reverse-G (transpose G))
  ;; 遍历所有的label
  (for ([label (tsort reverse-G)])
    ;; 该label的所有的 邻接点
    ;; 'start -> ... 正向的
    (define neighbor-labels (in-neighbors G label))
    ;; (for ([lbl neighbor-labels]) ...)
    ;; 该Block最后一条指令的live-after
    ;; 等于后面指令的live-before
    (define live-after (for/fold ([lives (set)])
                                 ([lbl neighbor-labels])
                         (set-union lives (live-before lbl CFG-hash))))
    ;; 分析该block的liveness
    (define new-block (uncover-live-block (dict-ref cfg label) live-after))
    (hash-set! CFG-hash label new-block))
  (hash->list CFG-hash))

(define uncover-live-block
  (lambda (ast live-after)
    (match ast
      [(Block info ss)
       (define lives ((uncover-live-stmt live-after) ss))
       (define new-info (dict-set info 'lives lives))
       (Block new-info ss)])))

(define (uncover-live-stmts orig-live-after)
  (lambda (orig-ss)
    
    (define loop
      (lambda (ss live-after lives)
        (cond
          [(null? ss) lives]
          [else
           (define new-live-after ((uncover-live-instr live-after) (car ss)))
           (loop (cdr ss) new-live-after (cons new-live-after lives))])))

    (loop (reverse orig-ss) orig-live-after (list orig-live-after))))


(define (uncover-live-instr live-after)
  (lambda (stmt)
    (set-union (set-subtract live-after (write-vars stms))
               (read-vars stmt))))

(define (write-vars instr)
  (match instr
    [(Jmp label) (set)]
    [(JmpIf cc label) (set)]
    [(Callq f num) (set)]
    [(Instr 'movq (list s d))
     (free-vars d)]
    [(Instr 'movzbq (list s d))
     (free-var d)]
    [(Instr name arg*)
     (if (eq? name 'set)
         (set)
         (apply set-union (for/list ([arg arg*])
                            (free-vars arg))))]
    [else (error "xxxx" instr)]))

(define (read-vars instr)
  (match instr
    ...))

(define (free-vars arg)
  (match arg
    [(Var x) (set x)]
    ...))

  
;; cfg-hash是什么？
;; 在Block的info中添加了'live后的Block
;; 获取某个block的live-before
(define live-before
  (lambda (label CFG-hash)
   (match (hash-ref CFG-hash label)
     [(Block info ss)
      ;; 想一想为什么要有car，因为要取第一条指令之前的liveness
      (car (dict-ref info 'live))])))
    
(define CFG->graph
  (lambda (cfg)
    ;; 创建有向图
    (define G (directed-graph '()))
    ;; 添加顶点
    (for ([label (in-dict-keys cfg)])
      (add-vertex! G label))
    ;; 添加边
    (for ([s b] (in-dict cfg))
      (for ([t (adjacent-instrs b)])
        (add-directed-edge! G s t)))
    ;; 返回图
    G))

;; 关联的指令
(define adjacent-instrs
  (lambda (b)
    (match b
      ;; ss为instructs list
      [(Block info ss)
       (for ([outs (set)]([s ss])
         (set-union outs (adjacent-instr s))))])))

;; 构造图的重点
(define adjacent-instr
  (lambda (s)
    (match s
      [(Jmp label)
       ;; 排除掉conclusion
       (cond
         [(string-suffix? (symbol->string label) "conclusion")
          (set)]
         [else
          (set label)])]
      [(JmpIf cc label)
       (set label)]
      [else (set)])))

;; The primary purpose of both the while loop and set! is
;; to cause side effects, so they do not give a meaningful result value.

(define (remove-jumps p)
  (match p
    [(X86Program info blocks)
     (define r-cfg (dict-ref info 'r-cfg))
     (define vertices-order (tsort (transpose r-cfg)))
     (define new-blocks '())
     (define removed-blocks (mutable-set))

     (for ([vert vertices-order])
       (if (not (set-member? removed-blocks vert))
           (let* ([instrs (Block-instr* (dict-ref blocks vert))]
                  [block-info (Block-info (dict-ref blocks vert))]
                  [new-instrs (...)]))))]))


(define fix-block
  (lambda (instrs cfg removed-blocks all-blocks curr-blcok)
    (cond
      [(null? instrs) '()]
      [else
       (let ([instr (car instrs)])
         (match instr
           [(Jmp target)
            #:when (and (not (equal? target 'conclusion))
                        (equal? (length (get-neighbors cfg target)) 1))
            (begin
              (set-add! removed-blcoks target)
              ...)]))])))


;;https://iucompilercourse.github.io/IU-Fall-2022/

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

             
;; interp Lwhile
;; type check Lwhile

;[(Prim op args)
;         (apply (interp-op op) (for/list ([e args]) (recur e)))]

(define (interp-exp env)
  (lambda (e)
    (define recur (interp-exp env))
    (define result
      (match e
        [(Let x e body)
         (define new-env (dict-set env x (box (recur e))))
         ((interp-exp new-env) body)]
        [(Var x) (unbox (dict-ref env x))]
        [(GetBang x) (unbox (dict-ref env x))]
        [(SetBang x rhs)
         (set-box! (dict-ref env x) (recur rhs))]
        [(WhileLoop cnd body)
         (define (loop)
           (cond
             [(recur cnd) ;;#t
              (recur body)
              (loop)]
             [else
              ;;while的返回值为void
              (void)]))
         (loop)]
        [(Begin es body)
         (for ([e es])
           (recur e))
         (recur body)]
        [(Void)
         (void)]
        [else ((super ...) e)]))

    result))

;; while type
(define type-equal?
  (lambda (t1 t2)
    ;; match* 匹配多个values
    (match* (t1 t2)
      [('_ t2) #t]
      [(t1 '_) #t]
      [(other wise)
       (equal? t1 t2)
       ;(equal? other wise)
       ])))

(define (type-check-exp env)
  (lambda (e)

    (define recur (type-check-exp env))

    (match e
      [(SetBang x rhs)
       (define-values (rhs^ rhsT) (recur rhs))
       (define varT (dict-ref env x))
       (check-type-equal? rhsT varT e)
       (values (SetBang x rhs^) 'Void)]
      [...])))

;;keleene fix point algorithm

(define analyze-dataflow
  ;; G is backward
  (lambda (G transfer bottom join)
    ;; 存放label与liveness的对应关系
    (define mapping (make-hash))
    (for ([v (in-vertices G)]) (dict-set! mapping v bottom))
    ;; 存放改变的顶点
    (define worklist (make-queue))
    (for ([v (in-vertices G)]) (enqueue! worklist v))

    ;; trans-G is forward
    (defien trans-G (transpose G))
    (while (not (queue-empty? worklist))
           (define node (dequeue! worklist))
           (define input
             (for/fold ([state bottom]) ([pred (in-neighbors trans-G node)])
               (join state (dict-ref mapping pred))))
           (define output (transfer node input))
           (cond
             [(not (equal? output (dict-ref mapping node)))
              (dict-set! mapping node output)
              (for ([v (in-neighbors G node)])
                (enqueue! worklist v))]))
    mapping))


(for/lists (a b) ([e (list 1 2 3 4)])
    (values (+ e 1) (* e e)))

(define/override (type-equal? t1 t2)
  (match* (t1 t2)
    [(`(Vector ,ts1 ...) `(Vector ,ts2 ...))
     (for/and ([t1 ts1] [t2 ts2])
       (type-equal? t1 t2))]
    [(other wise)
     (super ...)]))

(define/override (type-check-exp env)
  (lambda (e)
    (define recur (type-check-exp env))
    (match e
      [(Prim 'vector es)
       (unless (<= (length es) 50)
         (error ""))
       ;; 重点
       (define-values (e* t*)
         (for/lists (e* t*) ([e es]) (recur e)))
       (define t `(Vector ,@t*))
       (values (HasType (Prim 'vector e*) t)
               t)]

      [(Prim 'vector-ref (list e1 (Int i)))
       (define-values (e1^ t) (recur e1))
       (match t
         [`(Vector ,ts ...)
          (unless (and (0 . <= . i) (i . < . (length ts)))
            (error ""))
          (values (Prim 'vector-ref (list e1^ (Int i)))
                  (list-ref ts i))]
         [else
          (error "")])]

      [(Prim 'vector-set! (list e1 (Int i) arg))
       (define-values (e1^ t-vec) (recur e1))
       (define-values (e-arg^ t-arg) (recur arg))
       (match t-vec
         [`(Vector ,ts ...)
          (check-type-queal? (list-ref ts i) t-arg e)
          (values (Prim 'vector-set! (list e1^ (Int i) e-arg^))
                  ;类型为void
                  'Void)]
         [else
          (error "")])]

      [(Prim 'vector-length (list e1))
       (define-values (e^ t) (recur e1))
       (match t
         [`(Vector ,ts ...)
          (values (Prim 'vector-length (list e^)) 'Integer)]
         [else
          (error "")])]

      [(Prim 'eq? (list arg1 arg2))
       (define-values (e1 t1) (recur arg1))
       (define-values (e2 t2) (recur arg2))
       (match* (t1 t2)
         [(`(Vector ,ts1 ...) `(Vector ,ts2 ...))
          (void)]
         [(other wise)
          (check-type-equal? t1 t2 e)])
       (values (Prim 'eq? (list e1 e2)) 'Boolean)]

      [(HasType (Prim 'vector es) t)
       (recur (Prim 'vector es))]

      [(HasType e1 t)
       (define-values (e1^ t^) (recur e1))
       (check-type-equal? t t^ e)
       (values (HasType e1^ t) t)]

      [(GlobalValue name)
       ...])))

;;; root heap
;void initialize(uint64_t rootstack_size, uint64_t heap_size)
;;; root request
;void collect(int64_t** rootstack_ptr, uint64_t bytes_requested)

;; 创建vector的过程
;(has-type (vector e0 ...en–1) type)
;=⇒
;(let ([x0 e0]) ... (let ([xn–1 en–1])
;(let ([_ (if (< (+ (global-value free_ptr) bytes) (global-value fromspace_end))
;             (void)
;             (collect bytes))])
;(let ([v (allocate len type)])
;(let ([_ (vector-set! v 0 x0)]) ...
;(let ([_ (vector-set! v n – 1 xn–1)])
;v) ... )))) ...)

(has-type (vector e0 ... en-1) type)

;; 在垃圾回收期间，不能有已经分配地址，但是没有初始化的tuple
;; 所有需要先初始化所有的表达式

;; 这是具体语法，抽象语法会携带(HasType ... )
;; 多个let的嵌套
(let ([x0 e0])
  ...
    (let ([xn-1 en-1]) ;; 部分1 
      (let ([_ (if (< (+ (global-value free_ptr) bytes)
                      (global_value fromspace_end))
                   (void)
                   ;; bytes 是需要的字节数 = 8 + len*8
                   (collect bytes))]) 
        ;; len 是成员个数
        (let ([v (allocate len type)]);; 部分2
          (let ([_ (vector-set! v 0 x0)])
            ...
              (let ([_ (vector-set! v (- n 1) xn-1)]) ;; 部分3
                ;; 返回v
                ;; 一直嵌套到最里层
                v))))));; 部分4，也就是v

;; 对应(vector-ref (vector-ref (vector (vector 42)) 0) 0)
(vector-ref
 (vector-ref
  (let ([vecinit6
         (let ([_4 (if (< (+ (global-value free_ptr) 16)
                          (global-value fromspace_end))
                       (void)
                       (collect 16))])
           (let ([alloc2 (allocate 1 (Vector Integer))])
             (let ([_3 (vector-set! alloc2 0 42)])
               alloc2)))])
    (let ([_8 (if (< (+ (global-value free_ptr) 16)
                     (global-value fromspace_end))
                  (void)
                  (collect 16))])
      ;; allocate的例子
      ;; 只有一个vector
      (let ([alloc5 (allocate 1 (Vector (Vector Integer)))])
        (let ([_7 (vector-set! alloc5 0 vecinit6)])
          alloc5))))
  0)
 0)

;The type parameter is the type of the tuple: (Vector type1 ...typen) where typei is the type of the ith element.

(vector-ref
 (vector-ref
  (let ([vecinit6 (let ([_4 (if (< (+ (global-value free_ptr) 16)
                                   (global-value fromspace_end))
                                (void)
                                (collect 16))])
                    ;; allocate: the elements are not initialized
                    (let ([alloc2 (allocate 1 (Vector Integer))])
                      (let ([_3 (vector-set! alloc2 0 42)])
                        alloc2)))])
    (let ([_8 (if (< (+ (global-value free_ptr) 16)
                     (global-value fromspace_end))
                  (void)
                  (collect 16))])
      (let ([alloc5 (allocate 1 (Vector (Vector Integer)))])
        (let ([_7 (vector-set! alloc5 0 vecinit6)])
          alloc5))))
  0) 0)


(define (type-check-exp env e)
  (match e
    [(Var x)
     (define type (match-alist x env))
     (values (HasType (Var x) type) type)]
    ...
    [(Prim 'vector-set! (list vect (Int i) val))
     (define-values (vect-exp vect-type) (type-check-exp env vect))
     (define-values (i-exp i-type) (type-check-exp env (Int i)))
     (define-values (val-exp val-type) (type-check-exp env val))
     (if (not (eq? i-type 'Integer))
         (error "The type of index for vector-set! must be an Integer")
         (if (not (eq? (car vect-type) 'Vector))
             (error "Vector set got a non vector")
             (if (not (equal? (list-ref vect-type (add1 i)) val-type))
                 (error (format "Changing vector types is not supported got ~a ~a" 
                     (list-ref vect-type (add1 i)) val-type))
                 (values (HasType (Prim 'vector-set! (list vect-exp i-exp val-exp))
                                  'Void) 'Void))))]
    ...))


(define v (vector 1 2 #t)) ;; [1,2,#t] : (Vector Integer Integer Boolean)
(vector-ref v 0) ;; 1
(vector-ref v 1) ;; 2
(vector-set! v 0 5)       ;; [5,2,#t]
(vector-ref v 0) ;; 5

;type ::= Integer | Boolean | (Vector type+) | Void
;exp ::= ... 
;  | (vector exp+)
;  | (vector-ref exp int)
;  | (vector-set! exp int exp)
;  | (void)
;R3 ::= (program exp)

  (vector-ref
    (let ([t (vector 3 7)])
      t)
    0)  ;; 3

  ;; Question: How is that different from integers?

  (+
    (let ([t 5])
      t)
    0)

  ;; Answer: Ok, consider this example in which (vector 3 7)
  ;; is not returned from the `let`, but we can nevertheless
  ;; refer to it through the vector bound to `x`.

  (let ([x (vector (vector))])           ;; x ->    [ * ]
    (+                                   ;;           |
       (let ([t (vector 3 7)])           ;; t ->    [3,7]
          (vector-set! x 0 t)
          5) ;; 5
       (vector-ref (vector-ref x 0) 0))) ;; 3
  ;; 8

;Lalloc-monadic
;atm ::= (Int int) | (Var var) | (Bool bool) | (Void)
;exp ::= atm | (Prim 'read ()) | ... | (Collect int) | (Allocate int type) | (GlobalValue var)


(define (expose-exp e)
  (match e
    [(HasType (Prim 'vector es) type)
     (let* ([len (length es)]
            [bytes (* 8 len)]
            ;; vector的变量名
            [vect (gensym 'vec)]
            ;; 生成n个变量
            [vars (generate-n-vars len)])
       ;; 只形成第一部分的let，其余的嵌在其中的do-allocate中
       ;; do-allocate中嵌入let-set的部分
       ;; let-set的中间嵌入最终的值(HasType (Var vect) type)
       (expend-into-lets vars ;; 该参数和对应的exps，形成部分1， a sequence of temporary variable bindings for the initializing expressions,
                         ;;vars 对应的 exps
                         ;; 想想sicp中的name，命名
                         (for/list ([e es]) (expose-exp e)) 
                         ;;部分2  a conditional call to collect,
                         (do-allocate vect len bytes ;;vector的变量名
                                      ;; the initialization of the tuple. 各种set
                                      (bulk-vector-set
                                       ;; 最终的返回值
                                       (HasType (Var vect) type)
                                       ;; 与第一部分let中对应的变量
                                       vars type)
                                      type)
                         type))]
    [else e]))
                 
(define (generate-n-vars n)
  (if (zero? n)
      '()
      (cons (gensym 'tmp) (generate-n-vars (sub1 n)))))

(define (expend-into-lets vars exps base bas-type)
  (if (empty? exps)
      ;; 嵌入到其中的部分
      ;; 有三次嵌入，分别是allocate和let-set以及最终值
      base
      (HasType (Let (car vars) (car exps)
                    (expend-into-lets (cdr vars) (cdr exps) base base-type))
               base-type)))

(define (do-allocate vect len bytes base type)
  (Let '_ (If (Prim '< (list (Prim '+ (list (GlobalValue 'free_ptr) (Int bytes)))
                             (GlobalValue 'fromspace_end)))
              (Void)
              (Collect bytes))
       ;;body
       (Let vect (Allocate len type)
            ;; 嵌入其中的let-set
            base)))

(define (bulk-vector-set vect vars type)
  (expend-into-lets (duplicate '_ (length vars)) (make-vector-set-exps vect 0 vars)
                    ;; base 嵌入到let-set中的部分，也就是最终的值
                    vect type))

(define (duplicate x n)
  (if (zero? n)
      '()
      (cons x (duplicate x (sub1 n)))))

(define (make-vector-set-exps vect accum vars) ;; accum 为index
  (if (empty? vars)
      '()
      (cons (Prim 'vector-set! (list vect (Int accum) (Var (car vars))))
            (make-vector-set-exps vect (add1 accum) (cdr vars)))))

;;-----------

(define generate-n-vars
  (lambda (len)
    (if (zero? n)
        '()
        (cons (gensym 'tmp)
              (generate-n-vars (sub1 n))))))

(define (expose-exp e)
  (match e
    [(HasType (Prim 'vector es) type)
     (let* ([len (length es)]
            [bytes (* 8 len)]
            [vect (gensym 'vec)]
            [vars (generate-n-vars len)])
       (define let-set-continuation (HasType (Var vect)) type)
       (define allocate-continaution
         (bulk-vector-set let-set-continuation vars type))
       (define let-tmp-continuaiton
         (do-allocate vect len bytes allocate-continuation type))
       (expend-into-lets vars (for/list ([e es]) (expose-exp e))
                         let-tmp-continuation type))]))

(define (bulk-vector-set vect vars type)
  (expend-into-lets (duplicate '_ (length vars))
                    (make-vector-set-exps vect 0 vars)
                    vect type))

(define (duplicate x n)
  (if (zero? n)
      '()
      (cons x (duplicate x (sub1 n)))))

(define (make-vector-set-exps vect accum vars)
  (if (empty? vars)
      '()
      (cons (Prim 'vector-set! (list vect (Int accum) (Var (car vars))))
            (make-vector-set-exps vect (add1 accum) (cdr vars)))))

(define (do-allocate vect len bytes continuation type)
  (Let '_ (If (Prim '< (list (Prim '+ (list (GlobalValue 'free_ptr) (Int bytes)))
                             (GlobalValue 'fromspace_end)))
              (Void)
              (Collect bytes))
       (Let vect (Allocate len type)
            continuation)))

(define (expend-into-lets vars exps continuation base-type)
  (if (empty? exps)
      continutaion
      (HasType (Let (car vars) (car exps) (expend-into-lets (cdr vars) (cdr exps) continuation base-type))
               base-type)))

;; Larray interpreter
(define interp-Lvecof-class

  (define (interp-op op)
    (match op
      ['make-vector make-vector]
      ['vectorof-length vector-length]
      ['vectorof-ref
       ;; 返回一个函数
       (lambda (v i)
         (if (< i (vector-length v))
             (vector-ref v i)
             (error "")))]
      [...])))


;; Larray type check

(let ([x 0])
  (let ([y 0])
    (let ([z 20])
      (let ([f (lambda: ([a : Integer]) : Integer (+ a (+ x z)))])
        (begin
          (set! x 10)
          (set! y 12)
          (f y))))))

(define (f) : ( -> Integer)
  (let ([x 0])
    (let ([g (lambda: () : Integer x)])
      (begin
        (set! x 42)
        g))))
((f))

;(type ... -> type)
;(exp exp ...)
;(define (var [var:type] ...) : type texp)
;def ... exp

;(type ... -> type)
;(Apply exp exp ...)
;(Def var ([var:type] ...) : type '() exp)
;(ProgramDefsExp '() (def ...) exp)

;; 参数使用中括号
(define (map [f : (Integer -> Integer)] [v : (Vector Integer Integer)]) : (Vector Integer Integer)
  (vector (f (vector-ref v 0)) (f (vector-ref v 1))))
(define (inc [x : Integer]) : Integer
  (+ x 1))
(vector-ref (map inc (vector 0 41)) 1)

;; function interperter
(define (interp-program p)
  (match p
    [(ProgramDefsExp info ds body)
     (let ([top-level (for/list ([d ds]) (interp-def d))])
       (for/list ([f (in-dict-values top-level)])
         (set-box! f (mtach (unbox f)
                            [(Function xs body '())
                             (Function xs body top-level)])))
       ((interp-exp top-level) body))]
    [(ProgramDefs info ds)
     (define top-level (for/list ([d ds]) (interp-def d)))
     (for ([f (in-dict-values top-level)])
       (set-box! f (match (unbox f)
                     [(Funciton xs body '())
                      (Funciton xs body top-level)])))
     ((interp-exp top-level) (Apply (Var 'main) '()))]))
      
(define (interp-def d)
  (match d
    [(Def f (list `[,xs : ,ps] ...) rt _ body)
     (cons f (box (Function xs body '())))]))

(define (interp-exp env)
  (lambda (e)
    (define recur (interp-exp env)
      (match e
        [(Apply fun args)
         (define fun-val (recur fun))
         (define arg-vals (for/list ([e args]) (recur e)))
         (apply-fun fun-val arg-vals e)]
        [else
         ((super interp-exp evn) e)]))))

(define (apply-fun fun-val arg-vals e)
  (match fun-val
    [(Function xs body fun-env)
     (define params-args (for/list ([x xs] [arg arg-vals])
                           ;; 为什么要使用box
                           (cons x (box arg))))
     (define new-env (append params-args fun-env))
     ((interp-exp new-env) body)]
    [else
     (error "")]))

;;---------type check-----------------------------------------------

(define (type-check-program e)
  (match e
    [(ProgramDefsExp info ds body)
     ;; 获取所有函数的类型，要使用这些类型检查body的类型
     (define new-env (for/list ([d ds]) (cons (Def-name d) (fun-def-type d))))
     ;; 检查每个函数的类型
     (define ds^ (for/list ([d ds]) ((type-check-def new-env) d)))
     ;; 检查body的类型
     (define-values (body^ ty) ((type-check-exp new-env) body))
     (check-type-equal? ty 'Integer body)
     (ProgramDefsExp info ds^ body^)]
    [(ProgramDefs info ds)
     (define new-env (for/list ([d ds]) (cons (Def-name d) (fun-def-type d))))
     (define ds^ (for/list ([d ds]) ((type-check-def new-env) d)))
     (ProgramDefs info ds^)]
    [(Program info body)
     (define-values (body^ ty) ((type-check-exp '()) body))
     (check-type-equal? ty 'Integer body)
     ;; 返回ProgramDefsExp
     (ProgramDefsExp info '() body^)]
    [else
     (error "")]))
     


(define (fun-def-type d)
  (match d
    [(Def f (list `[,xs : ,ps] ...) rt info body)
     `(,@ps -> ,rt)] ;; 重点
    [else
     (error "")]))

(define (type-check-def env)
  (lambda (e)
    (match e
      [(Def f (and p:t* (list `[,xs : ,ps] ...)) rt info body)
       (unless (< (length xs) max-params) (error ""))
       (define new-env (append (map cons xs ps) env))
       (define-values (body^ ty^) ((type-check-exp new-env) body))
       ;; body的类型需要与函数的返回类型保持一致
       (check-type-equal? ty^ rt body)
       (Def f p:t* rt info body^)]
      [else
       (error "")])))
       

(define (type-check-exp env)
  (lambda (e)
    (define recur (type-check-exp env))
    (match e
      [(FunRef f n)
       (values (FunRef f n) (dict-ref env f))]
      [(Apply e es)
       (define-values (e^ es^ rt) (type-check-apply env e es))
       (values (Apply e^ es^) rt)]
      [(Call e es)
       (define-values (e^ es^ rt) (type-check-apply env e es))
       (values (Call e^ es^) rt)]
      ;[else ((super type-check-exp env) e)]
      [(Prim 'make-vector (list e1 e2))
       (define-values (e1^ t1) (recur e1))
       (define-values (e2^ elt-type) (recur e2))
       (define vec-type `(Vectorof ,elt-type))
       (values (HasType (Prim 'make-vector (list e1^ e2^)) vec-type) vec-type)]
      [(HasType (Prim 'make-vector es) t)
       (recur (Prim 'make-vector es))]
      [(Prim (or 'vector-ref 'vectorof-ref) (list e1 e2))
       (define-values (e1^ t1) (recur e1))
       (define-values (e2^ t2) (recur e2))
       (match* (t1 t2)
         [(`(Vectorof ,elt-type) 'Integer)
          (values (Prim 'vectorof-ref (list e1^ e2^)) elt-type)]
;         [(other wist)
;          (type-check-exp env) e]
         )]
      [...])))
       
(define (type-check-apply env e es)
  (define-values (e^ ty) ((type-check-exp env) e)) ;; 获取函数的类型
  (define-values (e* ty*) (for/lists (e* ty*) ([e (in-list es)]) ((type-check-exp env) e)))
  (match ty
    [`(,ty^* ... -> ,rt)
     (for ([arg-ty ty*] [param-ty ty^*]) ;; 实际参数类型和形参类型
       (check-type-equal? arg-ty param-ty (Apply e es)))
     (values e^ e* rt)] ;; 函数 参数 返回类型
    [else
     (error "")]))

(define (check-type-equal? t1 t2 e)
  (unless (type-equal? t1 t2) (error "check-type-equal?")))

(define (type-equal? t1 t2)
  (match* (t1 t2)
    ;; function
    [(`(,ts1 ... -> ,rt1) `(,ts2 ... -> ,rt2))
     ;; 每个对应的参数和返回值类型相等
     (and (for/and ([t1 ts1] [t2 ts2])
            (type-equal? t1 t2))
          (type-equal? rt1 rt2))]
;    [(other wise)
;     (super type-equal? t1 t2)]
    ;; tuple
    [(`(Vector ,ts1 ...) `(Vector ,ts2 ...))
     (for/and ([t1 ts1] [t2 ts2])
       (type-equal? t1 t2))]
    ;; while
    [('_ t2) #t]
    [(t1 '_) #t]
    [(other wise)
     (equal? t1 t2)]))

;; Lfun

(define (reveal-functions e)
  (match e
    [(ProgramDefs info ds)
     (define funs (for/list ([d ds])
                    (cons (Def-name d) (length (Def-param* d)))))
     (ProgramDefs info (for/list ([d ds])
                         ((reveal-functions-def funs) d)))]
    [else (error "")]))

(define (reveal-functions-def funs)
  (lambda (e)
    (match e
      [(Def f params rt info body)
       (Def f params rt info ((reveal-functions-exp funs) body))]
      [else (error "")])))

(define (reveal-functions-exp env)
  (lambda (e)
    (let* ([recur (reveal-functions-exp env)])
      (match e
        [(Int n) (Int n)]
        [(Var x)
         (define f-count (assq x env))
         (cond
           [f-count
            (FunRef (car f-count) (cdr f-count))]
           [else (Var x)])]
        [(Void) (Void)]
        [(Bool b) (Bool b)]
        [(HasType e t) (HasType (recur e) t)]
        [(Let x e body)
         (Let x (recur e) (recur body))]
        [(If cnd thn els)
         (If (recur cnd) (recur thn) (recur els))]
        [(Prim op es)
         (Prim op (map recur es))]
        [(Apply f es)
         (Apply (recur f) (map recur es))]
        [else (error "")]))))

;;-------------------------

(define (limit-functions e)
  (match e
    [(ProgramDefs info ds)
     (ProgramDefs info (for/list ([d ds]) (limit-functions-def d)))]
    [else (error "")]))

(define (limit-functions-def d)
  (match d
    [(Def f params rt info body)
     (define n (vector-length arg-registers))
     (define new-params
       (for/list ([p params])
         (match p
           [`[,x : ,t]
            `[,x : ,(limit-type t)]])))
     (cond
       [(<= (length new-params) n)
        (Def f new-params rt info ((limit-functions-exp '()) body))] ;; 为什么env为空链表
       [else
        (define vec-param (gensym 'vec-param))
        ;; 将参数分为两部分
        (define-values (first-params last-params)
          (cond
            [(> (length new-params) n)
             (split-at new-params (- n 1))]
            [else (values new-params '())]))
        ;; 创建类型
        (define vec-typ
          `(Vector ,@(map (lambda (e) (match e
                                        [`(,arg : ,typ) typ]))
                          last-params)))
        ;; 创建环境，用于处理body
        ;; (Var xi) ⇒ (Prim 'vector-ref (list tup (Int k))) 其中k为i-6
        ;; 想想该如何表示这个环境
        ;; 首先要存储xi对应的tup也就是vector的id，想想这个id是怎么来的，临时变量就可以
        ;; 函数调用的时候又该如何传递，直接传递一个vector
        ;; 第二，存储对应的下标
        ;; 第三，存储其他信息
        (define param-env
          (map (lambda (p i)
                 (match p
                   [`(,arg : ,typ)
                    `(,arg ,typ ,vec-typ ,(Var vec-param) ,i)]))
               last-params (range (length last-params))))
        (define new-body ((limit-functions-exp param-env) body))
        (Def f (append first-params `((,vec-param : ,vec-tup))) rt info new-body)])]))
                    
                                         

;; 主要用于body和调用
;; env中主要存储大于等于6的对应的参数
(define (limit-functions-exp param-env)
  (lambda (e)
    (define recur (limit-functions-exp param-env))
    (match e
      [(Int n) e]
      [(Var x)
       (let ([res (assq x param-env)])
         (cond
           [res
            (match res
              [`(,arg ,typ1 ,typ2 ,vec ,ind)
               (Prim 'vector-ref (list vec (Int ind)))]
              [else (error "")])]
           [else (Var x)]))]
      [...]
      [(Apply f es)
       (define n (vector-length arg-registers))
       (cond
         [(<= (length es) n) ;; 参数的个数小于等于6个
          (Apply f (map recur es))]
         [else
          ;; 1. 首先将参数分为两部分
          (define-values (first-es last-es)
            (cond
              [(> (length es) n)
               (split-at es (- n 1))]
              [else
               (values es '())]))
          ;; 2. 将第二部分变成vector
          (define vector-val (Prim 'vector (map recur last-es)))])]
      [else (error "")])))
          

(define (limit-type t)
  (match t
    [`(Vector ,ts ...)
     (define ts^ (for/list ([t ts]) (limit-type t)))
     `(Vector ,@ts^)]
    [`(,ts ... -> ,rt)
     (define ts^ (for/list ([t ts]) (limit-type t)))
     (define rt^ (limit-type rt))
     (define n (vector-length arg-registers))
     (cond
       [(> (length ts^) n)
        (define-values (first-ts last-ts) (split-at ts^ (- n 1)))
        `(,@first-ts (Vector ,@last-ts) -> ,rt^)]
       [else
        ;`(,@ts^ -> ,rt^)])]
        t])]
    [else t]))

;; explicate-control

(define (explicate-control p)
  (match p
    [ProgramDefs info ds]
    (define new-ds (for/list ([d ds]) (explicate-control-def d)))
    (ProgramDefs info new-ds)))


(define (explicate-control-def d)
  (match d
    [(Def f params info body)
     (set! basic-blocks '())
     (define body-block (explicate-tail body))
     (define new-CFG (dict-set basic-block (symbol-append f 'start) body-block))
     (Def f params info new-CFG)]))

(define (explicate-pred cnd thn-block els-block)
  (match cnd
    [(Apply f arg*)
     (define tmp (gensym 'tmp))
     (Seq (Assign (Var tmp) (Call f arg*))
          (IfStmt (Prim 'eq? (list (Var tmp) #t))
                  (create-block thn-block)
                  (create-block els-block)))]
    [(Var x)
     (generic-explicate-pred cnd thn-block els-block)]
    [(Bool #t)
     (create-block thn-blcok)]
    [(Bool #f)
     (create-block els-blcok)]
    [(Prim 'not (list e))
     (explicate-pred e els-block thn-block)]
    [(Prim op arg*)
     #:when (set-member? (comparison-ops) op)
     (IfStmt (Prim op arg*)
             (create-block thn-block)
             (create-block els-block))]
    [(Let x rhs body)
;     (if (let ([x rhs])
;           body)
;         xxx
;         yyy)
     (define body-blcok (explicate-pred body thn-block els-block))
     (explicate-assign rhs x body-block)]
    [(If cnd thn els)
;     (if (if cnd
;             thn
;             els)
;         xxx
;         yyy)
     ;; 要生成4个block
     ;; 这个是continuation，需要翻过来，所以先计算外面的，再计算里面的，然后把外面的放到里面，里面的放到外面
     (define thn-goto (create-block thn-block))
     (define els-goto (create-block els-block))
     (define new-thn (explicate-pred thn thn-goto els-goto))
     (define new-els (explicate-pred els thn-goto els-goto))
     (explciate-pred cnd new-thn new-els)]
    [else (error "explicate pred error")]))

(define (generic-explicate-pred cnd thn-block els-block)
  (IfStmt (Prim 'eq? (list cnd (Bool #t)))
          (create-block thn-block)
          (create-block els-blcok)))

     
(define (explicate-tail exp)
  (match  exp
    [(Int n) (values (Return (Int n)) '())]
    [(Var v) (values (Return (Var v)) '())]
    [(Bool bool) (values (Return (Bool bool)) '())]
    [(Prim rator rand) (values (Return (Prim rator rand)) '())]
    [(Let var exp body)
     (let*-values ([(let-body variables1) (explicate-tail body)]
                   [(assigned-tail variables2) (explicate-assign exp var let-body)])
       (values assigned-tail (cons var (append variables1 variables2))))]
    [(If cnd thn els)
     (let*-values ([(thn-tail vars1) (explicate-tail thn)]
                   [(els-tail vars2) (explicate-tail els)])
     (let-values ([(cnd-tail vars3) (explicate-pred cnd thn-tail els-tail)])
       ;; (values cnd-tail (append vars3 vars1 vars2))))]))
       (values cnd-tail (append vars1 vars2 vars3))))]
    [(Apply f arg*)
     (TailCall f arg*)]
    ))

(define (explicate-assign e x cont-block)
  (match e
    [(Aplly f arg*)
     ;; 想想TailCall和Call的不同
     (Seq (Assign (Var x) (Call f arg*)) cont-block)]
    [else ...]))

;; lfun select instructions
(define (select-instr-def d)
  (match d
    [(Def f (list `[,xs : ,ps] ...) rt info CFG)
     (define new-CFG (for/list ([label tail] [in-dict CFG])
                       (cons label (Block '() (select-instr-tail tail)))))
     ;; 这一步是做什么的？
     (define param-moves (for/list ([param xs] [r arg-registers])
                           (Instr 'movq (list (Reg r) (Var param)))))
     (define start-label (symbol-append f 'start))
     (define new-start
       (match (dict-ref new-CFG start-label)
         [(Block info ss)
          (Block info (append param-moves ss))]))
     (define newer-CFG (dict-set new-CFG start-label new-start))
     ;; 更新info信息
     (define new-info
       (dict-set-all info `((num-params . ,(length xs))
                            (locals-types . ,(append (map cons xs ps) (dict-ref info 'locals-types))))))
     (Def f '() 'Integer new-info newer-CFG)]))

(define (select-instr-tail t)
  (match t
    [(TailCall f es)
     (define new-f ...)
     (define new-es (for/list ([e es]) ...))
     ;; movq arg1 %rdi
     ;; movq arg2 %rsi
     ;; ...
     ;; tailjmp *f n
     (append (for/list ([arg new-es] [r arg-registers])
               (Instr 'movq (list arg (Reg r))))
             (list (TailJmp new-f (length es))))]
    [(Return e)
     ;; movq e %rax ; 将e的值move到rax中
     ;; => 
     ;; instr1
     ;; instr2
     ;; ...
     ;; jmp 'f-name-conclusion
     (define ret-label 'f-name-conclusion)
     (append (select-instr-stmt (Assign (Reg 'rax) e))
             (list (Jmp ret-label)))]
    [(Seq stmt tail)
     (define s ...)
     (define t ...)
     (append s t)]
    [(Goto label)
     (list (Jmp label))]
    [...]))

(define (select-instr-stmt stmt)
  (match stmt
    ;; lhs = (FunRef f n)
    [(Assign lhs (FunRef f n))
     (define new-lhs ...)
     (list (Instr 'leaq (list (FunRef f n) new-lhs)))]
    [(Assign lhs (Call f es))
     (define new-lhs ...)
     (define new-f ...)
     (define new-es ...)
     ;; movq arg1 %rdi
     ;; movq arg2 %xxx
     ;; ...
     ;; indirectCall new-f n
     ;; movq %rax lhs
     (append (for/list ([arg new-es] [r arg-registers])
               (Instr 'movq (list arg (Reg r))))
             (list (IndirectCallq ...)
                   (Instr 'movq (list ...))))]
    [(Assign v e)
     (match e
       [(? atm? e)
        ...]
       [(Prim 'read '())
        ...]
       ...)]))

;; lambda
(define ((interp-exp env) e)
  (define recur (interp-exp env))
  (match e
    [(Lambda (list `[,xs : ,Ts] ...) rT body)
     (Function xs body env)];; 重点是这个env
    [(Apply fun args)
     (define fun-val (recur fun))
     (define arg-vals (for/list ([e args]) (recur e)))
     (apply-fun fun-val arg-vals e)]
    [...]))

(define (interp-program p)
  (match p
    [(ProgramDefsExp info ds body)
     (let ([top-level (for/list ([d ds]) (interp-def d))])
       (for/list ([f (in-dict-values top-level)])
         (set-box! f (match (unbox f)
                       [(Function xs body '())
                        (Funciton xs body top-level)])));; 重点，这个是闭包，每个函数携带一个env
       ((interp-exp top-level) ;; 使用所有的函数作为env，来解释body
        body))]))

(define (apply-fun fun-val arg-vals)
  (match fun-val
    [(Function xs body fun-env);; fun-env是重点，闭包
     (define params-args (for/list ([x xs] [arg arg-vals])
                           (cons x (box arg))))
     (define new-env (append params-args fun-env))
     ((interp-exp new-env) body)]))

;; check lambda
(define (type-check-program e)
  (match e
    [(ProgramDefsExp info ds body)
     ;; 获取def的类型
     (define new-env (for/list ([d ds]) (cons (Def-name d) (fun-def-type d))))
     ;; 使用new-env check def的类型
     (define ds^ (for/list ([d ds]) ((type-check-def new-env) d)))
     ;; 使用new-env check body的类型
     (define-values (body^ ty) ((type-check-exp new-env) body))
     ;; 检查body的类型是否为Integer
     ...]))

(define (fun-def-type d)
  (match d
    [(Def f (list `[,xs : ,ps] ...) rt info body)
     `(,@ps -> ,rt)]))

(define (type-check-def env)
  (lambda (e)
    (match e
      [(Def f (and p:t* (list `[,xs : ,ps] ...)) rt info body)
       ;; 将参数与类型放入环境中
       (define new-env (append (map cons xs ps) env))
       ;; 获取body的类型，此时会获取到lambda的类型，重点是使用新的环境
       (define-values (body^ ty^) ((type-check-exp new-env) body))
       ;; 检查 ty^ 是否与 rt相等
       ...])))

(define (type-check-exp env)
  (lambda e
    (match e
      [(FunRef f n)
       ...]
      [(Apply e es)
       ...]
      [(HasType (Var x) t)
       ((type-check-exp env) (Var x))]
      [(Var x)
       ...]
      [(Lambda (and params `([,xs : ,Ts] ...)) rT body)
       ;; 将参数与对应的类型放入环境中
       (define new-env (append (map cons xs Ts) env))
       ;; 使用新的环境，check body
       (define-values (new-body bodyT) ((type-check-exp new-env) body))
       ;; body的类型是否等于rT
       (check-type-equal? rT bodyT e)
       ;; 返回Lambda的类型
       (define lambda-type `(,@Ts -> ,rt))
       (values e lambda-type)]
      [else ...])))



;; lambda
(define (convert-to-closures e)
  (match e
    [(ProgramDefs info ds)
     (ProgramDefs info (append*
                        (for/list ([d ds])
                          (convert-to-closure-def d))))]))

(define (convert-to-closures-def e)
  (match e
    [(Def f params rt info body)
     ;; 收集变量对应的类型
     ;(define types-set1 (free-variables-types params (set)))
     ;(define types-set (free-variables-types body types-set1)))
     ;; 转化body
     (define-values (new-body lambda-def)
       (convert-to-closures-exp body))
     (define new-params (for/list ([p params])
                          (match p
                            [`[,x : ,t]
                             `[,x : ,(closure-convert-type t)]])))
     (define new-rt (closure-convert-type rt))
     (cond
       [(eq? f 'main)
        (cons (Def f new-params new-rt info new-body)
              lambda-def)]
       [else
        (define fvs-tmp (gensym 'fvs))
        (cons
         (Def f (cons `[,fvs-tmp : _] new-params) new-rt info
              ;; 这个地方为什么要convert-fun-body??
              ;; 可以不用这个
              (convert-fun-body fvs-tmp '() new-body))
         lambda-def)])]
    [else
     (error "")]))


(define (convert-to-closures-exp e)
  (define recur convert-to-closures-exp)
  (match e
    [(or (Var _) (Int _) (Bool _) (Void))
     (values e '())] ;; 右侧为根据lambda生成的新的Def
    [(HasType e t)
     ;; except that each val-expr must produce as many values as corresponding ids
     (let-values ([(e^ b*) (recur e)])
       (values (HasType e^ (closure-convert-type t))
               b*))]
    [(Let x e body)
     (define-values (new-e e-lambda (recur e)))
     (define-values (new-body body-lambda (recur body)))
     (values (Let x new-e new-body) ;; 新的表达式
             (append e-lambda body-lambda))]
    [(If cnd thn els)
     (define-values (new-cnd cnd-lambda) (recur cnd))
     (define-values (new-thn thn-lambda) (recur thn))
     (define-values (new-els els-lambda) (recur els))
     (values (If new-cnd new-thn new-els)
             (append cnd-lambda thn-lambda els-lambda))]
    [(Prim op es)
     (define-values (new-es es-lambdas)
       (for/lists (ls1 ls2) ([e es])
         (recur e)))
     (values (Prim op new-es)
             (append* es-lambdas))]
    [(FunRef f n)
     ;; 转换为闭包 P154
     (values (Closure n (list (FunRef f n)))
             '())]
    [(Lambda (list `[,xs : ,Ts] ...) rT body)
     ;; body-lambda要跟本身的lambda生成的Def，进行组合
     (define-values (new-body body-lambda) (recur body))
     (define new-rT (closure-convert-type rT))
     ;; 生成一个新的Def
     (let* ([fun-name (gensym 'lambda)]
            [params (for/list ([x xs] [T Ts])
                      ;;`[,x : ,T]
                      `[,x : ,(closure-convert-type T)])]
            [rm (lambda (x s) (set-remove s x))]
            ;; 为什么要用foldl?
            ;; 将set中的变量删除xs
            [fvs (set->list (foldl rm (free-variabls body) xs))]
            [fvT ..........]
            ;; 需要一个fun-name，一个闭包name
            [closure-name (gensym 'clos)])
       (define clos-type `(Vector _ ,@fvT))
       ;; lambda 转化为闭包，并附带生成一个FunDef
       (values (Closure (length xs) (cons (FunRef fun-name (length xs)) fvs))
               ;;;;;;;;;;;;;;;;;;;;;;;;;;;[clos : (Vector _ fvts ...)]
               (cons (Def fun-name (cons `[,closure-name : ,clos-type] params) new-rT '()
                          (convert-fun-body closure-name fvs new-body)) ;; 要使用new-body
                     ;; 里面嵌入的def
                     body-lambda)))]
    [(Apply e es)
     (define-values (new-e e-lambda-def) (recur e))
     (define-values (new-es es-lambdas-defs) (for/lists (l1 l2)
                                                        ([e es])
                                               (recur e)))
     (define-values (bnds new-e^)
       (match new-e
         [(Var _)
          (values '() new-e)]
         [else
          (define tmp (gensym 'clos))
          (values (list (cons tmp new-e))
                  (Var tmp))]))
     (define new-apply
       (make-lets bnds
                  ;; let的body
                  (Apply (Prim 'vector-ref (list new-e^ (Int 0)))
                         (cons new-e^ new-es))))
     (values new-apply
             (append e-lambda-def
                     ;(append* es-lambdas-defs)
                     (apply append es-lambdas-defs)))]))

(define (closure-convert-type t)
  (match t
    [`(Vector ,ts ...)
     (define ts^ (for/list ([t ts])
                   (closure-convert-type t)))
     `(Vector ,@ts^)]
    [`(,ts ... -> ,rt)
     (define ts^ (for/list ([t ts])
                   (closure-convert-type t)))
     (define rt^ (closure-convert-type rt))
     ;;`(,@ts^ -> ,rt^)
     ;; P153 类型转换
     ;; 为什么多了个下划线，代表函数
     `(Vector ((Vector _) ,@ts^ -> ,rt^))]
    [else
     t]))

(define (free-variables e)
  (define recur free-variables)
  (match e
    [(Var x)
     (set x)]
    [(HasType e t)
     (recur e)]
    [(or (Int _) (Bool _) (FunRef f n))
     (set)]
    [(Let x e body)
     (set-union (recur e) (set-remove (recur body) x))]
    [(If cnd thn els)
     (set-union (recur cnd) (recur thn) (recur els))]
    [(Lambda (list `[,xs : ,ts] ...) rt body)
     (define (rm x s) (set-remove s x))
     (define body-fvs (recur body))
     (foldl rm body-fvs xs)]
    [(Apply e es)
     (apply set-union (cons (recur e) (map recur es)))]
    [(Prim op es)
     (apply set-union (map recur es))]
    [else
     (error "")]))     

(define (convert-fun-body clos-id free-vars body)
  (let loop ([xs free-vars] [i 1])
    (match xs
      ['()
       body]
      [(cons x xs^)
       (Let x (Prim 'vector-ref (list (Var clos-id) (Int i)))
            (loop xs^ (add1 i)))]
      [else
       (error "")])))


;; cpser传递的是上下文，计算出洞后，将洞塞到上下文中
;; cimpiler中传递的是洞，计算出洞外面的东西后，将洞塞进去


;;lambda
(define (optimize-known-calls-exp closures)
  (lambda (e)
    (let ([recur (optimize-known-calls-exp closures)])
      (match e
        [(or (Int n) (Var x) (Void) (Bool b))
         e]
        [(Prim 'vector-ref (list (Var c) (Int 0)))
         #:when (dict-has-key? closures c)
         (FunRef (dict-ref closures c))]
        [(HasType e t)
         (HasType (recur e) t)]
        [(Let x (HasType (Closure arity (list (FunRef f) fvs ...)) clos-ty)
              body)
         (define closures^ (cons (cons x f) closures))
         (define body^ ((optimize-known-calls-exp closures^) body))
         (Let x (HasType (Closure arity (cons (FunRef f) fvs)) clos-ty)
              body^)]
        ...))))

;; dynamic typing
(define (interp-Ldyn ast)
  (match ast
    [(ProgramDefsExp infos ds body)
     (define top-level
       ;; 解释所有的def
       (map (lambda (d) (interp-Ldyn-def d)) ds))
     ;; 结果为 f-name -> (Tagged (Function ...) 'Procedure)
     (for/list ([b top-level])
       (set-mcdr! b (match (mcdr b)
                      [(Function xs body '())
                       ;; 变成Tagged，为什么要这样做?
                       (Tagged (Function xs body top-level)
                               ;; 类型为 'Procedure
                               'Procedure)])))
     (define result
       ;; top-level为env
       ((interp-Ldyn-exp top-level) body))
     。。。。。。。。。]))


(define (interp-Ldyn-def ast)
  (match ast
    [(Def f xs rt info body)
     ;; '()为env
     (mcons f (Function xs body '()))]))

(define (interp-Ldyn-exp env)
  (lambda (ast)
    (define recur (interp-Ldyn-exp env))
    (define result
      (match ast
        [(Void)
         (Tagged 0 'Integer)]
        [(Var x)
         (lookup x env)]
        [(FunRef f n) ;; f的引用，函数调用的时候
         (lookup f env)]
        [(Int n)
         (Tagged n 'Integer)]
        [(Bool b)
         (Tagged b 'Boolean)]
        [(Lambda xs rt body)
         ;; lambda 的时候该如何处理?
         ;; 变成tagged，lambda变成什么样的tagged
         ;; 添加env进去
         (Tagged (Function xs body env) 'Procedure)]
        [(Prim 'vector es) ;; 定义一个vector
         (Tagged (apply vector
                        (for/list ([e es]) (recur e)))
                 'Vector)]
        [(Prim 'vector-ref (list e1 e2)) ;; 获取vector中的值
         ;; vector变量
         (define vec (recur e1)) ;; vec的值为(Prim 'vector es)分支的结果
         ;; vector索引
         (define i (recur e2))
         ;; 为什么要check-tag
         (check-tag vec 'Vector ast) ;; vec 为(Tagged ...)
         (check-tag i 'Integer ast) ;; i 为(Tagged ...)
         (unless (< (Tagged-value i)
                    (vector-length (Tagged-value vec)))
           (error ""))
         ;; 为什么返回值为vector-ref
         (vector-ref (Tagged-value vec) ;; 是一个vector
                     (Tagged-value i))]
        [(Prim 'vector-set! (list e1 e2 e3))
         (define vec (recur e1))
         (define i (recur e2))
         (define arg (recur e3))
         (check-tag vec 'Vector ast)
         (check-tag i 'Integer ast)
         (unless (< (Tagged-value i)
                    (vector-length (Tagged-value vec)))
           (error ""))
         (vector-set! (Tagged-value vec) (Tagged-value i) arg)
         ;; 返回void
         (Tagged (void) 'Void)]
        [(Let x e body)
         ((interp-Ldyn-exp (cons (cons x (recur e)) env)) body)]
        [(Prim 'and (list e1 e2))
         (recur (If e1 e2 (Bool #f)))]
        [(Prim 'or (list e1 e2))
         (define v1 (recur e1))
         (match (Tagged-value v1)
           [#f
            (recur e2)]
           [else v1])]           
        [(Prim 'not (list e1))
         (match (Tagged-value (recur e1))
           [#f
            (Tagged #t 'Boolean)]
           [else
            (Tagged #f 'Boolean)])]
        [(Prim 'eq? (list e1 e2))
         (Tagged (equal? (recur e1) (recur e2)) 'Boolean)]
        [(Prim op (list e1));; 判断逻辑
         #:when (set-member? type-predicates op)
         (tag-value ((interp-op op) (Tagged-value (recur e1))))]
        [(Prim op es)
         (define args (map recur es))
         (define tags (for/list ([arg args]) (Tagged-tag arg)))
         (unless (for/or ([expected-tags (op-tags op)])
                   (equal? expected-tags tags))
           (error ""))
         (tag-value
          (apply (interp-op op) (for/list ([a args]) (Tagged-value a))))]
        [...]))))
         

(define (check-tag val expected ast)
  ;(struct Tagged (value tag) #:transparent)
  (define tag (Tagged-tag val))
  (unless (eq? tag expected)
    (error "")))

(define (interp-op op)
  (match op
    ['+ fx+]
    ['- fx-]
    ['read read-fixnum]
    ['not (lambda (v)
            (match v
              [#t #f]
              [#f #t]))]
    ['< (lambda (v1 v2)
          (cond
            [(and (fixnum? v1) (fixnum? v2))
             (< v1 v2)]))]
    [...]
    ['procedure?
     (match-lambda
       [(Function xs body env)
        #t]
       [else #f])]
    [else
     (error "")]))

(define (tag-value v)
  (cond
    [(boolean? v)
     (Tagged v 'Boolean)]
    [...]
    [(procedure? v)
     (Tagged v 'Procedure)]))


;; any type check
(define (type-check-exp env)
  (lambda (e)
    (define recur (type-check-exp env))
    (match e
      [(If cnd thn els)
       (define-values (cnd^ Tc) (recur cnd))
       ...
       ;; 判断cnd的类型是否为boolean
       (check-type-equal? Tc 'Boolean cnd)
       ;; 判断分支的类型是否相等，可以都为Any，或者是某个具体类型
       (check-type-equal? Tt Te e)
       ;; 返回值
       (values (If cnd^ thn^ els^) (join-types Tt Te))]
      [(Prim 'any-vector-length (list e1))
       (define-values (e1^ t) (recur e1))
       ;; e1的类型为Any
       (check-type-equal? t 'Any e)
       ;; 返回值
       (values (Prim 'any-vector-length (list e1^)) 'Integer)]
      [(Prim 'any-vectorof-length (list e1))
       ...]
      [(Prim 'any-vector-ref (list e1 e2))
       ;; 
       (define-values (e1^ t1) (recur e1))
       (define-values (e2^ t2) (recur e2))
       (check-type-equal? t1 'Any e)
       (check-type-euqal? t2 'Integer e)
       (values (Prim 'any-vector-ref (list e1^ e2^)) 'Any)]

      [(Prim 'any-vector-set! (list e1 e2 e3))
       (define-values (e1^ t1) (recur e1))
       ...
       ;; t1的类型为Any
       ;; t2的类型为Integer
       ;; t3的类型为Any
       (check-type-equal? t1 'Any e)
       ...
       ;; 结果的类型为Void
       (values (Prim 'any-vector-set! (list e1^ e2^ e3^)) 'Void)]

      ;; 什么时候要变成inject
      ;; 1. e1是什么？ e1可以是任何表达式
      ;; 2. ty为e1的类型
      [(Inject e1 ty)
       (unless (flat-ty? ty)
         (error ""))
       (define-values (new-e1 e-ty) (recur e1))
       (check-type-equal? e-ty ty e)
       ;; 结果为Any类型
       (values (Inject new-e1 ty) 'Any)]

      [(Project e1 ty)
       (unless (flat-ty? ty)
         (error ""))
       (define-values (new-e1 e-ty) (recur e1))
       ;; 类型为Any
       (check-type-equal? e-ty 'Any e)
       (values (Project new-e1 ty) ty)]
      ...)))

(define (flat-ty? ty)
  (match ty
    [(or 'Integer 'Boolean '_ 'Void)
     #t]
    ;; 闭包
    [`(Vector ((Vector _) ;; 闭包的生成函数
               ,ts ... -> Any)) ;; lambda的入参和出参
     (for/and ([t ts])
       ;; 为什么闭包的入参都是Any类型
       (eq? t 'Any))]
    ;; vector类型
    ;; 为什么vector的元素类型都为Any
    [`(Vector ,ts ...)
     (for/and ([t ts])
       (eq? t 'Any))]
    ;; 函数
    [`(,ts ... -> ,rt)
     (and
      (eq? rt 'Any)
      (for/and ([t ts])
        (eq? t 'Any)))]
    [else
     #f]))


;; gradual typing


    

      
       
       
  
         
         
        
     


  


     



       
       
       
     
       



     
  
  




     
