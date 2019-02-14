#lang racket
(require "simpleParser.rkt")

; The overarching method interpret
(define interpret
  (lambda (file_name)
    (run_code (parser file_name) '((return)(null)) (lambda (v) v))))


; run_code will run code until a return or until end of file
(define run_code
  (lambda (parse_tree state return)
    (cond
      [(not (eq? (m_value 'return state) 'null)) (return (m_value 'return state))] ; check if there is something to return
      [(null? parse_tree) "Code run, nothing returned"]
      [else (run_code (cdr parse_tree) (run_line (car parse_tree) state return) return)])))


; run_line will run a line
; returns a state
; Example: (run_line '((if (< x 10) (return 1) (return 2))) '((x)(1)) (lambda (v) v))
; Example: (run_line '(var x) '((return) (null)) (lambda (v) v)) => '((x return) (null null))
; Example: (run_line '(= x 4) '((x return) (null null)) (lambda (v) v)) => '((x return) (4 null))
(define run_line
  (lambda (expr m_state return)
    (cond
      [(null? expr) m_state]
      [(eq? (get_op expr) 'var) (if (pair? (cddr expr))
                                    (return (m_assign (cadr expr) (m_eval (caddr expr) m_state) m_state))
                                    (return (m_declare (cadr expr) m_state)))]
      [(eq? (get_op expr) '=) (m_initialize (cadr expr) (m_eval (caddr expr) m_state) m_state)]
      [(eq? (get_op expr) 'return) (m_return (cadr expr) m_state return)]
      [(eq? (get_op expr) 'if) (m_if (cadr expr) (caddr expr) (cadddr expr) m_state return)]
      [(eq? (get_op expr) 'while) (m_while (cadr expr) (caddr expr) m_state return)])))


; m_while is a while loop
(define m_while
  (lambda (condition expr1 expr2 m_state return)
    (if (eq? (m_bool condition m_state return) #t)
        (run_line expr1 m_state return) ; assume no side effects
        (return (run_line expr2 m_state return)))))

; m_if is an if statement
; Example:
(define m_if
  (lambda (condition expr1 expr2 m_state return)
    (if (eq? (m_bool condition m_state return) #t)
        (run_line expr1 m_state return) ; assume no side effects
        (return (run_line expr2 m_state return)))))

; m_return returns
(define m_return
  (lambda (expr m_state return)
    (m_initialize 'return (m_eval expr m_state) m_state)))
   

; m_assign declares the variable its passed and assigns it to the given value
; Example: (m_assign 'a '(()())) => '((a)('null))
; Example; (m_assign 'x '((a b c) (5 6 10))) => '((x a b c) (null 5 6 10))
(define m_assign
  (lambda (var val m_state)
    (cond
      [(null? var) (error 'null_variable "Tried to declare a null variable")]
      [(m_member var m_state)
       (error 'declared_variable "Tried to declare an already declared variable")]
      (else (cons (cons var (car m_state)) (list (cons val (cadr m_state))))))))


; m_declare declares the variable its passed
; Example: (m_declare 'a '(()())) => '((a)('null))
; Example; (m_declare 'x '((a b c) (5 6 10))) => '((x a b c) (null 5 6 10))
(define m_declare
  (lambda (var m_state)
    (cond
      [(null? var) (error 'null_variable "Tried to declare a null variable")]
      [(m_member var m_state)
       (error 'declared_variable "Tried to declare an already declared variable")]
      (else (cons (cons var (car m_state)) (list (cons 'null (cadr m_state))))))))


;member is a helper function that just returns whether a variable is in the list
(define m_member
  (lambda (a m_state)
    (cond
      [(null? m_state) #f]
      [(null? (car m_state)) #f]
      [(eq? a (car (car m_state))) #t]
      (else (m_member a (cons (cdar m_state) (cdr m_state)))))))

; m_initialize takes a variable, value and m_state and returns the m_state with the initialiazed variable
; Example: (m_initialize 'x 14 '((x y z a) (null 2 3 4))) => ((x y z a) (14 2 3 4))
(define m_initialize
  (lambda (var val m_state)
    (cond
      [(null? m_state) (error 'null_state "m_state is null")]
      [(m_member var m_state)
       (if (eq? var (caar m_state))
           (cons (car m_state) (list (cons val (cdadr m_state))))
           (m_initialize var val (cons (cdar m_state) (list (cdadr m_state)))))]
      (else (error 'assignment_error "The variable hasn't been declared ")))))

; m_check_initialized checks if the variable has been initialized in the state
; Example: (m_check_initialized 'f  '((x y z a)(null 2 3 4))) => #f
; Example: (m_check_initialized 'x  '((x y z a)(null 2 3 4))) => #f
; Example: (m_check_initialized 'y  '((x y z a)(null 2 3 4))) => #t
(define m_check_initialized
  (lambda (var m_state)
    (if (m_member var m_state)
        (if (eq? 'null (m_value var m_state))
            #f
            #t)
        #f)))

; m_bool takes a condition and evaluates it
; returns true or false
; Example: (m_bool '(> (* x (+ x x)) y) '((x y)(2 7)) (lambda (v) v)) => #t
; Example: (m_bool '(> (* x (+ x x)) y) '((x y)(2 9)) (lambda (v) v)) => #f
(define m_bool
  (lambda (condition m_state return)
    (cond
      [(null? condition)
       (error 'invalid "expression is empty")]
      [(symbol? condition)
       (m_value condition m_state)]
      [(or (number? condition) (boolean? condition))
       condition]
      [(eq? (get_op condition) '&&)
       (and (m_bool (left_operand condition) m_state return) (m_bool (right_operand condition) m_state return))]
      [(eq? (get_op condition) '||)
       (or (m_bool (left_operand condition) m_state return) (m_bool (right_operand condition) m_state return))]
      [(eq? (get_op condition) '>=)
       (>= (m_bool (left_operand condition) m_state return) (m_bool (right_operand condition) m_state return))]
      [(eq? (get_op condition) '<=)
       (<= (m_bool (left_operand condition) m_state return) (m_bool (right_operand condition) m_state return))]
      [(eq? (get_op condition) '>)
       (> (m_bool (left_operand condition) m_state return) (m_bool (right_operand condition) m_state return))]
      [(eq? (get_op condition) '<)
       (< (m_bool (left_operand condition) m_state return) (m_bool (right_operand condition) m_state return))]
      [(eq? (get_op condition) '==)
       (eq? (m_bool (left_operand condition) m_state return) (m_bool (right_operand condition) m_state return))]
      [(eq? (get_op condition) '!=)
       (not (eq? (m_bool (left_operand condition) m_state return) (m_bool (right_operand condition) m_state return)))]
      [(eq? (get_op condition) '!)
       (not (m_bool (left_operand condition) m_state return))]
      [else
       (m_eval condition m_state)])))
      

; It takes a parameter of a variable and the m_state
; returns a state
; Example: (m_value 'x '((a b c x) (1 2 3 4))) returns 4
(define m_value
  (lambda (var m_state)
    (cond
      [(null? m_state) (error 'undefined_var "Variable is undefined")]
      [(eq? var (car (car m_state))) (car (car (cdr m_state)))]
      (else (m_value var (cons (cdar m_state) (list (cdadr m_state))))))))


; m_eval takes a mathematical expression and evaluates it
; returns the output value
; Example: (m_eval '(+ (* (+ 4 5) 3) 3) '(()())) => 30
; Example: (m_eval '(+ (* (+ x y) 5) z) '((x y z)(1 2 3))) => 18
(define m_eval
  (lambda (exp m_state)
    (cond
      [(null? exp) (error 'null_expression "Expression is null")]
      [(number? exp) exp]
      [(symbol? exp) (m_value exp m_state)]
      [(eq? (car exp) '+) (+ (m_eval (left_operand exp) m_state) (m_eval (right_operand exp) m_state))]
      [(eq? (car exp) '-) (- (m_eval (left_operand exp) m_state) (m_eval (right_operand exp) m_state))]
      [(eq? (car exp) '/) (quotient (m_eval (left_operand exp) m_state) (m_eval (right_operand exp) m_state))]
      [(eq? (car exp) '*) (* (m_eval (left_operand exp) m_state) (m_eval (right_operand exp) m_state))]
      [(eq? (car exp) '%) (remainder (m_eval (left_operand exp) m_state) (m_eval (right_operand exp) m_state))])))


; Abstractions and definitions
(define get_op car)
(define left_operand cadr)
(define right_operand caddr)
