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
      [(null? parse_tree) "Code run, nothing returned"]
      [(not (eq? (m_value 'return state) 'null)) (return (m_value 'return state))] ; check if there is something to return
      [else (run_code (cdr parse_tree) (run_line (car parse_tree) state return) return)])))


; run-line will run a line
; returns a state
(define run_line
  (lambda (expr state return)
    (cond
      [(null? expr) state]
      [(eq? (get_op expr) 'var) "Var"]
      [(eq? (get_op expr) '=) "Assign"]
      [(eq? (get_op expr) 'return) ]
      [(eq? (get_op expr) 'if) "If"]
      [(eq? (get_op expr) 'while) "While"])))


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
