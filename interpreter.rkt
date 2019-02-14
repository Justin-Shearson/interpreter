#lang racket
(require "simpleParser.rkt")

;Takes a condition and
(define m_bool
  (lambda (condition m_state return)
    (cond
      [(null? condition) (error 'invalid "expression is empty")]
      [(symbol? condition) (m_value condition m_state)]
      [(or (number? condition) (boolean? condition) condition]
      [(eq? (get_op condition) '&&) (and (m_bool (left_operand condition) m_state return) (m_bool (right_operand condition) m_state return))]
      [(eq? (get_op condition) '||) (or (m_bool (left_operand condition) m_state return) (m_bool (right_operand condition) m_state return))]
      [(eq? (get_op condition) '>=) (>= (m_bool (left_operand condition) m_state return) (m_bool (right_operand condition) m_state return))]
      [(eq? (get_op condition) '<=) (<= (m_bool (left_operand condition) m_state return) (m_bool (right_operand condition) m_state return))]
      [(eq? (get_op condition) '>) (> (m_bool (left_operand condition) m_state return) (m_bool (right_operand condition) m_state return))]
      [(eq? (get_op condition) '<) (< (m_bool (left_operand condition) m_state return) (m_bool (right_operand condition) m_state return))]
      [(eq? (get_op condition) '==) (eq? (m_bool (left_operand condition) m_state return) (m_bool (right_operand condition) m_state return))]
      [(eq? (get_op condition) '!=) (not (eq? (m_bool (left_operand condition) m_state return) (m_bool (right_operand condition) m_state return)))]
      [(eq? (get_op condition) '!) (not (m_bool (left_operand condition) m_state return) (m_bool (right_operand condition) m_state return))]
      [else (m_eval condition m_state)])))
      

;It takes a parameter of a variable and the m_state and it returns a m_state
;Example: (m_value 'x '((a b c x) (1 2 3 4))) returns 4
(define m_value
  (lambda (var m_state)
    (cond
      [(null? m_state) (error 'undefined_var "Variable is undefined")]
      [(eq? var (car (car m_state))) (car (car (cdr m_state)))]
      (else (m_value var (cons (cdar m_state) (list (cdadr m_state))))))))


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

(define get_op car)
(define left_operand cadr)
(define right_operand caddr)