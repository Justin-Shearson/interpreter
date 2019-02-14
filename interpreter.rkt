#lang racket
(require "simpleParser.rkt")

;Takes a condition and
(define m_bool
  (lambda (condition state return)
    (cond
      [(null? condition) (error 'invalid "expression is empty")]
      [(eq? (get_op condition) '&&) (and (m_bool (left_operand condition) state return) (m_bool (right_operand condition) state return))]
      [(eq? (get_op condition) '||) (or (m_bool (left_operand condition) state return) (m_bool (right_operand condition) state return))]
      [(eq? (get_op condition) '>=) (>= (m_bool (left_operand condition) state return) (m_bool (right_operand condition) state return))]
      [(eq? (get_op condition) '<=) (<= (m_bool (left_operand condition) state return) (m_bool (right_operand condition) state return))]
      [(eq? (get_op condition) '>) (> (m_bool (left_operand condition) state return) (m_bool (right_operand condition) state return))]
      [(eq? (get_op condition) '<) (< (m_bool (left_operand condition) state return) (m_bool (right_operand condition) state return))]
      [(eq? (get_op condition) '==) (== (m_bool (left_operand condition) state return) (m_bool (right_operand condition) state return))]
      [(eq? (get_op condition) '!=) (!= (m_bool (left_operand condition) state return) (m_bool (right_operand condition) state return))]
      [(eq? (get_op condition) '!) (! (m_bool (left_operand condition) state return) (m_bool (right_operand condition) state return))]
      

;It takes a parameter of a variable and the m_state and it returns a state
;Example: (m_value 'x '((a b c x) (1 2 3 4))) returns 4
(define m_value
  (lambda (var m_state)
    (cond
      [(null? m_state) (error 'undefined_var "Variable is undefined")]
      [(eq? var (car (car m_state))) (car (car (cdr m_state)))]
      (else (m_value var (cons (cdar m_state) (list (cdadr m_state))))))))


(define get_op car)
(define left_operand cadr)
(define right_operand caddr)