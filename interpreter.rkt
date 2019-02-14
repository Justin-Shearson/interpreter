#lang racket
;It takes a parameter of a variable and the m_state and it returns a state
;Example: (m_value 'x '((a b c x) (1 2 3 4))) returns 4
(define m_value
  (lambda (var m_state)
    (cond
      [(null? m_state) (error 'undefined_var "Variable is undefined")]
      [(eq? var (car (car m_state))) (car (car (cdr m_state)))]
      (else (m_value var (cons (cdar m_state) (list (cdadr m_state))))))))

(define rvalue
  (lambda (exp)
    (cond
      [(null? exp) (error 'undefined "undefined expression")]
      [(number? exp) exp]
      [(eq? (car exp) '+) (+ (rvalue (car (cdr exp))) (rvalue (car (cdr (cdr exp)))))]
      [(eq? (car exp) '-) (- (rvalue (car (cdr exp))) (rvalue (car (cdr (cdr exp)))))]
      [(eq? (car exp) '/) (quotient (rvalue (car (cdr exp))) (rvalue (car (cdr (cdr exp)))))]
      [(eq? (car exp) '*) (* (rvalue (car (cdr exp))) (rvalue (car (cdr (cdr exp)))))]
      [(eq? (car exp) '%) (remainder (rvalue (car (cdr exp))) (rvalue (car (cdr (cdr exp)))))])))

;m_eval takes an expression and returns the value of the expression
;Example: (m_eval '(+ x y) '((x y) (5 10))) returns 15 because x and y are stored in the state

(define m_eval
  (lambda (exp m_state)
    (cond
      [(null? exp) (error 'null_expression "Expression is null")]
      [(number? exp) exp]
      [(symbol? exp) (m_value exp m_state)]
      [(eq? (car exp) '+) (+ (m_eval (operand1 exp) m_state) (m_eval (operand2 exp) m_state))]
      [(eq? (car exp) '-) (- (m_eval (operand1 exp) m_state) (m_eval (operand2 exp) m_state))]
      [(eq? (car exp) '/) (quotient (m_eval (operand1 exp) m_state) (m_eval (operand2 exp) m_state))]
      [(eq? (car exp) '*) (* (m_eval (operand1 exp) m_state) (m_eval (operand2 exp) m_state))]
      [(eq? (car exp) '%) (remainder (m_eval (operand1 exp) m_state) (m_eval (operand2 exp) m_state))])))

(define operator car)
(define operand1 cadr)
(define operand2 caddr)



