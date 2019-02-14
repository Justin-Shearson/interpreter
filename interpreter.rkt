#lang racket
<<<<<<< HEAD
(require "simpleParser.rkt")

(define m_bool
  (lambda (expr state return)
    (cond
      [(null? expr) state])))
=======
;It takes a parameter of a variable and the m_state and it returns a state
;Example: (m_value 'x '((a b c x) (1 2 3 4))) returns 4
(define m_value
  (lambda (var m_state)
    (cond
      [(null? m_state) (error 'undefined_var "Variable is undefined")]
      [(eq? var (car (car m_state))) (car (car (cdr m_state)))]
      (else (m_value var (cons (cdar m_state) (list (cdadr m_state))))))))
    
>>>>>>> 398750511550dba7092c14a9b7354a01ca7f518d
