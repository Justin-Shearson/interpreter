#lang racket
(require "simpleParser.rkt")

; The overarching method interpret
(define interpret
  (lambda (file_name)
    (run_code (parser file_name) '(()()) (lambda (v) v))))

; run_code will run code until a return or until end of file
(define run_code
  (lambda (parse_tree state return)
    (cond
      [(null? parse_tree) "Code run, nothing returned"]
      [(not (eq? (m_value 'return state) 'null)) (return (m_value 'return state))] ; check if there is something to return
      [else (run_code (cdr parse_tree) (run_line (car parse_tree) state return) return)])))

; run-line will return a state
(define run_line
  (lambda (expr state return)
    (cond
      [(null? expr) state]
      [(eq? (get_operator expr) 'var) "Var"]
      [(eq? (get_operator expr) '=) "Assign"]
      [(eq? (get_operator expr) 'return) ]
      [(eq? (get_operator expr) 'if) "If"]
      [(eq? (get_operator expr) 'while) "While"])))


;It takes a parameter of a variable and the m_state and it returns a state
;Example: (m_value 'x '((a b c x) (1 2 3 4))) returns 4
(define m_value
  (lambda (var state)
    (cond
      [(null? state) (error 'undefined_var "Variable is undefined")]
      [(eq? var (car (car state))) (car (car (cdr state)))]
      (else (m_value var (cons (cdar state) (list (cdadr state))))))))

(define get_operator car)