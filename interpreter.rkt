#lang racket
(provide (all-defined-out))
(require "simpleParser.rkt")

;;;; ***************************************************
;;;; Austin Guo, Daniel Luo, Justin Shearson
;;;; EECS 345 Spring 2019
;;;; Interpreter Part 2
;;;; ***************************************************

;; The highest-level function, interpret
(define interpret
  (lambda (file_name)
    (call/cc
     (lambda (k)
       (run_code (parser file_name)
              '((return)(null))
              k
              (lambda (v) (error 'inappropriate_break "Tried to break when not inside a loop"))
              (lambda (v) v)
              (lambda (v) (error 'inappropriate_throw "Tried to throw when not inside a try")))))))


;; run_code will execute all the code within the parse tree
(define run_code
  (lambda (parse_tree state return break continue throw)
    (cond
      [(not (eq? (m_value 'return state) 'null)) (return (cond
                                                           [(eq? (m_value 'return state) #t) 'true]
                                                           [(eq? (m_value 'return state) #f) 'false]
                                                           [else (m_value 'return state)]))] ; check if there is something to return
      [(null? parse_tree) state] ;; Code may have been run with no return, will just return state
      [else (run_code (cdr parse_tree) (run_line (car parse_tree) state return break continue throw) return break continue throw)])))


;; run_line will run a single line of code within the parse tree
;;  returns a state
;; Example: (run_line '((if (< x 10) (return 1) (return 2))) '((x)(1)) (lambda (v) v))
;; Example: (run_line '(var x) '((return) (null)) (lambda (v) v)) => '((x return) (null null))
;; Example: (run_line '(= x 4) '((x return) (null null)) (lambda (v) v)) => '((x return) (4 null))
(define run_line
  (lambda (expr m_state return break continue throw)
    (cond
      [(null? expr) m_state]
      [(eq? (get_op expr) 'var) (if (pair? (cddr expr))
                                    (m_assign (arg1 expr) (m_eval (arg2 expr) m_state) m_state)
                                    (m_declare (arg1 expr) m_state))]
      [(eq? (get_op expr) '=) (m_initialize (arg1 expr) (m_eval (arg2 expr) m_state) m_state)]
      [(eq? (get_op expr) 'return) (m_return (arg1 expr) m_state return)] ; check if return is in try (must run finally)
      [(eq? (get_op expr) 'if) (if (pair? (cdddr expr))
                                   (m_if_else (cadr expr) (arg2 expr) (arg3 expr) m_state return break continue throw)
                                   (m_if (arg1 expr) (arg2 expr) m_state return break continue throw))]
      [(eq? (get_op expr) 'while)   (removeBreakLayer (call/cc
                                                       (lambda (k)
                                                         (m_while (arg1 expr) (arg2 expr) (loop_state m_state) return k continue throw))))]
      [(eq? (get_op expr) 'begin)   (removeLayer
                                     (call/cc
                                      (lambda (k)
                                        (run_code (cdr expr) (addLayer m_state) return break k throw))))]
      [(eq? (get_op expr) 'try)   (removeLayer (m_finally
                                                (arg3 expr)
                                                (addLayer (removeThrowLayer
                                                             (m_try
                                                              (arg1 expr)
                                                              (arg2 expr)
                                                              (try_state m_state)
                                                              return break continue throw)))
                                                return break continue throw))] ;CHANGE
      [(eq? (get_op expr) 'break)   (break m_state)] ; check if break is in try (must run finally)
      [(eq? (get_op expr) 'continue)   (continue m_state)]
      [(eq? (get_op expr) 'throw)   (throw (cons (m_eval (arg1 expr) m_state) (list m_state)))]))) ; check if throw is legal

;; Try block
(define m_try
  (lambda (trybody catchblock m_state return break continue throw)
    (cond
      [(atom? (car (call/cc (lambda (k) (run_code trybody m_state return break continue k))))) ; we got a value that was thrown
       (if (null? catchblock)
           (error 'throwWithoutCatch "Threw a value without a catch")
           (removeLayer (m_catch
                         (caadr catchblock) ; the name of thrown value
                         (car (call/cc (lambda (k) (run_code trybody m_state return break continue k)))) ; the value of thrown value
                         (caddr catchblock)
                         (addLayer (cadr (call/cc (lambda (k) (run_code trybody m_state return break continue k))))) ; the state remaining
                         return break continue throw)))] ; we got a state back
      [else
       (removeLayer (run_code trybody (addLayer m_state) return break continue throw))]))) ; we got a state

;; Catch block
(define m_catch
  (lambda (e_name e_val catchbody m_state return break continue throw)
    (cond
      [(null? catchbody) m_state]
      [else (run_code catchbody (m_assign e_name e_val m_state) return break continue throw)]))) ; add name with value of throw to the m_state

;; Finally block
(define m_finally
  (lambda (finallyblock m_state return break continue throw)
    (cond
      [(null? finallyblock) m_state]
      [else (run_code (cadr finallyblock) m_state return break continue throw)])))

;; Checks input condition
;; If the condition is true, recurse the while loop on the state returned after
;;  executing the expression
;; Otherwise return the current m_state
(define m_while
  (lambda (condition expr m_state return break continue throw)
    (if (eq? (m_bool condition m_state return) #t)
        (m_while condition expr (run_line expr m_state return break continue throw) return break continue throw) ; assume no side effects
        m_state)))

;; Checks the input condition
;; Returns the state of the expression within the body
;; Otherwise return the current m_state
(define m_if
  (lambda (condition expr1 m_state return break continue throw)
    (if (eq? (m_bool condition m_state return) #t)
        (run_line expr1 m_state return break continue throw) ; assume no side effects
        m_state)))


;; Checks the input condition
;; If the condition is true, return the state given by the expression
;; Otherwise returns the m_state of the expression within the else condition
(define m_if_else
  (lambda (condition expr1 expr2 m_state return break continue throw)
    (if (eq? (m_bool condition m_state return) #t)
        (run_line expr1 m_state return break continue throw) ; assume no side effects
        (run_line expr2 m_state return break continue throw))))

;; Sets the return variable in m_state to the result of expr
(define m_return
  (lambda (expr m_state return)
    (m_initialize 'return (m_eval expr m_state) m_state)))

;;;; **********************************************************
;;;;
;;;; Main assignment, declaration, initialization, and value-
;;;; retreival functions that use abstracted state functions
;;;;
;;;; **********************************************************

;; m_assign declares the variable its passed and assigns it to the given value
;; Example: (m_assign 'a '(()())) => '((a)('null))
;; Example; (m_assign 'x '((a b c) (5 6 10))) => '((x a b c) (null 5 6 10))
(define m_assign
  (lambda (var val m_state)
    (cond
      [(null? var)
            (error 'null_variable "Tried to declare a null variable")]
      [(s_member var m_state)
            (error 'declared_variable "Tried to redefine an already declared variable")]
      (else (s_assignvalue var val m_state)))))

;; m_declare declares the variable its passed
;; Example: (m_declare 'a '(()())) => '((a)('null))
;; Example; (m_declare 'x '((a b c) (5 6 10))) => '((x a b c) (null 5 6 10))
(define m_declare
  (lambda (var m_state)
    (cond
      [(null? var)
            (error 'null_variable "Tried to declare a null variable")]
      [(s_member var m_state)
            (error 'declared_variable "Tried to declare an already declared variable")]
      (else (s_declare var m_state)))))

;; Takes a variable, value and m_state and returns the m_state with the initialiazed variable
;; Example: (m_initialize 'x 14 '((x y z a) (null 2 3 4))) => ((x y z a) (14 2 3 4))
(define m_initialize
  (lambda (var val m_state)
    (cond
      [(null? m_state)
            (error 'null_state "m_state is null")]
      [(s_member var m_state)
            (s_initvalue var val m_state)]
      (else (error 'assignment_error "The variable hasn't been declared (initialization before declaration)")))))

;; It takes a parameter of a variable and the m_state and
;;  returns a state
;; Example: (m_value 'x '((a b c x) (1 2 3 4))) returns 4
(define m_value
  (lambda (var m_state)
    (s_value var m_state)))

;;;; **********************************************************
;;;;
;;;; Abstracted helper functions for the m_state
;;;;
;;;; **********************************************************

;; A helper function that adds a new layer to the state
(define addLayer
  (lambda (m_state)
    (if (null? m_state)
      '(()())
      (cons '(()()) (list m_state)))))

(define removeLayer
  (lambda (m_state)
    (if (null? m_state)
        '(()())
        (cadr m_state))))

(define loop_state
  (lambda (m_state)
    (s_declare 'loop--state m_state)))

(define removeBreakLayer
  (lambda (m_state)
    (cond
      [(s_member_layer 'loop--state m_state) (s_super_destroy 'loop--state m_state)]
      [else (removeBreakLayer (cadr m_state))])))

(define try_state
  (lambda (m_state)
    (s_declare 'try--state m_state)))

(define removeThrowLayer
  (lambda (m_state)
    (cond
      [(s_member_layer 'try--state m_state) (s_super_destroy 'try--state m_state)]
      [else (removeThrowLayer (cadr m_state))])))


;;Helper function that returns true if there exists a sublayer false if there isn't a layer 
(define s_layer
  (lambda (m_state)
    (cond
      [(list? (car m_state))
       (if (null? (car m_state))
           #f
           (if (list? (caar m_state))
               #t
               #f))]
      [else
       #f])))

;; A helper function that just returns whether a variable is in the list
(define s_member_layer
  (lambda (a m_state)
    (cond
      [(s_layer m_state)      (s_member a (car m_state)) ]
      [(null? m_state)        #f]
      [(null? (car m_state))  #f]
      [(eq? a (caar m_state)) #t]
      (else                   (s_member a (cons (cdar m_state) (cdr m_state)))))))
(define s_member
  (lambda (a m_state)
    (cond
      [(s_layer m_state)      (or (s_member a (car m_state))(s_member a (cadr m_state))) ]
      [(null? m_state)        #f]
      [(null? (car m_state))  #f]
      [(eq? a (caar m_state)) #t]
      (else                   (s_member a (cons (cdar m_state) (cdr m_state)))))))

;; It takes a parameter of a variable and the m_state and
;;  returns a state
;; Example: (m_value 'x '((a b c x) (1 2 3 4))) returns 4
(define s_value
  (lambda (var m_state)
    (cond
      [(s_layer m_state)
       (if (s_member_layer var (car m_state))
           (s_value var (car m_state))
           (s_value var (cadr m_state)))
           ]
      [(and (null? (car  m_state)) (pair? (cdr m_state)))
       (error 'undefined_var "The variable hasn't been declared (using before declaring)")]
      [(and (not (eq? var 'return)) (eq? var (caar m_state)) (eq? (caadr m_state) 'null))
       (error 'undefined_var "The variable hasn't been initialized (use before assignment)")]
      [(eq? var (caar m_state)) (caar (cdr m_state))]
      (else
       (s_value var (cons (cdar m_state) (list (cdadr m_state))))))))

;; Sets the value of the already declared variable in the state
(define s_initvalue
  (lambda (var val m_state)
    (if (s_layer m_state)
        (if (s_member_layer var (car m_state))
            (cons (s_initvalue var val (car m_state)) (cdr m_state))
            (cons (car m_state) (list (s_initvalue var val (cadr m_state)))))
        (if (eq? var (caar m_state))
            (cons (car m_state) (list (cons val (cdadr m_state))))
            (s_initvalue (caar m_state) (caadr m_state) (s_declare (caar m_state) (s_initvalue var val (cons (cdar m_state) (list (cdadr m_state))))))))))

;; Declares and initializes variable to the value input
(define s_assignvalue
  (lambda (var val m_state)
    (if (s_layer m_state)
        (cons (s_assignvalue var val (car m_state)) (cdr m_state))
        (cons (cons var (car m_state)) (list (cons val (cadr m_state)))))))

;; Finds the index of the given variable in the m_state
(define s_index
  (lambda (var m_state)
    (cond
      [(null? (car m_state)) (error 'varNotFoundError "Variable does not exist or is not in scope")]
      [(s_layer m_state) (s_index var (car m_state))]
      [(eq? var (caar m_state)) 0]
      [else (+ 1 (s_index var (cons (cdar m_state) (list (cdadr m_state)))))])))        


;; Deletes the variable from the m_state
(define s_super_destroy
  (lambda (var m_state)
    (cond
      [(s_layer m_state) (cons (s_super_destroy var (car m_state)) (cdr m_state))]
      [else (s_destroy (s_index var m_state) m_state)])))

(define s_destroy
  (lambda (index m_state)
    (cond
      [(zero? index) (cons (cdar m_state) (list (cdadr m_state)))]
      [else (s_assignvalue (caar m_state) (caadr m_state) (s_destroy (- index 1) (cons (cdar m_state) (list (cdadr m_state)))))])))
      

;; Declares a variable and sets its initial value to 'null
(define s_declare
  (lambda (var m_state)
    (if (s_layer m_state)
        (cons (s_declare var (car m_state)) (cdr m_state))
        (cons (cons var (car m_state)) (list (cons 'null (cadr m_state)))))))
;;
(define s_remove_front
  (lambda (m_state)
    (if (s_layer m_state)
        (cons (s_remove_front (car m_state)) (cdr m_state))
        (cons (cdar m_state) (list (cdadr m_state))))))

;(define s_remove_value
;  (lambda (m_state var)
 ;   (cond
  ;    [(if (s_layer m_state)
   ;        (if (s_member_layer  var m_state)
 ;              ))])))

;;;; **********************************************************
;;;;
;;;; Main evaluate and boolean functions
;;;;
;;;; **********************************************************

;; m_eval takes a mathematical expression and evaluates it
;; returns the output value
;; Example: (m_eval '(+ (* (+ 4 5) 3) 3) '(()())) => 30
;; Example: (m_eval '(+ (* (+ x y) 5) z) '((x y z)(1 2 3))) => 18
(define m_eval
  (lambda (exp m_state)
    (cond
      [(null? exp)
            (error 'null_expression "Expression is null")]
      [(number? exp)
            exp]
      [(and (symbol? exp) (eq? exp 'true))
            #t]
      [(and (symbol? exp) (eq? exp 'false))
            #f]
      [(symbol? exp)
            (m_value exp m_state)]
      [(eq? (get_op exp) '+)
            (+ (m_eval (left_operand exp) m_state) (m_eval (right_operand exp) m_state))]
      [(eq? (get_op exp) '-)
       (if (pair? (cddr exp))
            (- (m_eval (left_operand exp) m_state) (m_eval (right_operand exp) m_state))
            (* -1 (m_eval (left_operand exp) m_state)))] ; must check for unary -
      [(eq? (get_op exp) '/)
            (quotient (m_eval (left_operand exp) m_state) (m_eval (right_operand exp) m_state))]
      [(eq? (get_op exp) '*)
            (* (m_eval (left_operand exp) m_state) (m_eval (right_operand exp) m_state))]
      [(eq? (get_op exp) '%)
       (remainder (m_eval (left_operand exp) m_state) (m_eval (right_operand exp) m_state))]
      [else (m_bool exp m_state (lambda (v) v))])))


;; m_bool takes a condition and evaluates it
;; returns either true or false
;; Example: (m_bool '(> (* x (+ x x)) y) '((x y)(2 7)) (lambda (v) v)) => #t
;; Example: (m_bool '(> (* x (+ x x)) y) '((x y)(2 9)) (lambda (v) v)) => #f
(define m_bool
  (lambda (condition m_state return)
    (cond
      [(null? condition)
            (error 'invalid "expression is empty")]
      [(and (symbol? condition) (eq? condition 'true))
            #t]
      [(and (symbol? condition) (eq? condition 'false))
            #f]
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
      [(check_operator condition) (m_eval condition m_state)]
      [else (error 'Invalid_eval "The condition contains an illegal comparison or operator")])))

;; Checks if the operator is a mathematical operator
(define check_operator
  (lambda (exp)
    (cond
      [(eq? (get_op exp) '+) #t]
      [(eq? (get_op exp) '-) #t]
      [(eq? (get_op exp) '*) #t]
      [(eq? (get_op exp) '/) #t]
      [(eq? (get_op exp) '%) #t]
      (else #f))))


;; Abstractions for m_eval and m_bool
(define get_op car)
(define arg1 cadr)
(define arg2 caddr)
(define arg3 cadddr)
(define left_operand cadr)
(define right_operand caddr)
(define (atom? x) (not (or (pair? x) (null? x))))