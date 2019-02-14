#lang racket
(require "simpleParser.rkt")

(define m_bool
  (lambda (expr state return)
    (cond
      [(null? expr) state])))