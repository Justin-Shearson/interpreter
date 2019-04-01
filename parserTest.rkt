#lang racket
(require "simpleParser.rkt")

(define get_tree
  (lambda (file_name)
    (return (parser file_name))))


(define return (lambda (v) v))


  