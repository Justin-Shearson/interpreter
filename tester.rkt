#lang racket
(require "interpreter2.rkt")

(define return (lambda (v) v))

;; MAIN FUNCTION
;; get_test_output will run the code in the test files specified and put output into a list
(define get_test_output_for_range
  (lambda (start end)
    (run_files (get_file_list start end))))


;; run_files will run each file and return the output
(define run_files
  (lambda (lis)
    (cond
      [(null? lis) '()]
      [else (cons (interpret (car lis)) (run_files (cdr lis)))])))


;; helper to get_file_list will run all files in test file range
(define get_file_list
  (lambda (start end)
    (if (= start end)
        (list (string-append "tests/" (number->string end) ".txt"))
        (cons (string-append "tests/" (number->string start) ".txt") (get_file_list (+ start 1) end)))))


;; compare output for specified tests
(define compare_for_test_range
  (lambda (start end expected)
    (compare_output_to_expected (get_test_output_for_range start end) expected)))

;; compare the output of tests to what you expect
(define compare_output_to_expected
  (lambda (output expected)
    (cond
      [(null? output) '()]
      [(eq? (car output) (car expected)) (cons 'PASS (compare_output_to_expected (cdr output) (cdr expected)))]
      [else (cons 'FAIL (compare_output_to_expected (cdr output) (cdr expected)))])))

