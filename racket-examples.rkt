#lang racket
;;;; my-reduce - implementation of reduce applies a function to consecutive elements of
;;;; a list, collecting the result into a single value
;;;; Copyright 2012 - Humberto Ortiz-Zuazaga <humberto.ortiz@upr.edu>

;; Contract: my-reduce : function value list -> value
;; Purpose: apply FN to START and all the elements in LST reducing to a single value
;; Example: (my-reduce + 0 '(1 2 3)) should produce 6
(define (my-reduce fn start lst)
  (cond ([empty? lst] start)
        (else (my-reduce fn (fn start (first lst)) (rest lst)))))

;;;; examples
(my-reduce + 0 '(1 2 3))

(my-reduce * 1 '(1 2 3 4 5))
