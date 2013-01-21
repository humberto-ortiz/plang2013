#lang racket
;;;; my-reduce - implementation of reduce applies a function to consecutive elements of
;;;; a list, collecting the result into a single value
;;;; Copyright 2012 - Humberto Ortiz-Zuazaga <humberto.ortiz@upr.edu>
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Contract: my-reduce : function value list -> value
;; Purpose: apply FN to START and all the elements in LST reducing to a single value
;; Example: (my-reduce + 0 '(1 2 3)) should produce 6
(define (my-reduce fn start lst)
  (cond ([empty? lst] start)
        (else (my-reduce fn (fn start (first lst)) (rest lst)))))

;;;; examples
(my-reduce + 0 '(1 2 3))
;; 6
(my-reduce * 1 '(1 2 3 4 5))
;; 120