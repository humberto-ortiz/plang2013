#lang plai-typed
;;;; plai-examples.rkt - introductory examples of the plai-typed language
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

;; Contract: my-append: ((listof 'a) (listof 'a) -> (listof 'a))
;; Purpose: concatenate LST1 and LST2
;; Example: (my-append (list 1) (list 2 3)) -> '(1 2 3)

(define (my-append [lst1 : (listof 'a)] [lst2 : (listof 'a)]) : (listof 'a) 
  (local
    [(define (iter lst1 lst2 result)
    (cond ((not [empty? lst1]) (iter (rest lst1) lst2 (cons (first lst1) result)))
          ((not [empty? lst2]) (iter lst1 (rest lst2) (cons (first lst2) result)))
          (else (reverse result))))]
  (iter lst1 lst2 empty)))

(test (my-append empty empty) empty)

(test (my-append (list 1) empty) (list 1))

(test (my-append empty (list 2)) (list 2))

(test (my-append (list 1) (list 2 3)) (list 1 2 3))
