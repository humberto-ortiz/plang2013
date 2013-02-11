#lang plai-typed
;;;; Interpreter for simple arithmetic expressions with functions and conditionals.
;;;; This interpeter uses environments to map identifiers to values.

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

;; http://cs.brown.edu/courses/cs173/2012/book/adding-functions.html#%28elem._%28chunk._~3cfundef~3e~3a1%29%29
(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

;; http://cs.brown.edu/courses/cs173/2012/book/adding-functions.html#%28elem._%28chunk._~3cexpr.C~3e~3a1%29%29
(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : symbol) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [if0C (test : ExprC) (e1 : ExprC) (e2 : ExprC)])

;;;; Section 6.1
;;;; binding identifiers to values
(define-type Binding
  [bind (name : symbol) (val : number)])
 
(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

;; Interpreter that can handle functions
;; http://cs.brown.edu/courses/cs173/2012/book/adding-functions.html#%28part._.Growing_the_.Interpreter%29
(define (interp [expr : ExprC] [env : Env] [fds : (listof FunDefC)]) : number
  (type-case ExprC expr
    [numC (n) n]
    [idC (n) (lookup n env)]
    [appC (f a) (local ([define fd (get-fundef f fds)])
                  (interp (fdC-body fd)
                          (extend-env (bind (fdC-arg fd)
                                            (interp a env fds))
                                      env)
                          fds))]
    [plusC (l r) (+ (interp l env fds) (interp r env fds))]
    [multC (l r) (* (interp l env fds) (interp r env fds))]
    [if0C (t e1 e2) (if (= 0 (interp t env fds))
                        (interp e1 env fds)
                        (interp e2 env fds))]))

;; lookup a name in an environment
(define (lookup [id : symbol] [env : Env]) : number
  (cond
    [(empty? env) (error 'lookup "reference to undefined identifier")]
    [(cons? env) (cond
                   [(equal? id (bind-name (first env))) (bind-val (first env))]
                   [else (lookup id (rest env))])]))

;; helper function
(define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-fundef "reference to undefined function")]
    [(cons? fds) (cond
                   [(equal? n (fdC-name (first fds))) (first fds)]
                   [else (get-fundef n (rest fds))])]))

(test (interp (appC 'const5 (numC 1)) mt-env (list (fdC 'const5 '_ (numC 5)))) 5)

(test (interp (appC 'double (numC 3)) mt-env (list (fdC 'double 'x (plusC (idC 'x) (idC 'x))))) 6)

(test (interp (appC 'cube (numC 3)) mt-env (list (fdC 'cube 'x (multC (idC 'x) (multC (idC 'x) (idC 'x)))))) 27)

;;;; check dynamic scope
(test/exn (interp (appC 'f1 (numC 3))
                  mt-env
                  (list (fdC 'f1 'x (appC 'f2 (numC 4)))
                        (fdC 'f2 'y (plusC (idC 'x) (idC 'y))))) "undefined identifier")
