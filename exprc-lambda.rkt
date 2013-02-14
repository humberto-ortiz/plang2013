#lang plai-typed
;;;; Interpreter for simple arithmetic expressions with functions and conditionals.
;;;; This interpeter uses environments to map identifiers to values and allows functions anywhere.

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


;; http://cs.brown.edu/courses/cs173/2012/book/higher-order-functions.html#%28part._.Functions_as_.Expressions_and_.Values%29
(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : ExprC) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [if0C (test : ExprC) (e1 : ExprC) (e2 : ExprC)]
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

;;;; Section 6.1
;;;; binding identifiers to values
(define-type Binding
  [bind (name : symbol) (val : Value)])
 
(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(define-type Value
  [numV (n : number)]
  [funV (name : symbol) (arg : symbol) (body : ExprC)])

;; Interpreter that can handle functions
;; http://cs.brown.edu/courses/cs173/2012/book/adding-functions.html#%28part._.Growing_the_.Interpreter%29
(define (interp [expr : ExprC] [env : Env]) : Value
  (type-case ExprC expr
    [numC (n) (numV n)]
    [idC (n) (lookup n env)]
    [fdC (n a b) (funV n a b)]
    [appC (f a) (local ([define fd (interp f env)])
              (interp (funV-body fd)
                      (extend-env (bind (funV-arg fd)
                                        (interp a env))
                                  mt-env)))]
                         

[plusC (l r) (num+ (interp l env) (interp r env))]
    [multC (l r) (num* (interp l env) (interp r env))]
    [if0C (t e1 e2) (if (= 0 (numV-n (interp t env)))
                        (interp e1 env)
                        (interp e2 env))]))

;; lookup a name in an environment
(define (lookup [id : symbol] [env : Env]) : Value
  (cond
    [(empty? env) (error 'lookup "reference to undefined identifier")]
    [(cons? env) (cond
                   [(equal? id (bind-name (first env))) (bind-val (first env))]
                   [else (lookup id (rest env))])]))

(define (num+ l r)
  (cond ((and (numV? l) (numV? r)) (numV (+ (numV-n l) (numV-n r))))
        [else (error 'num+ "one argument was not a number")]))

(define (num* l r)
  (cond ((and (numV? l) (numV? r)) (numV (* (numV-n l) (numV-n r))))
        [else (error 'num* "one argument was not a number")]))

(test (interp (plusC (numC 10) (appC (fdC 'const5 '_ (numC 5)) (numC 10)))
              mt-env)
      (numV 15))

(test/exn (interp (appC (fdC 'f1 'x (appC (fdC 'f2 'y (plusC (idC 'x) (idC 'y)))
                                          (numC 4)))
                        (numC 3))
                  mt-env)
          "undefined")

;;;; Our first try fails to capture closure.
(test (interp     (appC (appC (fdC 'f1 'x
                     (fdC 'f2 'y
                          (plusC (idC 'x) (idC 'y))))
                (numC 4))
          (numC 5))

mt-env) (numV 9))

;;;; these are old tests
;(test (interp (appC (fdC 'const5 '_ (numC 5))) (numC 1)) 5)

;(test (interp (appC 'double (numC 3)) mt-env (list (fdC 'double 'x (plusC (idC 'x) (idC 'x))))) 6)

;(test (interp (appC 'cube (numC 3)) mt-env (list (fdC 'cube 'x (multC (idC 'x) (multC (idC 'x) (idC 'x)))))) 27)

;;;; check dynamic scope
;(test/exn (interp (appC 'f1 (numC 3))
;                  mt-env
;                  (list (fdC 'f1 'x (appC 'f2 (numC 4)))
;                        (fdC 'f2 'y (plusC (idC 'x) (idC 'y))))) "undefined identifier")
