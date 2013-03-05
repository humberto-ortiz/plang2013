#lang plai-typed
;;;; Interpreter for simple arithmetic expressions with functions and conditionals.
;;;; This interpeter uses environments to map identifiers to values and allows functions anywhere.
;;;; Use chapter 8 to add mutable structures

;;;; Copyright 2013 - Humberto Ortiz-Zuazaga <humberto.ortiz@upr.edu>
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


;;;; from 8.2.3 Interpreting Variables
(define-type ExprC
  [numC (n : number)]
  [varC (s : symbol)]
  [appC (fun : ExprC) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [lamC (arg : symbol) (body : ExprC)]
  [if0C (test : ExprC) (then : ExprC) (else : ExprC)]
  [setC (var : symbol) (arg : ExprC)]
  [seqC (b1 : ExprC) (b2 : ExprC)])

;; no more boxes
(define-type Value
  [numV (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)])

;;;; Defining the store, maps locations to values
;;;; http://cs.brown.edu/courses/cs173/2012/book/mut-struct-vs-var.html#(part._.Introducing_the_.Store)

(define-type-alias Location number)

;; environment now binds names to locations
(define-type Binding
  [bind (name : symbol) (place : Location)])
 
(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)
 
(define-type Storage
  [cell (location : Location) (val : Value)])
 
(define-type-alias Store (listof Storage))
(define mt-store empty)
(define override-store cons)

(define (lookup [for : symbol] [env : Env]) : Location
  (cond
    [(empty? env) (error 'lookup "reference to unbound identifier")]
    [(cons? env) (cond
                   [(equal? for (bind-name (first env))) (bind-place (first env))]
                   [else (lookup for (rest env))])]))

(define (fetch [loc : Location] [sto : Store]) : Value
  (cond
    [(empty? sto) (error 'fetch "reference to unallocated location")]
    [(cons? sto) (cond
                   [(equal? loc (cell-location (first sto))) (cell-val (first sto))]
                   [else (fetch loc (rest sto))])]))

(define-type Result
  [v*s (v : Value) (s : Store)])

;;;; 8.1.3
(define new-loc
  (let ([n (box 0)])
    (lambda ()
      (begin
        (set-box! n (add1 (unbox n)))
        (unbox n)))))

;; Interpreter that can handle functions
;; http://cs.brown.edu/courses/cs173/2012/book/adding-functions.html#%28part._.Growing_the_.Interpreter%29
(define (interp [expr : ExprC] [env : Env] [sto : Store]) : Result
  (type-case ExprC expr
    [numC (n) (v*s (numV n) sto)]
    [varC (n) (v*s (fetch (lookup n env) sto) sto)]
    [lamC (a b) (v*s (closV a b env) sto)]
    [appC (f a) (type-case Result (interp f env sto)
                  [v*s (f-v f-s) (type-case Result (interp a env f-s)
                                   [v*s (a-v a-s) 
                                        (let ((where (new-loc)))
                                          (interp (closV-body f-v)
                                                  (extend-env (bind (closV-arg f-v)
                                                                    where)
                                                              (closV-env f-v))
                                                  (override-store (cell where a-v)
                                                                  a-s)))])])]
                         

    [plusC (l r) (type-case Result (interp l env sto) 
                   [v*s (v-l s-l) (type-case Result (interp r env s-l)
                                    [v*s (v-r s-r)
                                         (v*s (num+ v-l v-r) s-r)])])]
    
    ;;[multC (l r) (num* (interp l env) (interp r env))]
    [multC (l r) (type-case Result (interp l env sto) 
                   [v*s (v-l s-l) (type-case Result (interp r env s-l)
                                    [v*s (v-r s-r)
                                         (v*s (num* v-l v-r) s-r)])])]
    
    ;[if0C (t e1 e2) (if (= 0 (numV-n (interp t env)))
    ;                    (interp e1 env)
    ;                    (interp e2 env))]
    [if0C (t e1 e2) (type-case Result (interp t env sto)
                      [v*s (v-t s-t) (if (= 0 (numV-n v-t))
                                         (type-case Result (interp e2 env s-t)
                                           [v*s (e2-v e2-s) (v*s e2-v e2-s)])
                                         (type-case Result (interp e1 env s-t)
                                           [v*s (e1-v e1-s) (v*s e1-v e1-s)]))])]

    ;;;; http://cs.brown.edu/courses/csci1730/2012/book/mut-struct-vs-var.html#(elem._(chunk._~3cset.C-case~3e~3a1))
    [setC (var val) (type-case Result (interp val env sto)
                     [v*s (v-val s-val) 
                          (let ([where (lookup var env)])
                            (v*s v-val
                                 (override-store (cell where v-val)
                                                 s-val)))])]
                                           
    
    [seqC (b1 b2) (let ((vs (interp b1 env sto)))
                    (interp b2 env (v*s-s vs)))]
    
    ;[else (error 'interp "can't grok")]
    ))

(define (num+ l r)
  (cond ((and (numV? l) (numV? r)) (numV (+ (numV-n l) (numV-n r))))
        [else (error 'num+ "one argument was not a number")]))

(define (num* l r)
  (cond ((and (numV? l) (numV? r)) (numV (* (numV-n l) (numV-n r))))
        [else (error 'num* "one argument was not a number")]))

(define (run expr)
  (v*s-v (interp expr mt-env mt-store)))

(test (run (plusC (numC 10) (appC (lamC '_ (numC 5)) (numC 10)))
              )
      (numV 15))

;; hey! this works now.
(test (run (appC (lamC 'x (appC (lamC 'y (plusC (varC 'x) (varC 'y)))
                                          (numC 4)))
                        (numC 3))
                  )
          (numV 7))

;;;; Our first try fails to capture closure.
(test (run     (appC (appC (lamC 'x
                     (lamC 'y
                          (plusC (varC 'x) (varC 'y))))
                (numC 4))
          (numC 5)) ) 
      (numV 9))

;;; Variables aren't boxes.

(test (run (appC (lamC 'x (varC 'x))
           (numC 3))) (numV 3))

(test (run (appC (lamC 'x (seqC (setC 'x (plusC (varC 'x)
                                                (numC 1)))
                                (varC 'x)))
                 (numC 3))) (numV 4))
