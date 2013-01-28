#lang plai-typed
;;;; parser for arithmetic expressions in s-expressions

;;;; Chapter 2.4
;;;; http://cs.brown.edu/courses/cs173/2012/book/Everything__We_Will_Say__About_Parsing.html#%28part._first-parser%29

(define-type ArithC
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)])

(define (parse [s : s-expression]) : ArithC
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusC (parse (second sl)) (parse (third sl)))]
         [(*) (multC (parse (second sl)) (parse (third sl)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))

;;;; Chapter 3.2
;;;; http://cs.brown.edu/courses/cs173/2012/book/first-interp.html#%28part._.Writing_an_.Interpreter%29

(define (interp [a : ArithC]) : number
  (type-case ArithC a
    [numC (n) n]
    [plusC (l r) (+ (interp l) (interp r))]
    [multC (l r) (* (interp l) (interp r))]))

(test (interp (parse '3)) 3)
(test (interp (parse '(+ (* 1 2) (+ 2 3)))) 7)