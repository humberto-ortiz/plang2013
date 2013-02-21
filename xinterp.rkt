#lang plai-typed
;; login : <YOUR-PIN-HERE>

(define-type Binding
  [binding (name : symbol) (named-expr : CFWAE)])

(define-type CFWAE
  [num (n : number)]
  [binop (op : symbol) (lhs : CFWAE) (rhs : CFWAE)]
  [with (lob : (listof Binding)) (body : CFWAE)]
  [id (name : symbol)]
  [if0 (c : CFWAE) (t : CFWAE) (e : CFWAE)]
  [fun (args : (listof symbol)) (body : CFWAE)]
  [app (f : CFWAE) (args : (listof CFWAE))])

(define-type Env
  [mtEnv]
  [anEnv (name : symbol) (value : CFWAE-Value) (env : Env)])

(define-type CFWAE-Value
  [numV (n : number)]
  [closureV (params : (listof symbol))
            (body : CFWAE)
            (env : Env)])

(define (parse-one-binding [sexp : s-expression]) : Binding
  (binding (s-exp->symbol (first (s-exp->list sexp)))
           (parse (second (s-exp->list sexp)))))

(define (parse-all-bindings [ls : (listof s-expression)]) : (listof Binding)
  (if (empty? ls) empty
      (cons (parse-one-binding (first ls)) (parse-all-bindings (rest ls)))))

; parse : expression -> CFWAE
; This procedure parses an expression into a CFWAE
(define (parse [sexp : s-expression]) : CFWAE
  (cond
    [(s-exp-number? sexp) (num (s-exp->number sexp))]
    [(s-exp-list? sexp)
     (let ([sl (s-exp->list sexp)])
       (case (s-exp->symbol (first sl))
         [(+ - * /) (binop (s-exp->symbol (first sl)) (parse (second sl)) (parse (third sl)))]
         
         ;; add conditional
         [(if0) (if0 (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [else (error 'parse "invalid list input")]))]))

;; interp : CFWAE -> CFWAE-Value
;; This procedure evaluates a CFWAE expression, producing a CFWAE-Value.
(define (interp [expr : CFWAE]) : CFWAE-Value
  (numV 0))

(test (parse '3) (num 3))
(test (parse '{+ 1 2}) (binop '+ (num 1) (num 2)))

; A ver si joseph tiene razon
(test (parse-one-binding '{x 1}) (binding 'x (num 1)))
(test (parse-all-bindings (list '{x 1} '{y 2})) (list (binding 'x (num 1))
                                                (binding 'y (num 2))))