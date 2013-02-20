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

(define keywords (list '+ '- '* '/ 'with 'if0 'fun))

;; parse : expression -> CFWAE
; This procedure parses an expression into a CFWAE
(define (parse [sexp : s-expression]) : CFWAE
  (cond
    [(s-exp-number? sexp) (num (s-exp->number sexp))]
    [(s-exp-symbol? sexp) (id (s-exp->symbol sexp))]
    [(and (s-exp-list? sexp) (< 0 (length (s-exp->list sexp))))
     (let ([sl (s-exp->list sexp)])
       (cond
         [(s-exp-list? (first sl)) (app (parse (first sl)) (make-actuals (rest sl)))]
         [else
          (case (s-exp->symbol (first sl))
            [(+ - * /) 
             (cond
               [(not (= 3 (length sl))) (error 'parse "binary operations must have exactly two arguments")]
               [else (binop (s-exp->symbol (first sl)) (parse (second sl)) (parse (third sl)))])]
            ;; add conditional
            [(if0) 
             (cond
               [(= 4 (length sl)) (if0 (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
               [else (error 'parse "if0 requires 3 arguments")])]
            ;; with takes a list of id, value pairs
            [(with) 
             (cond
               [(not (= 3 (length sl))) (error 'parse "error parsing 'with' expression")]
               [else (with (make-lob (second sl) keywords) (parse (third sl)))])]
            [(fun) 
             (cond
               [(not (= 3 (length sl))) (error 'parse "error parsing fun")]
               [else (fun (make-formals (s-exp->list (second sl)) keywords) (parse (third sl)))])]
            [else (app (parse (first sl)) (make-actuals (rest sl)))])]))]
    [else (error 'parse "can't parse sexp")]))


(define (make-lob [sexp : s-expression] [ids : (listof symbol)]) : (listof Binding)
  (local [(define (make-binding [sexp : s-expression]) : Binding
            (let* ([los (s-exp->list sexp)]
                   [id (s-exp->symbol (first los))]
                   [expr (parse (second los))])
              (cond
                [(member id ids) (error 'parse "identifiers in with must be unique")]
                [else (begin
                        (set! ids (cons id ids))
                        (binding id expr))])))]
  (map make-binding (s-exp->list sexp))))

(define (make-formals [los : (listof s-expression)] [ids : (listof symbol)]) : (listof symbol)
  (cond
      [(empty? los) empty]
      [else (let ([id (s-exp->symbol (first los))])
              (cond
                [(member id ids) (error 'parse "formal arguments to fun must be unique")]
                [else (cons id (make-formals (rest los) (cons id ids)))]))]))

(define (make-actuals [actuals : (listof s-expression)]) : (listof CFWAE)
  (map parse actuals))

;; interp : CFWAE -> CFWAE-Value
;; This procedure evaluates a CFWAE expression, producing a CFWAE-Value.
(define (interp [expr : CFWAE]) : CFWAE-Value
  (local [(define (interp-in [expr : CFWAE] [env : Env]) : CFWAE-Value
            (type-case CFWAE expr
              [num (n) (numV n)]
              [binop (op lhs rhs) (do-binop op (interp-in lhs env) (interp-in rhs env))]
              [with (lob body) (interp-in body (bind-all lob env))]
              [id (n) (lookup n env)]
              [if0 (test e1 e2) (if (= 0 (numV-n (interp-in test env))) (interp-in e2 env) (interp-in e1 env))]
              [fun (formals body) (closureV formals body env)]
              [app (f actuals)
                   (let* ([fval (interp-in f env)]
                         [new-lob (zip (closureV-params fval) actuals)])
                     (interp-in (closureV-body fval) (bind-all new-lob (merge-env (closureV-env fval) env))))]))
          (define (do-binop op lhs rhs)
            (case op
              [(+) (numV (+ (numV-n lhs) (numV-n rhs)))]
              [(-) (numV (- (numV-n lhs) (numV-n rhs)))]
              [(*) (numV (* (numV-n lhs) (numV-n rhs)))]
              ;; check for zero
              [(/) 
               (cond
                 [(= 0 (numV-n rhs)) (error 'interp "division by zero")]
                 [else (numV (- (numV-n lhs) (numV-n rhs)))])]))
          (define (bind-all lob env)
            (cond
              [(empty? lob) env]
              [else (bind-all (rest lob) (anEnv (binding-name (first lob)) (interp-in (binding-named-expr (first lob)) env) env))]))]
    (interp-in expr (mtEnv))))

(define (zip [formals : (listof symbol)] [actuals : (listof CFWAE)]) : (listof Binding)
  (cond
    [(empty? formals) (if (empty? actuals) empty (error 'interp "arity error"))]
    [else (cons (binding (first formals) (first actuals)) (zip (rest formals) (rest actuals)))]))

(define (lookup [n : symbol] [env : Env]) : CFWAE-Value
  (type-case Env env
    [mtEnv () (error 'lookup "reference to undefined identifier")]
    [anEnv (name value remaining) 
           (cond
             [(equal? n name) value]
             [else (lookup n remaining)])]))

(define (merge-env env1 env2)
  (type-case Env env1
    [mtEnv () env2]
    [anEnv (name value remaining) (merge-env remaining (anEnv name value env2))]))

;;;; testing

(print-only-errors true)
(test (parse '3) (num 3))
(test (interp (parse '3)) (numV 3))

(test (parse '(+ 1 2)) (binop '+ (num 1) (num 2)))
(test (parse '(+ 1 (* 2 3))) (binop '+ (num 1) (binop '* (num 2) (num 3))))
(test (interp (parse '{+ 2 3})) (numV 5))
(test (interp (parse '(+ 1 (* 2 3)))) (numV 7))

(test (parse '{with {{x 2}
       {y 3}}
  {with {{z {+ x y}}}
    {+ x z}}})
      (with (list (binding 'x (num 2)) (binding 'y (num 3))) (with (list (binding 'z (binop '+ (id 'x) (id 'y)))) (binop '+ (id 'x) (id 'z)))))

(test (interp (parse '{with {{x 2}
       {y 3}}
  {with {{z {+ x y}}}
    {+ x z}}})) (numV 7))

(test (interp (parse '{if0 0 1 2})) (numV 2))
(test (interp (parse '{if0 1 2 3})) (numV 2))

(test (parse '{{fun {x y} {* x y}} 2 3}) (app (fun (list 'x 'y) (binop '* (id 'x) (id 'y))) (list (num 2) (num 3))))
(test (interp (parse '{{fun {x y} {* x y}} 2 3})) (numV 6))

;; stuff from report
(test/exn (parse '{}) "can't parse sexp")
(test/exn (parse '{if0 5}) "if0")
(test/exn (parse '{with {{x 1} {x 2}} x}) "")
(test/exn (parse '{fun {x x} x}) "")
(test (interp (parse '(with ((f (fun (x y) (+ x y)))) (f (f 1 2) 3)))) (numV 6))
;(test/exn (parse '(with ((x with)) 5)) "")
