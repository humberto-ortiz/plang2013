#lang plai-typed

(require "typed-lang2.rkt")

(define-type Cell
  [cell (location : Location) (value : ValueC)])

(define-type-alias Store (listof Cell))

(define-type Result
  [v*s (value : ValueC) (store : Store)])

(define (interp-full [exprC : ExprC] [env : Env] [store : Store]) : Result
  (type-case ExprC exprC
    [NumC (n) (v*s (NumV n) store)]
    [TrueC () (v*s (TrueV) store)]
    [FalseC () (v*s (FalseV) store)]
    [StrC (s)  (v*s (StrV s) store)]
    [FuncC (Lsym mybody) (v*s (ClosureV Lsym mybody env) store)]
    
    
    [else (interp-error (string-append "Haven't covered a case yet:"
                                       (to-string exprC)))]))

(define (interp [exprC : ExprC]) : ValueC
  (type-case Result (interp-full exprC empty empty)
    [v*s (value store) value]))

(test (interp (FuncC (list 'x 'y) (StrC "Dembow"))) (ClosureV (list 'x 'y) (StrC "Dembow") empty)) 
(test (interp (FuncC empty (NumC 4))) (ClosureV empty (NumC 4) empty))  
;(interp(IfC (FalseC) (NumC 1) (StrC "hola")))
;(interp (FuncC empty (NumC 4)))