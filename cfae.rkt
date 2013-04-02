#lang plai
;;;;
;; CFAE stuff
;;
(define-type CFAE
    (num (n number?))
    (binop (op symbol?) (lhs CFAE?) (rhs CFAE?))
    (app (fun-expr CFAE?) (arg-expr CFAE?))
    (fun (param symbol?) (body CFAE?))
    (id (name symbol?)))

(define subst
    (lambda (expr sub-id val)
        (type-case CFAE expr
            (num (x) expr)
            (binop (op l r) (binop op (subst l sub-id val) (subst r sub-id val)))
            (app (fun-expr arg-expr) expr)
            (fun (bound-id bound-body) expr)
            (id (v) (if (symbol=? v sub-id) val expr)))))

(define eval-cfae
    (lambda (a-cfae)
        (type-case CFAE a-cfae
            (num (x) x)
            (binop (op l r) ((lookup op ops) (eval-cfae l) (eval-cfae r)))
            (fun (bound-id bound-body) a-cfae)
            (app (fun-expr arg-expr) (local ((define fun-val (eval-cfae fun-expr))) (eval-cfae (subst (fun-body fun-val) (fun-param fun-val) (eval-cfae arg-expr)))))
            (id (v) (error 'eval-cfae->id (string-append "Found a free identifier (" (symbol->string v) ")"))))))

;;;;
;; Binop stuff
;;
(define-type Binop
    (bop (name symbol?) (op procedure?)))

(define lookup
    (lambda (op-name op-table)
        (cond ((empty? op-table) (error 'lookup (string-append "Operator not found: " (symbol->string op-name))))
            (else
                (if (symbol=? (bop-name (car op-table)) op-name)
                    (bop-op (car op-table))
                    (lookup op-name (cdr op-table)))))))

(define ops (list
    (bop 'add +)
    (bop 'sub -)
    (bop 'mul *)
    (bop 'div /)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;
;; Tests
;;

; returns -35
(eval-cfae (binop 'add (binop 'sub (binop 'sub (num 4) (num 3)) (num 15)) (binop 'add (binop 'add (binop 'sub (num 10) (num 5)) (binop 'sub (num 3) (num 2))) (binop 'sub (num 15) (num 42)))))

; returns 10
(eval-cfae (binop 'div (num 50) (num 5)))

; test the op-table
(lookup 'add ops)
(lookup 'sub ops)
(lookup 'mul ops)
(lookup 'div ops)

; 3
(eval-cfae (binop 'add (num 1) (num 2)))

; 4
(eval-cfae (binop 'mul (num 2) (num 2)))

; 1
;(eval-cfae (if0 (num 0) (num 1) (num 2)))

; 5
(eval-cfae (app (fun 'x (id 'x)) (num 5)))

; 2
;(eval-cfae (app (fun 'x (binop 'add (id 'x) (num 1))) (num 1)))

; 4
;(eval-cfae (if0 (app (fun 'x (binop 'sub (id 'x) (num 2))) (num 3)) (app (fun 'x (binop 'mul (id 'x) (num 2))) (num 10)) (app (fun 'x (binop 'div (id 'x) (num 2))) (num 8))))

; 1
;(eval-cfae (app (if0 (num 0) (fun 'x (binop 'add (id 'x) (num 1))) (fun 'x (binop 'add (id 'x) (num 2)))) (num 0)))

; 5
;(eval-cfae (app (fun 'x (app (fun 'y (binop '+ (id 'x) (id 'y))) (num 3))) (num 2)))

; (fun 'x (binop 'add (id 'x) (num 1)))
(eval-cfae (fun 'x (binop 'add (id 'x) (num 1))))

; (fun 'x (id 'x))
(eval-cfae (fun 'x (id 'x)))