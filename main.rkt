; gulsen sabak
; 2020400072
; compiling: yes
; complete: yes
#lang racket

(provide (all-defined-out))

; read and parse the input file
(define parse (lambda (input-file)
        (letrec (
            [input-port (open-input-file input-file)]
            [read-and-combine (lambda ()
                (let ([line (read input-port)])
                    (if (eof-object? line)
                        '()
                        (append `(,line) (read-and-combine))
                    )
                )
            )]
            )
            (read-and-combine)
        )
    )
)
(define create-hash (lambda (vars values)
        (letrec (
            [create-hash-iter (lambda (vars values hash)
                (if (null? vars)
                    hash
                    (create-hash-iter (cdr vars) (cdr values) (hash-set hash (car vars) (car values)))
                )
            )]
            )
            (create-hash-iter vars values (hash))
        )
    )
)

(define add-to-hash (lambda (old-hash new-hash)
        (foldl (lambda (key hash) (hash-set hash key (hash-ref new-hash key)))
            old-hash
            (hash-keys new-hash)
        )
    )
)

(define eval-program (lambda (program-str)
        (get (eval-exprs (parse program-str) empty-state) '-r)
    )
)

; solution starts here

; 1. empty-state (5 points)
; creating an empty state (state represents hash)
(define empty-state (hash))

; 2. get (5 points)
; getting the given element from the given state
(define (get state var)
    ; control whether our variable is list or not
  (cond [(list? var) var] 
    ; if the variable is in the state then control whether it is symbol or not. If it is a symbol, then make recursion untill you find a number/ else just eval the variable                          
        [(hash-has-key? state var) (if (symbol? var) (get state (hash-ref state var)) (hash-ref state var))]  
    ; if it is not list just eval the variable               
    [else (eval var)]))


; 3. put (5 points)
; assign the value to the variable and put this pair to the given state
(define (put state var val)
    (hash-set state var val))

; 4. := (15 points)
; call eval-expr to get the val of -r and assign this value to the variable after that add this pair to the state which comes from eval-expr 
(define (:= var val-expr state)
   (put (eval-expr val-expr state) var (get (eval-expr val-expr state) '-r)))

; 5. if: (15 points)
; call eval-expr to get the value of the -r and if it is true then call eval-expr again with then expression/ if it is false call the eval-expr with else-expr again
(define (if: test-expr then-exprs else-exprs state)
    (if (get (eval-expr test-expr state) '-r) (eval-exprs then-exprs state) (eval-exprs else-exprs state)))

; 6. while: (15 points)
(define (while: test-expr body-exprs state)
    ; call eval-expr to get the value of the -r and if it is true make recursion/ if it is false assign the value which comes from eval-expr and -r to the -r and put it in the state
    (if (get (eval-expr test-expr state) '-r) 
    (while: test-expr body-exprs (eval-exprs body-exprs state))
    (put state '-r (get (eval-expr test-expr state) '-r))))

; map-eval is helper function that I wrote
; it takes a list and state if list contains symbols, function goes to the state and find the value of them and put these values to the list. 
; (map-eval '(+ b a) (hash 'a 5 'b 3)) result of that is '(#<procedure:+> 3 5)
(define (map-eval lst state)
    (map (lambda (var) (if (list? var) (get (eval-expr var state) '-r) (get state var)))lst)
)

; it takes state, variable list and value list, if the cdr of variable list empty then I put the car of value and variable listto the state/ if cdr of var list is not empty then I asisgn the car of variable list to the car of the valur list. After that I put this pair to the state which comes from the recursion of help-func
(define (help-func var_list value_list state)
    (cond [(empty? (cdr var_list)) (put state (car var_list) (car value_list))] [else (put  (help-func (cdr var_list) (cdr value_list) state) (car var_list) (car value_list))]))

; 7. func (15 points)
; it evaluates body-exprs with the help of state and params. After evaluation, it puts the result to the state
(define (func params body-exprs state)    
    (put state '-r (lambda args (get(eval-exprs  body-exprs (help-func params args state)) '-r))))


; 8. eval-expr (20 points)
(define (eval-expr expr state)
; control whether an ezpression is list or not
    (if (list? expr) 
    ; if car of the expr is := then I call := function with the necessary parameters
    (cond [(equal? (car expr) ':=) (apply := (list (cadr expr) (caddr expr) state))]
    ; if car of the expr is if: then I call if: function with the necessary parameters
        [(equal? (car expr) 'if:) (apply if: (list (cadr expr)  (caddr expr) (cadddr expr) state))]
    ; if car of the expr is while: then I call while: function with the necessary parameters
        [(equal? (car expr) 'while:) (apply while: (list (cadr expr) (caddr expr) state))]
    ; if car of the expr is func then I call func function with the necessary parameters
        [(equal? (car expr) 'func) (apply func (list (cadr expr) (caddr expr) state))]
    ; if car of the expr is lambda then I assign -r to the evaluated expr and put it in the state
        [(equal? (car expr) 'lambda) (put state '-r (eval expr))]
    ; If the return value of map eval is list and first element of that list is procedure
        (else (if (and (list? (map-eval expr state)) (procedure? (car (map-eval expr state))))
        ; apply the first and second element of the state and assign it to the -r and put the state
            (put state '-r (apply (car (map-eval expr state)) (cdr (map-eval expr state))))
        ; If the return value of map eval is list then I assign -r to the result of the map-eval and put to the state
            (if (list? (map-eval expr state))
            (put state '-r (map-eval expr state))
        ; else I evaluated the result of the map eval. After that I assign -r to that result and put to the state
            (put state '-r (eval (map-eval expr state)))))))
    ; if expr is symbol then I get its value and assign it to the -r and put to the state. / else I eval the expr and assign to -r and put to the state
    (if (symbol? expr) (put state '-r (get state expr)) (put state '-r (eval expr))))) 

   
; 9 eval-exprs (5 points)
; evaluates each expression in exprs with eval-expr function and returns the resulting state after evaluating the last expr in exprs
(define (eval-exprs exprs state) (foldl eval-expr state exprs))