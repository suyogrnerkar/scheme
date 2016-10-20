;;-*- mode: scheme; -*-
;; :set filetype=scheme

;;Return a 2-element list containing the 2 roots of the quadratic
;;equation a*x^2 + b*x + c = 0 using the classical formula for the
;;roots of a quadratic equation.  The first element in the returned list
;;should use the positive square-root of the discriminant, the second
;;element should use the negative square-root of the discriminant.
;; (quadratic-roots <coeff of x^2> <coeff of x> <const c>)
(define (quadratic-roots a b c)
  (define delta (sqrt (- (* b b) (* 4 a c))))
  (let ([denom (* 2 a)])
    (list (/ (+ (- b) delta) denom) (/ (- (- b) delta) denom))))

;;Return the list resulting by multiplying each element of `list` by `x`.
;; (mul-list <list of elements> <multiplier x>)
(define (mul-list list x)
  (if (null? list)
      '()
      (cons (* x (car list)) (mul-list (cdr list) x))))

;;Given a proper-list list of proper-lists, return the sum of the
;;lengths of all the contained lists.
;; (sum-lengths <list of lists>)
(define (sum-lengths list)
  (if(null? list)
     0
     (+ (length (car list)) (sum-lengths (cdr list)))))

;;Evaluate polynomial with list of coefficients coeffs (highest-order
;;first) at x.  The computation should reflect the traditional
;;representation of the polynomial.
;; (poly-eval <list of coeffs of x> <value of x>)
(define (poly-eval coeffs x)
  (if (null? coeffs)
      0
      ; <length of coeffs> - 1, serves as the exponent of x
      (+ (* (car coeffs) (expt x (- (length coeffs) 1))) (poly-eval (cdr coeffs) x)))) 

;;Evaluate polynomial with list of coefficients coeffs (highest-order
;;first) at x using Horner's method.
;; (poly-eval-horner <list of coeffs of x> <value of x>)
(define (poly-eval-horner coeffs x)
  ;; auxiliary/helper function horner-aux
  (define (horner-aux coeffs x accumulator)
    (if (null? coeffs)
        accumulator
        (horner-aux (cdr coeffs) x (+ (* accumulator x) (car coeffs)))))
  (horner-aux coeffs x 0)) 

;;Return count of occurrences equal? to x in exp
;; (count-occurrences <s-expression> <value of x>)
(define (count-occurrences exp x)
  (if (pair? exp)
      (begin ; if part of expression
        (let ([head (car exp)] [tail (cdr exp)])
          (if (equal? x head)
              [+ 1 (count-occurrences head x) (count-occurrences tail x)]
              [+ (count-occurrences head x) (count-occurrences tail x)])))
      0))    ; else part of expression

;;Return result of evaluating arith expression over Scheme numbers
;;with fully parenthesized prefix binary operators 'add, 'sub, 'mul
;;and 'div.
;; (eval-arith <expression to eveluate>)
(define (eval-arith exp)
  ;; auxiliary/helper function eval-helper, takes parameter expr 
  (define (eval-helper expr)
    (let ((operator (car expr))            ; operator
          (op1 (eval-arith (cadr expr)))   ; operand 1
          (op2 (eval-arith (caddr expr)))) ; operand 2
      (cond ([equal? operator 'add] (+ op1 op2))
            ([equal? operator 'sub] (- op1 op2))
            ([equal? operator 'mul] (* op1 op2))
            ([equal? operator 'div] (/ op1 op2))
            (else (error "Operation not supported for expr:" expr)))))
  
  (cond ([number? exp] exp) ; if number found, return itself 
        (else (eval-helper exp)))) ;if not number, handle two-arg expression by helper function


;;Given a proper-list list of proper-lists, return sum of lengths of
;;all the contained lists.  Must be tail-recursive.
;; (sum-lengths-tr <list of proper lists>)
(define (sum-lengths-tr list)
  (letrec ([sum-lengths-helper
            (lambda (list accumulator)
              (if (null? list)
                  accumulator
                  (sum-lengths-helper (cdr list) (+ accumulator (length (car list))))))])
    (sum-lengths-helper list 0))) 

;;Evaluate polynomial with list of coefficients coeffs (highest-order
;;first) at x.  Must be tail-recursive.
;; (poly-eval-tr <list of coeffs of x> <value of x>)
(define (poly-eval-tr coeffs x)
  (define (poly-eval-tr-aux coeff accumulator)
    (if (null? coeff)
        accumulator
        (poly-eval-tr-aux (cdr coeff) (+ (* accumulator x) (car coeff)) )))
  (poly-eval-tr-aux coeffs 0))

;;Return the list resulting by multiplying each element of `list` by `x`.
;;Cannot use recursion, can use one or more of `map`, `foldl`, or `foldr`.
;; (mul-list-2 <list of elements> <multiplier x>)
(define (mul-list-2 list x)
  (if (null? list)
      '()
      (map (lambda(elem)(* elem x)) list)))

;;Given a proper-list list of proper-lists, return the sum of the
;;lengths of all the contained lists.  Cannot use recursion, can use
;;one or more of `map`, `foldl`, or `foldr`.
;; (sum-lengths-2 <list of lists>)
(define (sum-lengths-2 list)
  (if (null? list)
      0
      ; sums up the length of lists found by applying map length to sublists
      (foldl + 0 (map length list)))) 
