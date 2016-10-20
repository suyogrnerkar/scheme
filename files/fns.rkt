;;-*- mode: scheme; -*-
;; :set filetype=scheme

;;Return a 2-element list containing the 2 roots of the quadratic
;;equation a*x^2 + b*x + c = 0 using the classical formula for the
;;roots of a quadratic equation.  The first element in the returned list
;;should use the positive square-root of the discriminant, the second
;;element should use the negative square-root of the discriminant.
(define (quadratic-roots a b c)
  (define delta (sqrt (- (* b b) (* 4 a c))))
  ( let ([denom (* 2 a)])
     (list (/ (+ (- b) delta) denom) (/ (- (- b) delta) denom))))

;;Return the list resulting by multiplying each element of `list` by `x`.
(define (mul-list list x)
  (if (null? list)
      '()
      (cons (* x (car list))
            (mul-list (cdr list) x))))

;;Given a proper-list list of proper-lists, return the sum of the
;;lengths of all the contained lists.
(define (sum-lengths list)
  (if(null? list)
     0
     (+ (length (car list)) (sum-lengths (cdr list)))))

;;Evaluate polynomial with list of coefficients coeffs (highest-order
;;first) at x.  The computation should reflect the traditional
;;representation of the polynomial.
(define (poly-eval coeffs x)
  (if (null? coeffs)
      0
      (+ (* (car coeffs) (expt x (- (length coeffs) 1))) (poly-eval (cdr coeffs) x))))

;;Evaluate polynomial with list of coefficients coeffs (highest-order
;;first) at x using Horner's method.
(define (poly-eval-horner coeffs x)
  (define (horner-aux coeffs x accumulator)
    (if (null? coeffs)
        accumulator
        (horner-aux (cdr coeffs) x (+ (* accumulator x) (car coeffs)))))
  (horner-aux coeffs x 0)) 

;;Return count of occurrences equal? to x in exp
(define (count-occurrences exp x)
  (cond ((null? exp) 0)
        ((equal? (car exp) x) (+ 1 (count-occurrences (cdr exp) x)))
        (else (count-occurrences (cdr exp) x))))

;;Return result of evaluating arith expression over Scheme numbers
;;with fully parenthesized prefix binary operators 'add, 'sub, 'mul
;;and 'div.
(define (eval-arith exp)
  (define (eval-helper expr)
    (let ((operator (car expr))
          (op1 (eval-arith (cadr expr)))
          (op2 (eval-arith (caddr expr))))
      (cond ([equal? operator 'add] (+ op1 op2))
            ([equal? operator 'sub] (- op1 op2))
            ([equal? operator 'mul] (* op1 op2))
            ([equal? operator 'div] (/ op1 op2))
            (else (error "Operation not supported for expr:" expr)))))
  
  (cond ([number? exp] exp) ; if number found, return itself 
        (else (eval-helper exp)))) ;if not number, handle two-arg expression by helper


;;Given a proper-list list of proper-lists, return sum of lengths of
;;all the contained lists.  Must be tail-recursive.
(define (sum-lengths-tr list)
  (letrec ([sum-lengths-helper
            (lambda (list accumulator)
              (if (null? list)
                  accumulator
                  (sum-lengths-helper (cdr list) (+ accumulator (length (car list))))))])
    (sum-lengths-helper list 0))) 

;;Evaluate polynomial with list of coefficients coeffs (highest-order
;;first) at x.  Must be tail-recursive.
(define (poly-eval-tr coeffs x)
  (define (poly-eval-tr-aux coeff accumulator)
    (if (null? coeff)
        accumulator
        (poly-eval-tr-aux (cdr coeff) (+ (* accumulator x) (car coeff)) )))
  (poly-eval-tr-aux coeffs 0))

;;Return the list resulting by multiplying each element of `list` by `x`.
;;Cannot use recursion, can use one or more of `map`, `foldl`, or `foldr`.
(define (mul-list-2 list x)
  (if (null? list)
      '()
      (map (lambda(elem)(* elem x)) list)))

;;Given a proper-list list of proper-lists, return the sum of the
;;lengths of all the contained lists.  Cannot use recursion, can use
;;one or more of `map`, `foldl`, or `foldr`.
(define (sum-lengths-2 list)
  (if (null? list)
      0
      (foldl + 0 (map length list))))
