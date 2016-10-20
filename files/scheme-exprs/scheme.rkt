#lang racket

;Q2 // Square roots
(define (quadratic-roots a b c)
  (define delta (sqrt (- (* b b) (* 4.0 a c))))
  ( let ([denom (* 2.0 a)])
     (list (/ (+ (- b) delta) denom) (/ (- (- b) delta) denom))))

; Q3 // Multiply each element by x.
(define (mul-list lst x)
  (if (null? lst)
      '()
      (cons (* x (car lst))
            (mul-list (cdr lst) x))))

;Q4 // sum of lengths of lists
(define (sum-lengths list)
  (define len (length list))
  (if(null? list)
     0
     (+ (length (car list)) (sum-lengths (cdr list)))))

; Q5 // Poly eval
(define (poly-eval coeffs x)
  (if (null? coeffs)
      0
      (+ (* (car coeffs) (expt x (- (length coeffs) 1))) (poly-eval (cdr coeffs) x))))

; Q6 // Poly eval Horners
(define (poly-eval-horner coeffs x)
  (define (horner-aux coeffs x accumulator)
    (if (null? coeffs)
        accumulator
        (horner-aux (cdr coeffs) x (+ (* accumulator x) (car coeffs)))))
  (horner-aux (reverse coeffs) x 0)) 

; Q7 // Count occurences
(define (count-occurrences s-exp x)
  (cond ((null? s-exp) 0)
        ((equal? (car s-exp) x) (+ 1 (count-occurrences (cdr s-exp) x)))
        (else (count-occurrences (cdr s-exp) x))))

; Q8 // Eval arithmetic
(define (eval-arith exp)
  (cond ([number? exp] exp) ; if number found, return itself 
        (else (eval-helper exp)))) ;if not number, handle two-arg expression by helper

(define (eval-helper expr)
  (let ((operator (car expr))
        (op1 (eval-arith (cadr expr)))
        (op2 (eval-arith (caddr expr))))
    (cond ([equal? operator 'add] (+ op1 op2))
          ([equal? operator 'sub] (- op1 op2))
          ([equal? operator 'mul] (* op1 op2))
          ([equal? operator 'div] (/ op1 op2))
          (else (error "Operation not supported for expr:" expr)))))

; Q9 // Sum lengths tail rec
(define (sum-lengths-tr list)
  (letrec ([sum-lengths-helper
            (lambda (list accumulator)
              (if (null? list)
                  accumulator
                  (sum-lengths-helper (cdr list) (+ accumulator (length (car list))) )))])
  (sum-lengths-helper list 0)))    

; Q11 // Map :: Multiply each element by x.
(define (mul-mapped elements x)
  (if (null? elements)
      '()
      (map (lambda(elem)(* elem x)) elements)))

; Q12 // Foldl :: Sum of lengths of lists.
(define (sum-len-mapped elements)
  (if (null? elements)
      0
      (foldl + 0 (map length elements))))

