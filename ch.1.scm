;; SICP -- Chapter 1 exercises

                                        ; Exercise 1.1

;; 10
;; 10

(+ 5 3 4)
;; 12

(- 9 1)
;; 8

(/ 6 2)
;; 3

(+ (* 2 4) (- 4 6))
;; 6

(define a 3)
;; a

(define b (+ a 1))
;; b

(= a b)
;; #f

(if (and (> b a) (< b (* a b)))
    b
    a)
;; 4

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
;; 16

(+ 2 (if (> b a) b a))
;; 6

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))
;; 16

                                        ; Exercise 1.2

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))
;; -37/150

                                        ; Exercise 1.3
;; Define a procedure that takes three numbers as arguments and returns the sum
;; of the squares of the two larger numbers.

;; There were two methods I liked here, but unfortunately I am responsible for
;; neither of them... I broke under the pressure and googled these I'm sorry to
;; say. It probably would have been best to sleep on it, but I figured that in
;; work, I would have googled an answer I was struggling with as well.

;; Method 1:

(define (square x) (* x x))

(define (sum-of-squares x y) (+ (square x) (square y)))

(define (largest a b c)
  (cond ((and (> a b) (> a c)) a)
        ((and (> b a) (> b c)) b)
        ((and (> c b) (> c a)) c)))

(define (larger x y) (if (> x y) x y))

(define (sum-of-two-largest-squared a b c)
  (cond ((= (largest a b c) a) (sum-of-squares a (larger b c)))
        ((= (largest a b c) b) (sum-of-squares b (larger a c)))
        ((= (largest a b c) c) (sum-of-squares c (larger a b)))))

;; Method 2: Using the list special form

(define (max-two a b c)
  (if (>= a b)
      (if (>= b c)
          (list a b)
          (list a c))
      (if (>= a c)
          (list b a)
          (list b c))))

(define (sum-and-square-largest a b c)
  (apply sum-of-squares (max-two a b c)))

;; both methods used here w/ same numbers
(sum-and-square-largest 12 32 13)
;; 1193
(sum-of-two-largest-squared 12 32 13)
;; 1193

                                        ; Exercise 1.4
;; Observe that our model of evaluation allows for combinations whose operators
;; are compound expressions. Use this observation to describe the behavior of
;; the following procedure:

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(a-plus-abs-b 3 5)

;; We evaluate to see if b is greater than 0. If so, then perform addition on a
;; and b, if not, then perform subtraction on a and b. In the case of
;; subtraction, we are subtracting a negative number, in ends up adding the
;; absolute value of b.

                                        ; Exercise 1.5
;; Ben Bitdiddle has invented a test to determine whether the
;; interpreter he is faced with is using applicative-order evaluation or
;; normal-order evaluation. He defines the following two procedures:

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))


;; Then he evaluates the expression

(test 0 (p))

;; What behavior will Ben observe with an interpreter that uses
;; applicative-order evaluation?

;; What behavior will he observe with an interpreter that uses normal-order
;; evaluation? Explain your answer.

;; (Assume that the evaluation rule for the special form if is the same whether
;; the interpreter is using normal or applicative order: The predicate
;; expression is evaluated first, and the result determines whether to evaluate
;; the consequent or the alternative expression.)

;; applicative-order evaluation will result in the value 0 being returned. This
;; is because the (= x 0) expression is the first applied expression. As it is
;; true in this case the second expression (p) is never evaluated. In
;; normal-order evaluation, all expressions are expanded to their primatives.
;; (p) would evaluate to (p) and so on recursively, causing an infinite loop.

;; ------------------------------------ notes

;; 1.1.7 Square roots by newtons method

(define (sqrt-ier guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-ier (improve guess x)
                x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-ier 1.0 x))

(sqrt 9)
                                        ;  3.00009155413138
(sqrt (+ 100 37))
                                        ; 11.704699917758145
(sqrt (+ (sqrt 2) (sqrt 3)))
                                        ; 1.7739279023207892
(square (sqrt 1000))
                                        ;  1000.000369924366

;; ------------------------------------ exercies

                                        ; Exercise 1.6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(new-if (= 2 3) 0 5) ; 5
(new-if (= 1 1) 0 5) ; 0

(define (new-sqrt-ier guess x)
  (new-if (good-enough? guess x)
          guess
          (new-sqrt-ier (improve guess x)
                        x)))

(sqrt 4) ; 2.0000000929222947
(new-sqrt-ier 1.0 4) ; ERRORError: retort-syntax

                                        ; Exercise 1.7

;; design a new good-enough? that watches how guess changes from one iteration
;; to another and stop when change is a very small fraction of the guess

;; for very small numbers which are smaller than the specified good-enough? .001
;; value there isn't enough specificity to define the number. for larger
;; numbers, the machine is unable to define very small differences between very
;; large numbers.

(define (good-enough? guess x)
  (= (improve guess x) guess))

(sqrt 100000000)


                                        ; Exercise 1.8
(define (square x)
  (* x x))

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (good-enough? guess x)
  (= (improve guess x) guess))

(define (3rt-ier guess x)
  (if (good-enough? guess x)
      guess
      (3rt-ier (improve guess x) x)))

(define (3root x)
  (3rt-ier 1.1 x))

(3root 9) ; 2.080083823051904f

;; ------------------------------------ Section 1.2
