;; SICP -- Chapter 1 exercises

                                        ; Exercise 1.1

10
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
;; define a prodecure that takes three numbers as
;; arguments and returns the sum of the squares of the
;; two larger numbers.

;; There were two methods I liked here, but unfortunately
;; I am responsible for neither of them... I broke under
;; the pressure and googled these I'm sorry to say.
;; It probably would have been best to sleep on it, but
;; I figured that in work, I would have googled an answer
;; I was struggling with as well.

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
