#lang sicp
486
(+ 3 5)
(* (+ 2 5)
   (- 5 1))

(define size 2)

size

(* 5 size)

(define pi 3.14159)
(define radius 10)
(* pi (* radius radius))

(define circumference (* 2 pi radius))
circumference

;; Recursive

;; defien procedure

(define (square x)
  (* x x))

(square 3)
(square (+ 2 5))
(square (square 3))

(define (sum-of-squares x y)
  (+ (square x)
     (square y)))

(sum-of-squares 3 4)

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))
(f 5)

;; cond
(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (abs2 x)
  (cond ((< x 0) (- x))
        (else x)))

;; if
(define (abs3 x)
  (if (< x 0)
      (- x)
      x))

(abs 5)
(abs -5)
(abs 0)
(abs2 5)
(abs2 -5)
(abs2 0)

(and true true)
(or false false)
(not (or true false))

(let ((x 6))
  (and (> x 5) (< x 10)))

(define (>= x y)
  (or (> x y) (= x y)))

(>= 3 3)

;; quiz 1.2
(/ (+ 5 4 (-
           2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 2 6) (- 2 7)))

;; quiz 1.3
(define (sum-of-two-large x y z)
  (if (< x y)
      (if (< x z)
          (+ y z)
          (+ x y))
      (if (< y z)
          (+ z x)
          (+ x y))))
(sum-of-two-large 1 2 3)
(sum-of-two-large 4 2 3)
(sum-of-two-large 1 5 3)
(sum-of-two-large 3 4 1)

;; quiz 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(a-plus-abs-b 3 5)
(a-plus-abs-b 3 (- 5))


;; section 1.1.7
;;  牛顿发求平方根

(define (average x y)
  (/ (+ x y) 2))
(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x))
     0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
(sqrt-iter 1 2)
(sqrt-iter 1.0 2.0)

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 9)
(sqrt 3)

(square (sqrt 1000))

;; quiz 1.7
(define (good-enough-2? old-guess new-guess x)
  (< (abs (- 1 (/ new-guess old-guess))) 0.0001)
  )
(define (sqrt-iter-2 old-guess new-guess x)
  (if (good-enough-2? old-guess new-guess x)
      new-guess
      (sqrt-iter-2 new-guess (improve new-guess x) x)))
(define (sqrt-2 x)
  (sqrt-iter-2 x 1.0 x))

(sqrt-2 2)
(sqrt 0.000005)
(sqrt-2 0.000005)
(square (sqrt 0.000005))
(square (sqrt-2 0.000005))

;; quiz 1.8
;; 求立方根
(define (improve-cube-root guess x)
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))

(define (cuberoot-iter old-guess new-guess x)
  (if (good-enough-2? old-guess new-guess x)
      new-guess
      (cuberoot-iter new-guess (improve-cube-root new-guess x) x)))
(define (cuberoot x)
  (cuberoot-iter x 1.0 x))

(cuberoot 9)
(cuberoot 2)
(cuberoot 27)

;; Abstract
(define (double x) (+ x x))

(define (square-2 x)
  (exp (double (log x))))
(square 5.0)
(square-2 5.0)

;; 局部定义 词法作用域
(define (sqrt-3 x)

  (define (improve guess)
    (average guess (/ x guess)))

  (define (good-enough? guess)
    (< (abs (- (square guess) x))
       0.001))

  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(sqrt-3 2)

;; Section 1.2  过程和它们所产生的计算
;; factorial 线性递归和迭代

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))
(factorial 6)

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

(define (factorial2 n)
  (fact-iter 1 1 n))
(factorial2 6)

;; 尾递归

(define (my+ a b)
  (if (= a 0)
      b
      (my+ (dec a) (inc b))))
(my+ 3 5)

;; quiz 1.10  Ackermann function
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10)
(A 2 4)
(A 3 3)

;; Tree Recursive
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))
(fib 5)
(fib 8)

(define (fib-2 n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))


;; coin change
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0)
             (= kinds-of-coins 0))
         0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (count-change amount)
  (cc amount 5))

;; quiz 1.11  将下面的树形递归转成迭代形式
(define (f1_11 n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))
(f1_11 2)

;; quiz 1.12 Pascal三角

(define (pascal-tri n idx)
  (cond
    ((or (= idx 0) (> idx n)) 0)
    ((= n 1) 1)
    (else (+ (pascal-tri (- n 1) (- idx 1))
             (pascal-tri (- n 1) idx)))))

(pascal-tri 1 0)
(pascal-tri 1 1)
(pascal-tri 1 2)
(pascal-tri 2 0)
(pascal-tri 2 1)
(pascal-tri 2 2)
(pascal-tri 2 3)
(pascal-tri 3 1)
(pascal-tri 3 2)
(pascal-tri 3 3)
(pascal-tri 4 1)
(pascal-tri 4 2)
(pascal-tri 4 3)
(pascal-tri 4 4)

;; Big O

;; expt
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))
(expt 2 10)
(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b (- counter 1) (* b product))))
(expt-iter 2 10 1)

(define (even? n)
  (= (remainder n 2) 0))
(even? 3)
(even? 6)

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
(fast-expt 2 10)


;; Fermat Prime test
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(fast-prime? 1000 10)
(fast-prime? 97 2)

;; Carmichael numbers: 561,1105,1729,2465,2821,6601
(fast-prime? 561 5)


;; Section 1.3 高阶函数抽象
(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))


(define (cube x)
  (* x x x))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))

;; 收敛很慢
(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2)))
         (pi-sum (+ a 4) b))))
(define (calc-pi n)
  (* 8 (pi-sum 1 n)))
(calc-pi 2000)

;; abstract to sum
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (sum-cubes2 a b)
  (sum cube a inc b))
(sum-cubes 1 10)
(sum-cubes2 1 10)

(define (sum-integers2 a b)
  (sum identity a inc b))
(sum-integers 1 10)
(sum-integers2 1 10)

(define (pi-sum2 a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(* 8 (pi-sum2 1 1000))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))
(integral cube 0 1 0.01)
;; 定积分 x^2 [0,1] => 1/3
(integral square 0 1 0.0001)

;; quiz 1.30
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a)
                          result))))
  (iter a 0))
(sum-iter identity 1 inc 10)

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))
(define (factorial-2 n)
  (product identity 1 inc n))
(factorial-2 5)
