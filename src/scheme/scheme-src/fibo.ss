(define <
  (lambda (a b)
    (if (and (fixnum?# a)
             (fixnum?# b))
        (<# a b)
        (panic!# 0))))

(define +
  (lambda (a b)
    (if (and (fixnum?# a)
             (fixnum?# b))
        (+# a b)
        (panic!# 1))))

(define -
  (lambda (a b)
    (if (and (fixnum?# a)
             (fixnum?# b))
        (-# a b)
        (panic!# 2))))

(define fibo
  (lambda (n)
    (if (< n 2)
        n
        (+ (fibo (- n 1))
           (fibo (- n 2))))))

(define main
  (lambda ()
    (display# (fibo 40))))

