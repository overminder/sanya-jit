;; XXX: Not lazy.
(define and
  (lambda (a b)
    (if a
        b
        0)))

(define <
  (lambda (a b)
    (if (and (fixnum?# a)
             (fixnum?# b))
        (<# a b)
        (panic-inline-sym# NOT-INT))))

(define +
  (lambda (a b)
    (if (and (fixnum?# a)
             (fixnum?# b))
        (+# a b)
        (panic-inline-sym# NOT-INT))))

(define -
  (lambda (a b)
    (if (and (fixnum?# a)
             (fixnum?# b))
        (-# a b)
        (panic-inline-sym# NOT-INT))))

(define fibo
  (lambda (n)
    (if (< n 2)
        n
        (+ (fibo (- n 1))
           (fibo (- n 2))))))

(define main
  (lambda ()
    (display# (fibo 40))))

