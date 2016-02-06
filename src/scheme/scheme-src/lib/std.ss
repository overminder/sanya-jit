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

(define display
  (lambda (x)
    (display# x)))

(define cons
  (lambda (a b)
    (cons# a b)))
