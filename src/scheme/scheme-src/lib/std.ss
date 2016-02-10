(define <
  (lambda (a b)
    (if (and (fixnum?# a)
             (fixnum?# b))
        (<# a b)
        (panic!# 'not-a-fixnum))))

(define +
  (lambda (a b)
    (if (and (fixnum?# a)
             (fixnum?# b))
        (+# a b)
        (panic!# 'not-a-fixnum))))

(define -
  (lambda (a b)
    (if (and (fixnum?# a)
             (fixnum?# b))
        (-# a b)
        (panic!# 'not-a-fixnum))))

(define display
  (lambda (x)
    (display# x)))

(define cons
  (lambda (a b)
    (cons# a b)))
