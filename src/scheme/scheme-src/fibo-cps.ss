(define <
  (lambda (a b k)
    (if (and (fixnum?# a)
             (fixnum?# b))
        (k (<# a b))
        (panic-inline-sym# NOT-INT))))

(define +
  (lambda (a b k)
    (if (and (fixnum?# a)
             (fixnum?# b))
        (k (+# a b))
        (panic-inline-sym# NOT-INT))))

(define -
  (lambda (a b k)
    (if (and (fixnum?# a)
             (fixnum?# b))
        (k (-# a b))
        (panic-inline-sym# NOT-INT))))

(define fibo
  (lambda (n k)
    (< n 2
       (lambda (res1)
         (if res1
             (k n)
             (- n 1
                (lambda (mn1)
                  (fibo mn1
                        (lambda (res1)
                          (- n 2
                             (lambda (mn2)
                               (fibo mn2
                                     (lambda (res2)
                                       (+ res1 res2 k))))))))))))))

;; (fibo 37)
;; Fully CPSed, with inline closure allocation: 3.7 seconds

(define main
  (lambda ()
    (fibo 37
          (lambda (res)
            (display# res)))))

