(define main
  (lambda ()
    (letrec ((fibo
               (lambda (n)
                 (if (< n 2)
                     n
                     (+ (fibo (- n 1))
                        (fibo (- n 2))))))
             (<
               (lambda (a b)
                 (if (and (fixnum?# a)
                          (fixnum?# b))
                     (<# a b)
                     (panic!# 0))))

             (+
               (lambda (a b)
                 (if (and (fixnum?# a)
                          (fixnum?# b))
                     (+# a b)
                     (panic!# 1))))

             (-
               (lambda (a b)
                 (if (and (fixnum?# a)
                          (fixnum?# b))
                     (-# a b)
                     (panic!# 2)))))
      (display# (fibo 40)))))
