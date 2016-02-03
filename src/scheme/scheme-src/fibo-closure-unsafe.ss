(define main
  (lambda ()
    (letrec ((fibo
               (lambda (n)
                 (if (<# n 2)
                     n
                     (+# (fibo (-# n 1))
                         (fibo (-# n 2)))))))
      (display# (fibo 40)))))
