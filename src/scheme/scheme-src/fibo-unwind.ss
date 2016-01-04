(define fibo
  (lambda (n)
    (if (<# n 2)
        (panic-inline-sym# 我擦)
        (+# (fibo (-# n 1))
            (fibo (-# n 2))))))

(define main
  (lambda ()
    (display# (fibo 40))))

