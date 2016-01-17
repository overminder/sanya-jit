(define fibo
  (lambda (n k)
    (if (<# n 2)
        (k n)
        (fibo (-# n 1)
              (lambda (res1)
                (fibo (-# n 2)
                      (lambda (res2)
                        (k (+# res1 res2)))))))))

(define main
  (lambda ()
    (fibo 20
          (lambda (res)
            (display# res)))))
