(define fibo
  (lambda (n)
    (if (< n 2)
        n
        (+ (fibo (- n 1))
           (fibo (- n 2))))))

(define main
  (lambda ()
    (display (fibo 40))))

