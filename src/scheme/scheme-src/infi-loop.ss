(define loop
  (lambda (n)
    (loop (+# n 1))))

(define main
  (lambda ()
    (loop 0)))

