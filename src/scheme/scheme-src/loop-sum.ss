(define loop-sum
  (lambda (s n)
    (if (<# n 1)
        s
        (loop-sum (+# s n) (-# n 1)))))

(define main
  (lambda ()
    (display# (loop-sum 0 200000000))))



