(define populate
  (lambda (arr i N)
    (if (<# i N)
        (begin
          (set-nth!# arr i i)
          (populate arr (+# i 1) N))
        arr)))

(define arr-sum
  (lambda (arr i N s)
    (if (<# i N)
        (arr-sum arr (+# i 1) N (+# s (nth# arr i)))
        s)))

(define main-loop
  (lambda (i)
    (if (<# 0 i)
        (begin
          (main-once)
          (main-loop (-# i 1)))
        0)))

(define main-once
  (lambda ()
    (define N 2000)
    (define arr (mk-arr# N 0))
    (populate arr 0 N)
    (display# (arr-sum arr 0 N 0))))

(define main
  (lambda ()
    (main-loop 10)))

