(define populate
  (lambda (arr i)
    (let ((N (len# arr)))
      (if (<# i N)
          (begin
            (set-nth!# arr i i)
            (populate arr (+# i 1)))
          arr))))

(define arr-sum
  (lambda (arr i s)
    (let ((N (len# arr)))
      (if (<# i N)
          (arr-sum arr (+# i 1) (+# s (nth# arr i)))
          s))))

(define main-loop
  (lambda (i)
    (if (<# 0 i)
        (begin
          (main-once)
          (main-loop (-# i 1)))
        0)))

(define main-once
  (lambda ()
    (let* ((N 2000)
           (arr (mk-arr# N 0)))
      (populate arr 0)
      (display# (arr-sum arr 0 0)))))

(define main
  (lambda ()
    (main-loop 10)))

