(define loop
  (lambda ()
    (+# 1 2)
    (loop)))

(define main
  (lambda ()
    (loop)))

