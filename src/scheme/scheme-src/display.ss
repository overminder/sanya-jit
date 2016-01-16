(define main
  (lambda ()
    (define arr (mk-arr# 10 42))
    (define uarr (mk-arr-i64# 10 42))
    (display# arr)
    (display# uarr)
    (display# main)
    (display# (nth# arr 9))))
