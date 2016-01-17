(define fibo
  (lambda (n k)
    (if (<# n 2)
        (k n)
        (fibo (-# n 1)
              (lambda (res1)
                (fibo (-# n 2)
                      (lambda (res2)
                        (k (+# res1 res2)))))))))

;; (fibo 37)
;; Naive closure allocation (one call per alloc): 6.2 seconds
;; With inline closure allocation: 1.1 seconds

(define main
  (lambda ()
    (fibo 40
          (lambda (res)
            (display# res)))))
