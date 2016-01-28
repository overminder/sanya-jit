(define main
  (lambda ()
    (define x (many-cons 1000))
    (many-cons 500)
    (many-cons 500)
    (many-cons 500)
    (many-cons 500)
    (display# x)))

(define many-cons
  (lambda (n)
    (define loop (mk-box# 0))
    (set-box!# loop
      (lambda (i xs)
        (if (<# n i)
            xs
            ((unwrap-box# loop) (+# i 1) (cons# i xs)))))
    ((unwrap-box# loop) 0 0)))
