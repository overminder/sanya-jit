(define main
  (lambda ()
    (define a 42)
    (define f (lambda () a))
    (display# (f))))

