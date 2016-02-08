(define main
  (lambda ()
    (let* ((a 42)
           (f (lambda () a)))
      (display (f)))))
