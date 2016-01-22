(define main
  (lambda ()
    (define i-box (mk-box# 0))
    (define go
      (lambda ()
        (define i (unwrap-box# i-box))
        (if (<# i 10)
            (begin
              (display# i)
              (set-box!# i-box (+# i 1))
              (go))
            i)))
    (go)))

