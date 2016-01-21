(define fibo
  (lambda (n)
    (if (<# n 2)
        n
        (+# (fibo (-# n 1))
            (fibo (-# n 2))))))

;; fibo(40)
;; Fully unboxed fixnums: 1.4 sec
;; Boxed fixnums, unsafe operations, with if-<# cmp-jcc optimization: 2.8 sec
;; Boxed fixnums, unsafe operations, with if-<# cmp-jcc optimization, and
;;   constant oop pool optimization: 2.4 sec.
;; and with alloc fast-path optimization: 2.1 sec.
;; Boxed fixnums, unsafe operations: 3.3 sec
;; Boxed fixnums, checked operations: 12.6 sec!

(define main
  (lambda ()
    (display# (fibo 40))))

