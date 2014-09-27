(define (prime n)
  ; z - running product of primes
  (define (iter z num count)
    (cond ((= count 0) (- num 1))
          ((prime? z num) (iter (* z num) (+ num 1) (- count 1)))
          (else (iter z (+ num 1) count))))
  (iter 1 2 n))

(define (prime? z n)
  (cond ((= n 2) #t)
        (not (= (gcd z n) 1))))
