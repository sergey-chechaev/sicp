(define (square x) (* x x))

(define (smallest-devisor n)
  (find-devisor n 2))

(define (find-devisor n test-devisor)
  (cond ((> (square test-devisor) n) n)
        ((devides? test-devisor n) test-devisor)
        (else (find-devisor n (+ test-devisor 1)))))

(define (devides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= (smallest-devisor n) n))


(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

;; 1.22 написать процедуру которая проверяет все нечетные числа на простото в заданном диапазоне

(define (odd? n)
  (> (remainder n 2) 0))

(define (search-for-primes start end)
  (cond ((= start end) (newline))
        ((odd? start) (timed-prime-test start) (search-for-primes (+ start 1) end))
        (else (search-for-primes (+ start 1) end))))


