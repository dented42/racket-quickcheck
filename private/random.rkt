#lang typed/racket/base

; This is a purely functional random generator, based on Lennart
; Augustsson's generator shipped with Hugs.

; Its comment says:

; This implementation uses the Portable Combined Generator of L'Ecuyer
; ["System.Random\#LEcuyer"] for 32-bit computers, transliterated by
; Lennart Augustsson.  It has a period of roughly 2.30584e18.

; This makes it not as good as Sebastian Egner's reference
; implementation of SRFI 27, but much faster for applications that
; need a pure random-number generator with a `split' operation.

(provide Random-Generator
         (rename-out [make-random-generator random-generator])
         random-generator?
	 random-generator-next
	 random-generator-split
	 random-integer
	 random-real)

(require racket/fixnum)

(define-type Random-Generator random-generator)

(struct random-generator ([s1 : Fixnum]
                          [s2 : Fixnum]))

(: min-bound Fixnum)
(define min-bound (fx- 0 (fxlshift 1 31)))

(: min-bound Fixnum)
(define max-bound (fx- (fxlshift 1 31) 1))

(: int-range Fixnum)
(define int-range (fx- max-bound min-bound))

(: make-random-generator (Fixnum → Random-Generator))
(define (make-random-generator s)
  (if (fx< s 0)
      (make-random-generator (fx- 0 #{s :: Fixnum}))
      (let ((q (fxquotient s 2147483562))
	    (s1 (fxremainder s 2147483562)))
	(let ((s2 (fxremainder q 2147483398)))
	  (random-generator (fx+ 1 s1) (fx+ 1 s2))))))

(: random-generator-next (Random-Generator → (Values Fixnum Random-Generator)))
(define (random-generator-next rg)
  (let ((s1 (random-generator-s1 rg))
	(s2 (random-generator-s2 rg)))

    (let ([k (fxquotient s1 53668)]
	  [k* (fxquotient s2 52774)])
      (let ((s1*  (fx- (fx* 40014 (fx- s1 (fx* k 53668)))
		     (fx* k 12211)))
	    (s2* (fx- (fx* 40692 (fx- s2 (fx* k* 52774)))
		    (fx* k* 3791))))
	(let ((s1** (if (negative? s1*)
			(fx+ s1* 2147483563)
			s1*))
	      (s2** (if (negative? s2*)
			(fx+ s2* 2147483399)
			s2*)))
	  (let* ((z (fx- s1** s2**))
		 (z* (if (fx< z 1)
			 (fx+ z 2147483562)
			 z)))
	    (values z* (random-generator s1** s2**))))))))


(: random-generator-split (Random-Generator → (Values Random-Generator Random-Generator)))
(define (random-generator-split rg)
  (let ((s1 (random-generator-s1 rg))
	(s2 (random-generator-s2 rg)))
    (let ((new-s1 (if (fx= s1 2147483562)
		      1
		      (fx+ s1 1)))
	  (new-s2 (if (fx= s2 1)
		      (assert 2147483398 fixnum?)
		      (fx- s2 1))))
      (call-with-values
	  (lambda ()
	    (random-generator-next rg))
	(lambda (_ [nrg : Random-Generator])
          (values (random-generator new-s1
                                    (random-generator-s2 nrg))
                  (random-generator (random-generator-s1 nrg)
                                    new-s2)))))))

; The intervals are inclusive.

(: random-integer (Random-Generator Integer Integer → (Values Integer Random-Generator)))
(define (random-integer rg low high)
  (let ([b 2147483561]
        [k : Positive-Integer (+ (assert (- high low) exact-nonnegative-integer?) 1)])
    (let loop : (Values Integer Random-Generator) ([n : Nonnegative-Integer (ilogbase b k)]
                                                   [acc low]
                                                   [rg rg])
      (if (zero? n)
	  (values (+ low (modulo acc k))
		  rg)
	  (call-with-values
	      (lambda () (random-generator-next rg))
	    (lambda ([x : Fixnum] [rgn : Random-Generator])
	      (loop (- n 1) (+ x (* acc b)) rgn)))))))

(: random-real (Random-Generator Real Real → (Values Real Random-Generator)))
(define (random-real rg low high)
  (call-with-values
      (lambda ()
	(random-integer rg min-bound max-bound))
    (lambda ([x : Integer] [nrg : Random-Generator])
      (let ((scaled-x (+ (/ (+ low high) 2)
			 (* (/ (- high low) int-range)
			    x))))
	(values scaled-x nrg)))))

(: ilogbase (Nonnegative-Integer Nonnegative-Integer → Positive-Integer))
(define (ilogbase b i)
  (if (< i b)
      1
      (+ 1 (ilogbase b (quotient i b)))))