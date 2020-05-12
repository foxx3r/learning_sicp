(define (linear-combination a b x y)
  (+ (* a x) (* b y)))

(define (linear-combination a b x y)
  (add (mul a x) (mul b y)))

(define (add-rat x y)
  (make-rat (+
              (* (numer x) (denom y))
              (* (numer y) (denom x))) 
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (-
              (* (numer x) (denom y))
              (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (*
              (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (*
              (numer x)
              (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))
