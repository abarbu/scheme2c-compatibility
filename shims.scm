;; From traversal

(define rest cdr)

(define (map-n-vector f n)
 ;; needs work: Won't work correctly when F is nondeterministic.
 (let ((n (inexact->exact (round n))))
  (let ((v (make-vector n)))
   (let loop ((i 0))
    (when (fx< i n)
     (vector-set! v i (f i))
     (loop (fx+ i 1))))
   v)))

(define (for-each-n f n)
 (let ((n (inexact->exact (round n))))
  (let loop ((i 0)) (when (fx< i n) (f i) (loop (fx+ i 1))))))

(define (for-each-indexed f l)
 (let loop ((i 0) (l l))
  (unless (null? l) (f (first l) i) (loop (+ i 1) (rest l)))))

(define (enumerate-vector n)
 (let ((v (make-vector n)))
  (for-each-n (lambda (i) (vector-set! v i i)) n)
  v))

(define (for-each-vector f v . &rest)
 (for-each-n
  (lambda (i)
   (apply f (vector-ref v i) (map (lambda (v) (vector-ref v i)) &rest)))
  (vector-length v)))

(define (positionv x l)
 (let loop ((l l) (i 0))
  (cond ((null? l) #f)
	((eqv? x (first l)) i)
	(else (loop (rest l) (+ i 1))))))
