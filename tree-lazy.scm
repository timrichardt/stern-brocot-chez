;;--------------------------------------------------------------------
;; Lazy sequence construction

(define (repeat* x)
  (cons x (delay (repeat* x))))

(define (repeat x)
  (delay (repeat* x)))

(define (repeatedly* f)
  (cons (f) (delay (repeatedly* f))))

(define (repeatedly f)
  (delay (repeatedly* f)))

(define (cycle* s r)
  (if (null? r)
      (cycle* s s)
      (cons (car r) (delay (cycle* s (cdr r))))))

(define (cycle s)
  (delay (cycle* s s)))

;;--------------------------------------------------------------------
;; Lazy sequence realization

(define (all ls)
  (let* ((res (force ls)))
    (if (null? res)
        '()
        (let ((v (car res))
              (next-ls (cdr res)))
          (cons v (all next-ls))))))

(define (drop n ls)
  (if (eq? n 0)
      ls
      (let* ((res (force ls)))
        (if (null? res)
            '()
            (let ((next-ls (cdr res)))
              (drop (1- n) next-ls))))))

(define (take n ls)
  (if (eq? n 0)
      '()
      (let* ((res (force ls)))
        (if (null? res)
            '()
            (let ((v (car res))
                  (next-ls (cdr res)))
              (cons v (take (1- n) next-ls)))))))

;;--------------------------------------------------------------------
;; Stern-Brocot tree

(define I
  '(1 0 0 1))

(define (L a b c d)
  (list (+ a b) b (+ c d) d))

(define (R a b c d)
  (list a (+ a b) c (+ c d)))

(define (node->Q* a b c d)
  (/ (+ a b) (+ c d)))

(define (node->Q n)
  (apply node->Q* n))

(define (SB->Q* u n)
  (if (null? u)
      (node->Q n)
      (let ((next (car u)))
        (SB->Q* (cdr u) (apply next n)))))

(define (SB->Q u)
  (SB->Q* u I))

(define (Q->SB* n d)
  (cond
   ((< n d) (cons L (delay (Q->SB* n (- d n)))))
   ((> n d) (cons R (delay (Q->SB* (- n d) d))))
   (else '())))

(define (Q->SB q)
  (let ((n (numerator q))
        (d (denominator q)))
    (delay (Q->SB* n d))))

(define (SSB->Q u)
  (let ((s (car u))
        (u (cdr u)))
    (* s (SB->Q u))))

(define (Q->SSB q)
  (delay
    (if (= q 0)
        '(0)
        (let* ((q (inexact->exact q))
               (n (numerator q))
               (d (denominator q)))
          (cond ((and (fxpositive? n) (fxpositive? d))
                 (cons 1  (Q->SB* n d)))
                ((and (fxpositive? n) (fxnegative? d))
                 (cons -1 (Q->SB* n (- d))))
                ((and (fxnegative? n) (fxpositive? d))
                 (cons -1 (Q->SB* (- n) d)))
                ((and (fxnegative? n) (fxnegative? d))
                 (cons 1  (Q->SB* (- n) (- d)))))))))

(define (sgn u)
  (car u))

;;--------------------------------------------------------------------
;; Special sequences

(define (sqrt-2)
  (delay (cons R (cycle (list L L R R)))))

(define (sgn-sqrt-2)
  (delay (cons 1 (sqrt-2))))

(define (random-sb*)
  (if (eq? (random 2) 1)
      (cons R (delay (random-sb*)))
      (cons L (delay (random-sb*)))))

(define (random-sb)
  (delay (random-sb*)))

(define (random-ssb)
  (delay (cons 1 (random-sb))))

;;--------------------------------------------------------------------
;; Helpers

(define (fmt u)
  (apply string-append
         (map (lambda (n)
                (cond
                 ((eq? n L) "L")
                 ((eq? n R) "R")
                 ((= n -1)  "-")
                 ((= n 1)   "")
                 ((= n 0 )  "0")))
              u)))
