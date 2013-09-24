(definerec (fact x) (if (= x 0) 1 (* n (fact (- x 1))))) 
(letrec f (lambda (x) (if (= x 0) 1 (* x (f (- x 1))))) (f 10))
(letrec c (lambda (x y)
			(if (= y 0) 
				1
				(if (= x y)
					1
					(+ (c (- x 1) (- y 1)) (c (- x 1) y)))))
		(c 4 2))

(definerec (c x y)
			(if (= y 0) 
				1
				(if (= x y)
					1
					(+ (c (- x 1) (- y 1)) (c (- x 1) y)))))

(c 5 2)
(c 9 7)


(definerec ones (cons 1 (delay ones)))
(definerec (from n) (cons n (delay (from (+ n 1)))))
(define naturals (from 0))
(define (stream_hd s) (car s))
(define (stream_tl s) (force (cdr s)))

(definerec (nth s n)
  (if (= n 0)
      (stream_hd s)
      (nth (stream_tl s) (- n 1))))

(definerec (from_list l)
  (if (null l)
      ()
      (cons (car l) (delay (from_list (cdr l))))))

(definerec (to_list stream)
  (if (null stream)
      ()
      (cons (car stream) (to_list (stream_tl stream)))))

(definerec (take n s)
  (if (<= n 0) ()
    (if (null s) ()
      (cons (stream_hd s) (take (- n 1) (stream_tl s))))))

(definerec (drop n s)
  (if (<= n 0) s
    (if (null s) s
      (drop (- n 1) (stream_tl s)))))
	  
(take 5 naturals)
(take 5 (force (cdr naturals)))
(drop 5 (stream_tl naturals))

	
(definerec (map f xs)
  (if (null xs)
	  nil
	  (cons (f (car xs)) (map f (cdr xs)))))

(definerec (filter f xs)
  (if (null xs)
	  nil
	  (let x (car xs)
		(if (f x)
			(cons x (filter f (cdr xs)))
			(filter f (cdr xs))))))
		  

(definerec (fold f acc xs)
  (if (null xs)
	  acc
	  (fold f (f acc (car xs)) (cdr xs))))

(definerec (append xs ys)
  (if (null xs)
	  ys
	  (cons (car xs) (append (cdr xs) ys))))

(definerec (quicksort xs)
  (if (null xs)
	  nil
	  (let x (car xs)
		(append 
		 (quicksort (filter (lambda y (< y x)) (cdr xs))) 
		 (cons x (quicksort (filter (lambda y (>= y x)) (cdr xs))))))))


(define y (map (lambda x (+ x 10)) (list 1 2 3 4 5)))
(define z (fold (lambda (x y) (+ x y)) 0 y))

(quicksort (list 2 4 7 1 2 9 0))
(quicksort (list 9 8 7 6 1 2 0 3))
