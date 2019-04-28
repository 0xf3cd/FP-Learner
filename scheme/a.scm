(define (fact-let n)
  (let loop((n1 n) (p n))           ; 1
    (if (= n1 1)                    
    p
    (let ((m (- n1 1)))
      (loop m (* p m))))))      ; 2

(define (delete-x ls x)
	(
		let loop((ls_ ls))
		(if (null? ls_)
			'()
			(if (= (car ls_) x)
				(loop (cdr ls_))
				(cons (car ls_) (loop (cdr ls_)))
			)
		)
	)
)

(define (find-x ls x)
	(
		let loop((ls_ ls) (n 0))
		(if (null? ls_)
			#f 
			(if (= x (car ls_))
				n
				(loop (cdr ls_) (+ n 1))
			)
		)
	)
)

(define my-rev (lambda (ls) (
	let loop((ls_ ls) (nls_ '()))
	(if (null? ls_)
		nls_
		(loop (cdr ls_)  (cons (car ls_) nls_))
	)
)))

(define sum (lambda (ls) (
	let loop((ls_ ls) (s 0))
	(if (null? ls_)
		s
		(loop (cdr ls_) (+ s (car ls_)))
	)	
)))

(define (trans str) (
	let loop((ls (string->list str)) (value 0))
	(if (null? ls)
		value
		(loop (cdr ls) (+ 	(* 10 value)
							(- (char->integer (car ls)) 48)))
	)
))

(define (range n) (
	let loop((ls '()) (count n))
	(if (= count 0)
		ls
		(loop (cons (- count 1) ls) (- count 1))
	)
))

(define (fact-letrec n) (
	letrec ((iter (lambda (n1 p)
				(if (= n1 1)
					p
					(
						let ((m (- n1 1)))
						(iter m (* p m))
					)
	))))     ; *
  (iter n n)
))

(define (my-rev ls) (
	letrec ((iter (lambda (ls_ nls) (
		if (null? ls_)
		nls
		(iter (cdr ls_) (cons (car ls_) nls))
	))))
	(iter ls '())
))

(define (sum ls) (
	letrec ((iter (lambda (ls_ value) (
		if (null? ls_)
		value
		(iter (cdr ls_) (+ value (car ls_)))
	))))
	(iter ls 0)
))

(define trans (lambda (str) (
	letrec ((iter (lambda (ls value) (
		if (null? ls)
		value
		(iter (cdr ls) (+ (* 10 value) (- (char->integer (car ls)) 48)))
	))))
	(iter (string->list str) 0)
)))

(define (fact-do n)
  (
  	do ((n1 n (- n1 1)) (p n (* p (- n1 1)))) 
  	((= n1 1) p)
))

(define (my-rev ls) (
	do ((ls_ ls (cdr ls_)) (nls '() (cons (car ls_) nls)))
	((null? ls_) nls)
))

(define sum (lambda (ls) (
	do ((ls_ ls (cdr ls_)) (value 0 (+ value (car ls_))))
	((null? ls_) value)
)))

(define int2char (lambda (c) (
	- (char->integer c) 48
)))

(define trans (lambda (str) (
	do ((ls_ (string->list str) (cdr ls_)) (value 0 (+ (* 10 value) (int2char (car ls_)))))
	((null? ls_) value)
)))

(define double (lambda (ls) (
	map (lambda (x) (* 2 x)) ls
)))

(define (minus la lb) (
	map - la lb
))

(define (filter-even ls) (
	filter (lambda (x) (= 0 (mod x 2))) ls
))

(define (filter-10-100 ls) (
	filter (lambda (x) (and (>= x 10) (<= x 100))) ls
))

(define (square-sum ls) (
	reduce (lambda (x y) (+ y (* x x))) 0 ls
))

(define (sort-sin ls) (
	sort ls (lambda (x y) (> (sin x) (sin y)))
))

(define (sort-length ls) (
	sort ls (lambda (x y) (> (length x) (length y)))
))

(define (apply-square-sum ls) (
	apply + (map (lambda (x) (* x x)) ls)
))

(define (member-if judge ls) (
	cond
	((null? ls) #f)
	((judge (car ls)) ls)
	(else (member-if judge (cdr ls)))
))

(define (has-member judge ls x) (
	cond
	((null? ls) #f)
	((judge (car ls) x) ls)
	(else (has-member judge (cdr ls) x))
))

;(keep-matching-items '(1 2 -3 -4 5) positive?)
(define (match judge ls) (
	if (null? ls)
	'()
	(if (judge (car ls))
		(cons (car ls) (match judge (cdr ls)))
		(match judge (cdr ls))
	)
))

(define (my-map fun . lss)
  (letrec ((iter (lambda (fun lss)
               (if (null? lss)
               '()
               (cons (fun (car lss))
                 (iter fun (cdr lss)))))
					)
       		(map-rec (lambda (fun lss)
              (if (memq '() lss)
              '()
              (cons (apply fun (iter car lss))
                (map-rec fun (iter cdr lss)))))
					))
  (map-rec fun lss))
)

(define (read-file file-name) (
	let ((port (open-input-file file-name)))
	(let loop((ls '()) (c (read-char port)))
		(if (eof-object? c)
		(begin 
			(close-input-port port) 
			(list->string (reverse ls)))
		(loop (cons c ls) (read-char port))))))