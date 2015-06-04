"(begin (define f (lambda (x) (+ x 10) )) (define result ( f (car '(50 34 5) ))) results)"
"(begin (define f 5) (let ((f 2)) (set! f 10)) f)"

(begin 
	(define fat 
		(lambda (n) 
			(if (eqv? n 0) 1 
				(* n (fat (- n 1))))))
	(lt? (fat 5) 100))

"(begin (define fat (lambda (n) (if (eqv? n 0) 1 (* n (fat (- n 1)))))) (lt? (fat 5) 100))"

(begin
  (define x 10)
  (let ((x 5) (y (* x 2)))
  (+ x y))
)
"(begin (define x 10) (let ((x 5) (y (* x 2))) (+ x y)))"


(begin 
    (define x 5) 
    (define y 10) 
    (define res -1) 
    (let ((x 8)) 
        (begin  
            (set! res 10) 
            (let ((z 5) (y 21)) 
                (begin 
                    (set! x 3) 
                    (set! y 12) 
                ) 
            ) 
            (set! y x) 
        ) 
    )
    (define lista (cons x (cons y (cons res '()))))
) 
"(begin (define x 5) (define y 10) (define res -1) (let ((x 8)) (begin (set! res 10) (let ((z 5) (y 21)) (begin (set! x 3) (set! y 12) ) ) (set! y x) ) ) (define lista (cons x (cons y (cons res '())))))"


(begin
  (let ((i 1))
    (define f 
      (make-closure 
        (lambda (y) 
          (begin (set! i (+ i y)) i) 
        )
      )
    )
  )
  (define val1 (f 1))
  (define val2 (f 2))
  (+ val1 val2) 
)
"(begin (let ((i 1)) (define f (make-closure (lambda (y) (begin (set! i (+ i y)) i) )))) (define val1 (f 1)) (define val2 (f 2)) (+ val1 val2))"

"(begin (let ((i 1)) (define f (make-closure (lambda (y) (begin (set! i (+ i y)) i) )))) f)"

(begin 
	(define partition (lambda (compare l1) 
		(if (eqv? l1 '()) '() 
			(if (compare (car l1))
	    		(cons (car l1) (partition compare (cdr l1))) 
	    		(partition compare (cdr l1))))))
	    (define quicksort (lambda (l1) 
	    (if (eqv? l1 '())
	    	'() 
	    	(let ((pivot (car l1))) 
	    		(append 
	    			(append 
	    				(quicksort 
	    					(partition (lambda (x) ( lt? x pivot)) l1)) 
	    				(partition (lambda (x) ( eqv? x pivot)) l1))
	    			(quicksort (partition (lambda (x) ( lt? pivot x)) l1)))))))
	    (quicksort '(9 8 7 6 5 4 3 2 1)))

(if (eqv? '() (quote ())))
"(begin (define partition (lambda (compare l1) (if (eqv? l1 '()) '() (if (compare (car l1)) (cons (car l1) (partition compare (cdr l1))) (partition compare (cdr l1)))))) (define quicksort (lambda (l1) (if (eqv? l1 '()) '() (let ((pivot (car l1))) (append (append (quicksort (partition (lambda (x) ( lt? x pivot)) l1)) (partition (lambda (x) ( eqv? x pivot)) l1)) (quicksort (partition (lambda (x) ( lt? pivot x)) l1))))))) (quicksort '(9 8 7 6 5 4 3 2 1)))"

(begin
	(define sum 
		(lambda (n)
			(if (eqv? n 0) 0
			(+ n sum(- n 1))
			)
		)
	)
	(define x 10)
	(let (f 5)
		(set! x 20)
		(* x f)
	)
	(let (f 20)
		(set! x 200)
		(/ x f)
	)
	(lt? (sum x) 1000)
)
"(begin (define sum (lambda (n) (if (eqv? n 0) 0 (+ n sum(- n 1))))) (define x 10) (let (f 5) (set! x 20) (* x f)) (let (f 20) (set! x 200) (/ x f)) (lt? sum(x) 1000))"

(begin
	(define sumList
		(lambda (ls)
			(if (eqv? car(ls) '()) 0
			(+ car(ls) sumList(cdr(ls)))
			)
		)
	)
	(define f (cons 5 (2 3 4)))
	(lt? sumList(f) 14)
)
"(begin (define sumList (lambda (ls) (if (eqv? car(ls) '()) 0 (+ car(ls) sumList(cdr(ls)))))) (define f (cons 5 (2 3 4))) (lt? sumList(f) 14))"

