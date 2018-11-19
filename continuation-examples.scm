#!/usr/bin/env racket --script
;;------- cps brown
;;------- 2 cps share a stack
(define froz1)
(define froz2)
(let ((x 0))
  (call/cc (lambda (cc)
			 (set! froz1 cc)
			 (set! froz1 cc)))
  (set! x (+ 1 x))
  x)

(froz1 '())
(froz2 '())
;;------------- no-local return example, 
(define cc1 #f)
(+ (call/cc (lambda (return)
			  (set! cc1 return)
			  (* 2 3))) ;; do whatever doesn't matter
   (+ 100))

(cc 10)


(define ecount 0)
(let ((test 7))
  (call/cc (lambda (return)
			 (if (odd? test)
				 (return 5))
			 (set! ecount (+ ecount 1))
			 7)))
;;-------------
(define (test e cc)
  (if (zero? e)
	  (cc "find zero")))

(define (search-zero test lst)
  (call/cc
   (lambda (ret)
	 (for-each
	  (lambda (e) (test e ret) (display e))
	  lst))))

(display (search-zero test '(-3 -2 -1 0 1 2 3)))

(call/cc (lambda (cc)
		   (for-each
			(lambda (e)
			  (test e cc)
			  (display e))
			'(-3 -2 -1  1 2 3))
		   'not-found))

;;------- product cps
(define product
  (lambda (ns)
	(call/cc
	 (lambda (exit)
	   (let f ((ns ns))
		 (cond ((null? ns) 1)
			   ((= (car ns) 0) (exit "zero"))
			   (else (* (car ns) (f (cdr ns))))))))))
(display (product '(1 2 0 3 4)))


;;--------- producer & consumer
(define dish #f)
(define (put! fruit) (set! dish fruit))
(define (get!) (let ((fruit dish))
				 (set! dish #f)
				 fruit))

(define (consumer do-other-stuff)
  (let loop ()
	(when dish
		  (printf "c: get ~a~%" (get!))
		  (set! do-other-stuff (call/cc do-other-stuff))
		  (loop))))

(define (producer do-other-stuff)
  (for-each (lambda (fruit)
			  (put! fruit)
			  (printf "P: put ~a~%" fruit)
			  (set! do-other-stuff (call/cc do-other-stuff)))
			'("a" "b" "c" "d" "e")))

(producer consumer)

;;--------- yin-yang
(let* ((yin
		((lambda (cc) (display #\@) cc) (call-with-current-continuation (lambda (c) c))))
	   (yang
		((lambda (cc) (display #\*) cc) (call-with-current-continuation (lambda (c) c))))
	   (yin yang)))



(define (generate-one-element-at-a-time lst)
  (define (control-state return)
	(for-each
	 (lambda (element)
			  (set! return (call/cc 
							(lambda (resume-here)
									 (set! control-state resume-here)
									 (return element)))))
	 lst)
	(return 'you-fell-off-the-end))

  (define (generator)
	(call/cc control-state))
  
  generator)


(define generate-digit
  (generate-one-element-at-a-time '(0 1 2)))

(generate-digit)


(call-with-values
	(lambda () (values 4 5))
  (lambda (a b) (+ a b)))
