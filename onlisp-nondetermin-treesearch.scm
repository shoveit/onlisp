(define family '(zuzong
				 (grandma ante1
						  (ante2 cusion1
								 (cusion2
										  niece1
										  niece2))
						  (mama daughter))
				 (grandpa (papa son1
								son2
								son3
								son4))))

(define (child person family)
  (if (list? family)
	  (if (eq? person (car family))
		  (flatten (cdr family))
		  (flatten (map (lambda (x) (child person x))
						(cdr family))))
	  '()))

;;--------- solution1: explicit traverse
(define (descent n1 n2)
  (if (eq? n1 n2)
	  (list n2)
	  (let ((p (try-paths (kids n1) n2)))
		(if p (cons n1 p) #f))))

(define (try-paths ns n2)
  (if (null? ns)
	  #f
	  (or (descnet (car ns) n2)
		  (try-paths (cdr ns) n2)
		  (try-paths (cdr ns) n2))))

