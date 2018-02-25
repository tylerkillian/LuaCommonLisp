(defun sum (n)
	(if (= n 0)
		0
		(+ n (sum (- n 1)))
	)
)

(format t "sum(0) = ~a~%" (sum 0))
(format t "sum(1) = ~a~%" (sum 1))
(format t "sum(2) = ~a~%" (sum 2))
(format t "sum(3) = ~a~%" (sum 3))
(format t "sum(4) = ~a~%" (sum 4))
(format t "sum(5) = ~a~%" (sum 5))

