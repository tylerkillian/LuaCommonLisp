(defun sum (n)
	(if (= n 0)
		0
		(+ n (sum (- n 1)))
	)
)

(format t "sum(5) = ~a~%" (sum 5))

