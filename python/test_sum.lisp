(defun sum (n)
	(if (= n 0)
		0
		(+ n (sum (- n 1)))
	)
)

(format t "sum(~a) = ~a~%" 0 (sum 0))
(format t "sum(~a) = ~a~%" 1 (sum 1))
(format t "sum(~a) = ~a~%" 2 (sum 2))
(format t "sum(~a) = ~a~%" 3 (sum 3))
(format t "sum(~a) = ~a~%" 4 (sum 4))
(format t "sum(~a) = ~a~%" 5 (sum 5))

