(defun -abs (n)
	(if (typep n 'complex)
		(sqrt (+ (expt (realpart n) 2) (expt (imagpart n) 2)))
		(if (< n 0) (- n) n)))

