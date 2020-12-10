(defun test-addition ()
	(assert (equal 15 (+ 1 2 3 4 5)))
)

(defun test-append () 
	(assert (equal '(1 2 3 4 5) (append (list 1 2 3) (list 4 5))))
)

(__run-all-tests__)

