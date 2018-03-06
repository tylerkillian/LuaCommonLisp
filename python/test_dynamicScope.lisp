(defun use-dynamic-scope ()
	(format t "x = ~a~%" x)
)

(setf x 2)
(use-dynamic-scope)

(setf x 5)
(use-dynamic-scope)

