(defun rest (x) (cdr x))
(defun cadr (x) (car (cdr x)))

(defun and (a b)
	(if a (if b t nil) nil)
)

(defun has-cars (lists)
	(and (caar lists) (has-cars (cdr lists)))
)

(defun allcars (lists)
	(if (has-cars lists)
		(cons (caar lists) (allcars (cdr lists)))
		nil
	)
)

(defun has-cdrs (lists)
	(and (cdar lists) (has-cdrs (cdr lists)))
)

(defun allcdrs (lists)
	(if (has-cdrs lists)
		(cons (cdar lists) (allcdrs (cdr lists)))
		nil
	)
)
