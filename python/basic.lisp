(defun rest (x) (cdr x))
(defun caar (x) (car (car x)))
(defun cadr (x) (car (cdr x)))

(defun and (a b)
	(if a (if b t nil) nil)
)

(defun has-cars (lists)
	(if lists
		(and (caar lists) (has-cars (cdr lists)))
		t
	)
)

(defun allcars (lists)
	(if (has-cars lists)
		(cons (caar lists) (allcars (cdr lists)))
		nil
	)
)

(defun has-cdrs (lists)
	(if lists
		(and (cdar lists) (has-cdrs (cdr lists)))
		t
	)
)

(defun allcdrs (lists)
	(if (has-cdrs lists)
		(cons (cdar lists) (allcdrs (cdr lists)))
		nil
	)
)

(defun mapcar (f &rest lists)
	(if (has-cars lists)
		(append
			(apply f (allcars lists))
			(apply #'mapcar f (allcdrs lists))
		)
		nil
	)
)


