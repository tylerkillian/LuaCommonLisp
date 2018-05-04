(defun rest (x) (cdr x))
(defun caar (x) (car (car x)))
(defun cadr (x) (car (cdr x)))
(defun cdar (x) (cdr (car x)))

(defun and (a b)
        (if a (if b t nil) nil)
)

(defun not (x)
	(if x nil t)
)

(defun has-cars (lists)
        (if lists
                (and (caar lists) (has-cars (cdr lists)))
                t
        )
)

(defun get-all-cars (lists)
        (if lists
                (if (has-cars lists)
                        (cons (caar lists) (get-all-cars (cdr lists)))
                        nil
                )
                nil
        )
)

(defun has-cdrs (lists)
        (if lists
                (and (cdar lists) (has-cdrs (cdr lists)))
                t
        )               
)

(defun get-all-cdrs (lists)
        (if lists
                (if (has-cdrs lists)
                        (cons (cdar lists) (get-all-cdrs (cdr lists)))
                        nil
                )
                nil
        )
)

(defun mapcar (f &rest lists)
        (if lists
                (if (has-cars lists)
                        (cons
                                (apply f (get-all-cars lists))
                                (apply #'mapcar f (get-all-cdrs lists))
                        )
                        nil
                )
                nil
        )
)

(defun assoc (key alist)
	(if alist
		(if (eql (caar alist) key)
			(car alist)
			(assoc key (cdr alist))
		)
		nil
	)
)

(defun reverse (sequence)
        (if sequence
                (append (reverse (cdr sequence)) (list (car sequence)))
                nil
        )
)

