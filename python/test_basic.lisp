(defun test-assoc-found ()
	(let ( (a '((d . 3) (e . 4) (f . 5))) )
		(assert
			(equal
				(list (assoc 'f a) (assoc 'd a) (assoc 'e a))
        			'((f . 5) (d . 3) (e . 4))
			)
		)
	)
)

(defun test-assoc-not-found ()
	(let ( (a '((d . 3) (e . 4) (f . 5))) )
		(assert (not (assoc 'g a)))
	)
)

(__run-all-tests__)

