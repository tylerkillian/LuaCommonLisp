(defun repl (filename)
	(with-open-file (f filename :direction :input)
		(let ((c (read-char f nil)))
		(loop while (not (null c)) do
			(format t "~c~%" c)
			(setf c (read-char f nil))
		)
		)
	)
)

(repl "run-all-tests.lsp")
