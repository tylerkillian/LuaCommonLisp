(setf x 'a)
(setf a 1)
(setf y 'b)
(setf b 2)
(format t "~a~%" ``(w ,x ,,y))
(format t "~a~%" `(w ,x ,b))

