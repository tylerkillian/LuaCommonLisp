(let ((input))
(loop until (string= input "")
  do
  (format t ": ")
  (setf input (read-line))
  (format t "You entered: ~a~%" input)
))
