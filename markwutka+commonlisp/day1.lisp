(load "mwlib.lisp")
(ql:quickload :cl-ppcre)

(defun split-line (line)
  (mapcar #'parse-integer (ppcre:split "\\s+" line)))

(defun day1a ()
  (let* ((pairs (mapcar #'split-line (read-file "data/day1.txt")))
	 (firsts (sort (mapcar #'car pairs) #'<))
	 (seconds (sort (mapcar #'cadr pairs) #'<)))
    (apply #'+
	   (mapcar (lambda (a b) (abs (- a b))) firsts seconds))))

(defun day1b ()
  (let* ((pairs (mapcar #'split-line (read-file "data/day1.txt")))
	 (firsts (sort (mapcar #'car pairs) #'<))
	 (seconds (sort (mapcar #'cadr pairs) #'<))
	 (count-table (make-hash-table)))
    (mapc (lambda (n) (setf (gethash n count-table)
			    (+ 1 (gethash n count-table 0)))) seconds)
    (apply #'+ (mapcar (lambda (n) (* n (gethash n count-table 0))) firsts))))
