(ql:quickload :cl-ppcre)
(load "mwlib.lisp")

(defun matches-target (curr nums)
  (if (not (cdr nums)) (eq curr (car nums))
      (if (and (= 0 (mod curr (car nums)))
	       (matches-target (floor (/ curr (car nums))) (cdr nums))) t
	       (if (> curr (car nums))
		   (matches-target (- curr (car nums)) (cdr nums))
		   nil))))

(defun can-unsplice (a b)
  (let ((b-pow (ceiling (log b 10))))
    (= b (mod a (expt 10 b-pow)))))

(defun unsplice (a b)
  (let ((b-pow (ceiling (log b 10))))
    (floor (/ a (expt 10 b-pow)))))

(defun matches-target-b (curr nums)
  (if (not (cdr nums)) (eq curr (car nums))
      (if (and (= 0 (mod curr (car nums)))
	       (matches-target-b (floor (/ curr (car nums))) (cdr nums))) t
	       (if (and (> curr (car nums))
			(matches-target-b (- curr (car nums)) (cdr nums))) t
			(if (can-unsplice curr (car nums))
			    (matches-target-b (unsplice curr (car nums)) (cdr nums))
			    nil)))))

(defun process-eqn (matcher line)
  (let* ((parts (ppcre:split ": " line))
	 (target (parse-integer (car parts)))
	 (nums (reverse (mapcar #'parse-integer (ppcre:split "\\s+" (cadr parts))))))
    (if (funcall matcher target nums) target 0)))

(defun day7a ()
  (let* ((lines (read-file "data/day7.txt")))
    (apply #'+ (mapcar (partial #'process-eqn #'matches-target) lines))))

(defun day7b ()
  (let* ((lines (read-file "data/day7.txt")))
    (apply #'+ (mapcar (partial #'process-eqn #'matches-target-b) lines))))
