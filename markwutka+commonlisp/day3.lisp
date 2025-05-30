(ql:quickload :cl-ppcre)
(load "mwlib.lisp")

(defun do-mul (m)
  (ppcre:register-groups-bind ((#'parse-integer a) (#'parse-integer b))
      ("mul[(]([0-9]{1,3}),([0-9]{1,3})[)]" m)
    (* a b)))

(defun day3a ()
  (let* ((line (apply #'concatenate 'string (read-file "data/day3.txt")))
	 (muls (ppcre:all-matches-as-strings
		"mul[(][0-9]{1,3},[0-9]{1,3}[)]" line)))
    (apply #'+ (mapcar #'do-mul muls))))

(defun process-instr (ctx instr)
  (let ((sum (car ctx))
	(on (cdr ctx)))
    (cond
      ((equal instr "do()") (cons sum t))
      ((equal instr "don't()") (cons sum nil))
      (on (cons (+ sum (do-mul instr)) on))
      (t ctx))))

(defun day3b ()
  (let* ((line (apply #'concatenate 'string (read-file "data/day3.txt")))
	 (muls (ppcre:all-matches-as-strings
		"do[(][)]|don't[(][)]|mul[(][0-9]{1,3},[0-9]{1,3}[)]" line)))
    (car (reduce #'process-instr muls :initial-value '(0 . t)))))
