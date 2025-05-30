(ql:quickload :cl-ppcre)
(load "mwlib.lisp")

(defun parse-line (line)
  (mapcar #'parse-integer (ppcre:split "\\s+" line)))

(defun ok-pos (n)
  (and (>= n 1) (<= n 3)))

(defun ok-neg (n)
  (and (<= n -1) (>= n -3)))

(defun is-safe (l)
  (let ((diffs (mapcar #'- l (cdr l))))
    (or (every #'ok-pos diffs)
	(every #'ok-neg diffs))))

(defun remove-nth (n l)
  (nconc (take n l) (drop (+ n 1) l)))

(defun is-safe2 (l)
  (let ((lists (cons l (mapcar (lambda (n) (remove-nth n l))
			       (iota (length l))))))
    (some #'is-safe lists)))

(defun day2a ()
  (let* ((lines (mapcar #'parse-line (read-file "data/day2.txt"))))
    (length (remove-if-not #'is-safe lines))))

(defun day2b ()
  (let* ((lines (mapcar #'parse-line (read-file "data/day2.txt"))))
    (length (remove-if-not #'is-safe2 lines))))

