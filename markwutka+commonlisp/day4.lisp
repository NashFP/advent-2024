(load "mwlib.lisp")

(defun has-word (arr start-coord dir word)
  (if (equal (get-line arr start-coord dir 4) word) 1 0))

(defun check-coord (arr start-coord word)
  (let ((dirs '((0 1) (0 -1) (1 0) (-1 0) (1 1) (-1 -1)
		 (1 -1) (-1 1))))
    (apply #'+ (mapcar (lambda (dir) (has-word arr start-coord dir word)) dirs))))

(defun day4a ()
  (let* ((grid (make-grid (read-file "data/day4.txt")))
	 (coords (array-coords grid))
	(xmas (list #\X #\M #\A #\S)))
    (apply #'+
	   (mapcar (lambda (coord) (check-coord grid coord xmas)) coords))))

(defun has-xmas-pattern (arr coord)
  (let ((ul (get-at-dir arr coord '(-1 -1)))
	(ur (get-at-dir arr coord '(-1 1)))
	(ll (get-at-dir arr coord '(1 -1)))
	(lr (get-at-dir arr coord '(1 1))))
    (if (and (eq (get-at arr coord)#\A)
	     (or (and (eq ul #\M) (eq lr #\S))
		 (and (eq ul #\S) (eq lr #\M)))
	     (or (and (eq ur #\M) (eq ll #\S))
		 (and (eq ur #\S) (eq ll #\M))))
	1 0)))

(defun day4b ()
  (let* ((grid (make-grid (read-file "data/day4.txt")))
	 (coords (array-coords grid)))
    (apply #'+ (mapcar (lambda (coord) (has-xmas-pattern grid coord))
		       coords))))
