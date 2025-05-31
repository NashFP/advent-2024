(load "mwlib.lisp")

(defun add-coord (grid antenna-table coord)
  (let ((ch (get-at grid coord)))
    (when (not (eq ch #\.))
	(setf (gethash ch antenna-table)
	      (cons coord (gethash ch antenna-table))))))

(defun make-antenna-table (grid antenna-table)
  (let ((coords (array-coords grid)))
    (mapc (partial #'add-coord grid antenna-table) coords)
    antenna-table))


(defun count-antinodes (grid)
  (apply #'+
	 (mapcar (lambda (c) (if (eq (get-at grid c) #\#) 1 0))
		 (array-coords grid))))

(defun add-antinode (grid coord-pair)
  (let* ((a (car coord-pair))
	 (b (cadr coord-pair))
	 (diff (add-pair b (scalar-pair -1 a))))
    (when (not (equal a b))
      (set-at grid (add-pair b diff) #\#)
      (set-at grid (add-pair a (scalar-pair -1 diff)) #\#))))

(defun add-antenna-antinodes (grid adder key coords)
  (mapc (partial adder grid) (cartesian-product coords coords)))

(defun compute-antinodes (grid antenna-table adder)
  (maphash (partial #'add-antenna-antinodes grid adder) antenna-table))

(defun day8a ()
  (let* ((grid (make-grid (read-file "data/day8.txt")))
	 (antenna-table (make-antenna-table grid (make-hash-table :test 'eq))))
    (compute-antinodes grid antenna-table #'add-antinode)
    (count-antinodes grid)))

(defun factor-diff (diff)
  (let ((divisor (gcd (car diff) (cadr diff))))
    (list (floor (/ (car diff) divisor))
	  (floor (/ (cadr diff) divisor)))))

(defun mark-while-valid (grid point diff)
  (let ((new-point (add-pair point diff))
	(grid-bounds (array-dimensions grid)))
    (when (and (>= (car new-point) 0)
	     (>= (cadr new-point) 0)
	     (< (car new-point) (car grid-bounds))
	     (< (cadr new-point) (cadr grid-bounds)))
      (set-at grid new-point #\#)
      (mark-while-valid grid new-point diff))))

(defun add-antinode-b (grid coord-pair)
  (let* ((a (car coord-pair))
	 (b (cadr coord-pair))
	 (diff (factor-diff (add-pair b (scalar-pair -1 a)))))
    (when (not (equal a b))
      (mark-while-valid grid a diff)
      (mark-while-valid grid a (scalar-pair -1 diff)))))

(defun day8b ()
  (let* ((grid (make-grid (read-file "data/day8.txt")))
	 (antenna-table (make-antenna-table grid (make-hash-table :test 'eq))))
    (compute-antinodes grid antenna-table #'add-antinode-b)
    (count-antinodes grid)))
