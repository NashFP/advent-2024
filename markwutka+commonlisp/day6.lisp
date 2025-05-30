(load "mwlib.lisp")

(defun find-guard (grid)
  (let ((coords (array-coords grid)))
    (find-if (lambda (coord) (eq (get-at grid coord) #\^)) coords)))

(defun count-visited (visited)
  (apply #'+ (mapcar (lambda (c) (if (get-at visited c) 1 0))
		     (array-coords visited))))

(defun turn-right (dir)
  (cond
    ((equal dir '(-1 0)) '(0 1))
    ((equal dir '(0 1)) '(1 0))
    ((equal dir '(1 0)) '(0 -1))
    (t '(-1 0))))

(defun move (coord dir)
  (list (+ (car coord) (car dir))
	(+ (cadr coord) (cadr dir))))

(defun do-guard-path (grid visited guard-coord dir)
  (setf (aref visited (car guard-coord) (cadr guard-coord)) t)
  (let* ((next-coord (move guard-coord dir))
	 (grid-char (get-at grid next-coord :default nil)))
    (cond
      ((not grid-char) visited)
      ((eq grid-char #\#) (do-guard-path grid visited guard-coord (turn-right dir)))
      (t (do-guard-path grid visited next-coord dir)))))

(defun day6a ()
  (let* ((grid (make-grid (read-file "data/day6.txt")))
	 (visited (make-array (array-dimensions grid) :initial-element nil))
	 (guard-coord (find-guard grid)))
    (setf (aref grid (car guard-coord) (cadr guard-coord)) #\.)
    (count-visited (do-guard-path grid visited guard-coord '(-1 0)))))

(defun guard-path-loops (grid visited guard-coord dir)
  (if (gethash (cons guard-coord dir) visited) t
      (progn
	(setf (gethash (cons guard-coord dir) visited) t)
	(let* ((next-coord (move guard-coord dir))
	       (grid-char (get-at grid next-coord :default nil)))
	  (cond
	    ((not grid-char) nil)
	    ((eq grid-char #\#) (guard-path-loops grid visited guard-coord (turn-right dir)))
	    (t (guard-path-loops grid visited next-coord dir)))))))

(defun try-obstacle (grid guard-coord dir obst-coord)
  (setf (aref grid (car obst-coord) (cadr obst-coord)) #\#)
  (let ((result (if (guard-path-loops grid (make-hash-table :test 'equal) guard-coord dir) 1 0)))
    (setf (aref grid (car obst-coord) (cadr obst-coord)) #\.)
    result))

(defun day6b ()
  (let* ((grid (make-grid (read-file "data/day6.txt")))
	 (guard-coord (find-guard grid))
	 (obst-coords (remove-if-not (lambda (c) (eq #\. (aref grid (car c) (cadr c))))
				     (array-coords grid))))
    (setf (aref grid (car guard-coord) (cadr guard-coord)) #\.)
    (apply #'+ (mapcar (lambda (oc) (try-obstacle grid guard-coord '(-1 0) oc))
		       obst-coords))))
