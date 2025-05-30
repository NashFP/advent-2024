(ql:quickload :split-sequence)

(defun read-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
       while line
       collect line)))

(defun split-reduce (curr line)
  (if (= (length line) 0)
      (cons '() (cons (car curr) (cdr curr)))
      (cons (cons line (car curr)) (cdr curr))))

(defun split-groups2 (lines)
  (reduce #'split-reduce (cons "" (reverse lines)) :initial-value '(() . ()) ))

(defun split-groups (lines)
  (split-sequence:split-sequence "" lines :test #'equal))

(defun split-groups-old (lines groups group)
  (cond ((null lines) (reverse (if (null group) groups (cons (reverse group) groups))))
	((= (length (car lines)) 0) (split-groups-old (cdr lines) (cons (reverse group) groups) '()))
	(t (split-groups-old (cdr lines) groups (cons (car lines) group)))))

(defun take (n list)
  (if (or (= n 0) (null list)) '()
      (cons (car list) (take (1- n) (cdr list)))))

(defun take-while (f l)
  (if (null l) '()
      (if (apply f (car l)) (cons (car l) (take-while f (cdr l)))
	  (take-while f (cdr l)))))

(defun drop (n list)
  (if (or (= n 0) (null list)) list
      (drop (1- n) (cdr list))))

(defun tr-sub (ch char-map)
  (let ((match (assoc ch char-map)))
    (if match (cadr match) ch)))

(defun tr (source char-map)
  (map 'string
       (lambda (ch) (tr-sub ch char-map)) source))

(defun iota (n &key (start 0))
  (loop for i from start below (+ n start) collect i))

(defun repeat (x n)
  (labels ((rec (x n acc)
	     (if (= n 0) acc (rec x (1- n) (cons x acc)))))
    (rec x n '())))

(defun cartesian-product (&rest lists)
  (labels ((prod (a b)
	     (mapcan (lambda (bx) (mapcar (lambda (ax)
					    (if (listp bx) (cons ax bx)
						(list ax bx)))
					  a))
		     b)))
    (reduce #'prod lists :from-end t)))

(defun cartesian-product-rev (&rest lists)
  (labels ((prod (a b)
	     (mapcan (lambda (ax) (mapcar (lambda (bx)
					    (if (listp bx) (cons ax bx)
						(list ax bx)))
					  b))
		     a)))
    (reduce #'prod lists :from-end t)))

(defun partial (f &rest args)
  (lambda (&rest others) (apply f (append args others))))

(defun make-grid (lines)
  (make-array (list (length lines) (length (car lines))) :initial-contents lines))

(defun array-coords (arr)
  (apply #'cartesian-product-rev
	 (mapcar #'iota (array-dimensions arr))))

(defun get-at (arr coord &key default)
  (let ((dims (array-dimensions arr))
	(x (second coord))
	(y (first coord)))
    (if (or (< x 0) (< y 0)
	    (>= y (first dims))
	    (>= x (second dims))) default
	    (aref arr y x))))

(defun get-at-dir (arr coord dir &key default)
  (get-at arr (list (+ (car coord) (car dir)) (+ (cadr coord) (cadr dir)))
	  :default default))

(defun coord-from (start-coord dir n)
  (list (+ (car start-coord) (* n (car dir))) (+ (cadr start-coord) (* n (cadr dir)))))

(defun get-line (arr start-coord dir len)
  (let ((coords (mapcar (lambda (n) (coord-from start-coord dir n))
			(iota len))))
    (mapcar (lambda (coord) (get-at arr coord)) coords)))
