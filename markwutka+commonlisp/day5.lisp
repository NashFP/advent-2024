(ql:quickload :cl-ppcre)
(load "mwlib.lisp")

(defun parse-orders (b before-table)
  (let ((parts (mapcar #'parse-integer (ppcre:split "[|]" b))))
    (setf (aref before-table (second parts) (first parts)) t)))

(defun order-ok (table lst)
  (if (not lst) t
      (let ((page (car lst)))    
	(and (notany (lambda (p) (aref table page p)) (cdr lst))
	     (order-ok table (cdr lst))))))

(defun mid-element (lst)
  (nth (- (ash (+ 1 (length lst)) -1) 1) lst))

(defun page-list-score (table page-list)
  (let ((lst (mapcar #'parse-integer (ppcre:split "," page-list))))    
    (if (order-ok table lst)
	(mid-element lst)
	0)))

(defun day5a ()
  (let* ((groups (split-groups (read-file "data/day5.txt")))
	 (before-table (make-array '(100 100) :initial-element nil)))
    (mapc (lambda (b) (parse-orders b before-table)) (car groups))
    (apply #'+ (mapcar (lambda (pl) (page-list-score before-table pl)) (cadr groups)))))

(defun next-good (before-table lst)
  (let ((page (car lst)))
    (if (notany (lambda (p) (aref before-table page p)) (cdr lst)) page
	(next-good before-table (cdr lst)))))

(defun fix-order (before-table lst acc)
  (if (not lst) (reverse acc)
      (let ((page (next-good before-table lst)))
	(fix-order before-table (remove page lst) (cons page acc)))))

(defun day5b ()
  (let* ((groups (split-groups (read-file "data/day5.txt")))
	 (before-table (make-array '(100 100) :initial-element nil)))
    (mapc (lambda (b) (parse-orders b before-table)) (car groups))
    (apply #'+
	   (mapcar (lambda (page-list)
		     (let ((pl (mapcar #'parse-integer (ppcre:split "," page-list))))
		       (if (order-ok before-table pl) 0			   
			   (mid-element (fix-order before-table pl ())))))
		   (cadr groups)))))

