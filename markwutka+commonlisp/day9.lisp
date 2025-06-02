(load "mwlib.lisp")

(defun split-list (is-left lst block-list free-list pos block-num)
  (cond
    ((not lst) (list (reverse block-list) (reverse free-list)))
    (is-left
     (split-list nil (cdr lst) (cons (list (car lst) pos block-num) block-list)
		 free-list (+ pos (car lst)) (+ 1 block-num)))
    (t (split-list t (cdr lst) block-list
		   (cons (list (car lst) pos) free-list) (+ pos (car lst)) block-num))))

(defun block-sum-range (len pos num)
  (* num (+ (* len pos) (floor (/ (* len (- len 1)) 2)))))

(defun compute-checksum-a (block-sizes free-sizes checksum)
  (if (not block-sizes) checksum
      (destructuring-bind (block-len block-pos block-num) (car block-sizes)
	(if (not free-sizes)
	    (compute-checksum-a (cdr block-sizes) free-sizes
				(+ checksum
				   (block-sum-range block-len block-pos block-num)))
	    (destructuring-bind (free-len free-pos) (car free-sizes)
	      (cond
		((> free-pos block-pos)
		 (compute-checksum-a (cdr block-sizes) ()
				     (+ checksum
					(block-sum-range block-len block-pos block-num))))
		((= free-len block-len)
		 (compute-checksum-a (cdr block-sizes) (cdr free-sizes)
				     (+ checksum
					(block-sum-range block-len free-pos block-num))))
		((< free-len block-len)
		 (compute-checksum-a (cons (list (- block-len free-len) block-pos block-num)
					   (cdr block-sizes))
				     (cdr free-sizes)
				     (+ checksum
					(block-sum-range free-len free-pos block-num))))
		(t
		 (compute-checksum-a (cdr block-sizes)
				     (cons (list (- free-len block-len)
						 (+ free-pos block-len))
					   (cdr free-sizes))
				     (+ checksum
					(block-sum-range block-len free-pos block-num))))))))))

(defun get-free (req-size pos free-list pre)
;  (format t "get-free ~a ~a ~a ~a~%" req-size pos free-list pre)
  (if (not free-list) nil
      (destructuring-bind (free-len free-pos) (car free-list)
	(cond
	  ((and (= free-len req-size) (< free-pos pos))
	   (list free-pos (append (reverse pre) (cdr free-list))))
	  ((and (> free-len req-size) (< free-pos pos))
	   (list free-pos (append (reverse pre)
				  (cons (list (- free-len req-size)
					      (+ free-pos req-size))
					(cdr free-list)))))
	  (t (get-free req-size pos (cdr free-list)
		       (cons (car free-list) pre)))))))

(defun compute-checksum-b (block-sizes free-sizes checksum)
  (if (not block-sizes) checksum
      (destructuring-bind (block-len block-pos block-num) (car block-sizes)
	(if (not free-sizes)
	    (compute-checksum-b (cdr block-sizes) ()
				(+ checksum
				   (block-sum-range block-len block-pos block-num)))
	    (let* ((found-free (get-free block-len block-pos free-sizes ()))
		   (free-pos (car found-free))
		   (free-rest (cadr found-free)))
	      (if (not found-free)
		  (compute-checksum-b (cdr block-sizes) free-sizes
				      (+ checksum
					 (block-sum-range block-len block-pos block-num)))
		  (compute-checksum-b (cdr block-sizes) free-rest
				      (+ checksum
					 (block-sum-range block-len free-pos block-num)))))))))

(defun day9a ()
  (let* ((disk-map (map 'list (lambda (ch) (- (char-code ch) (char-code #\0)))
			(car (read-file "data/day9.txt"))))
	 (parts (split-list t disk-map () () 0 0)))
    (compute-checksum-a (reverse (first parts)) (second parts) 0)))

(defun day9b ()
  (let* ((disk-map (map 'list (lambda (ch) (- (char-code ch) (char-code #\0)))
			(car (read-file "data/day9.txt"))))
	 (parts (split-list t disk-map () () 0 0)))
    (compute-checksum-b (reverse (first parts)) (second parts) 0)))
