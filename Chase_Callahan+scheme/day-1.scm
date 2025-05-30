(define (split delimiter str)
  (define char-delim (car (string->list delimiter)))
  (define (make-list accum rem)
    (cond ((null? rem) (list accum))
          ((eq? char-delim (car rem))
           (cons accum (make-list () (cdr rem))))
          (else
            (make-list (append accum (list (car rem))) (cdr rem)))))

   (define list-of-charlists (make-list () (string->list str)))

   (map list->string list-of-charlists))

(define (sort sort-predicate? items)
  (define (place-in-list num accum-list)
    (cond ((null? accum-list) (list num))
          ((sort-predicate? num (car accum-list)) (cons num accum-list))
          (else
            (cons (car accum-list) (place-in-list num (cdr accum-list))))))
  (define (iter sorted-list items)
    (if (null? items)
      sorted-list
      (iter (place-in-list (car items) sorted-list) (cdr items))))

  (iter () items))

(define (read-in-lines file-path transform)
  (define in (open-input-file file-path))
  (define (iter accum)
    (let ((line (read-line in)))
      (if (eof-object? line) 
        accum
        (iter (append accum (list (transform line)))))))
  (iter ()))


(define (lines->lists lines)
  (define (iter rem-pairs list1 list2)
    (if (null? rem-pairs)
      (cons list1 list2)
      (let ((curr-pair (car rem-pairs)))
        (iter 
          (cdr rem-pairs) 
          (append list1 (list (string->number (car curr-pair))))
          (append list2 (list (string->number (cadr curr-pair))))))))

  (define output (iter lines () ()))

  (make-lists (car output) (cdr output)))

(define (make-lists list1 list2) (cons list1 list2))
(define (list1 lists) (car lists))
(define (list2 lists) (cdr lists))

(define lists (lines->lists (read-in-lines "./data/day-1.txt" (lambda (line) (split " " line)))))

(define sorted-lists (make-lists (sort < (list1 lists)) (sort < (list2 lists))))

(reduce 
  + 
  0 
  (map 
    (lambda (a b) (abs (- a b)))
    (list1 sorted-lists)
    (list2 sorted-lists)))
