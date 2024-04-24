(defun s(object lst test d) 
    (cond
        ((null lst) nil)
        ((funcall test object (car lst)) d)
        (T (s object (cdr lst) test (+ d 1)))
    )
)

(defun @position (object lst test)
    (cond ((null test) (s object lst 'equal 0))
          (T (s object lst test 0))
    )
)

(defun sp(test lst d) 
    (cond
        ((null lst) nil)
        ((funcall test (car lst)) d)
        (T (sp test (cdr lst) (+ d 1)))
    )
)

(defun @position-if (test lst)
    (sp test lst 0)  
)

(trace @position)
(trace s)
(trace @position-if)
(trace sp)

(@position '(a b c) '((a) (a b c) (b) d) nil)
(@position '3 '(5 7 -7 4) '>)
(@position-if 'minusp '(1 2 -5 1))
