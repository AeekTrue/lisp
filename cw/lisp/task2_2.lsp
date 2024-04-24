(defun it(list1 list2)
    (let ((init2 list2))
        (loop
            (when (null list1) (return T))
            (when (null list2) (setq list2 init2))
            (format T "~a~a " (pop list2) (pop list1)) 
        )
    )
)

(defun @printmixt (list1 list2)
    (it list1 list2)
)

(@printmixt '(1 2 3 4 5 6 7) '(a b c d))
