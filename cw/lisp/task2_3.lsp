(defun @printmixt(list1 list2 f param)
    (let ((init2 list2))
        (loop
            (when (null list1) (return T))
            (when (null list2) (setq list2 init2))
            (apply f (append param (list (pop list2) (pop list1)))) 
        )
    )
)


(@printmixt '(1 2 3 4 5 6 7) '(a b c d) 'format '(T "~a~a "))
