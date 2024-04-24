(defun req(list1 list2 init2)
    (cond ((null list1) T)
          ((null list2) (req list1 init2 init2))
          (T (format T "~a~a " (car list2) (car list1)) (req (cdr list1)
                                                            (cdr list2)
                                                            init2))
    )
)

(defun @printmixt (list1 list2)
    (req list1 list2 list2)
)

(@printmixt '(1 2 3 4 5 6 7) '(a b c d))
