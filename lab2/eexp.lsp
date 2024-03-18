(defun mulp(op)
    (cond ((=(apply op '(3 3)) 9) T)
          ((=(apply op '(3 3)) 1) T)
          (T NIL)
    )
)


(defun eexp(e)
    (cond ((null e) 0) 
        ((numberp e) e) 
        ((mulp (cadr e)) (eexp (cons
                                   (apply (cadr e)
                                         '(
                                           (eexp (car e))
                                           (eexp (caddr e))
                                          )
                                   )
                                   (cdddr e)
                               )
                         ))
        (T (+ (eexp (car e)) (eexp (cddr e))))  
    )
)
