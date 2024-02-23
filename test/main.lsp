(defun foo (a b c d e f g h)
    (-
      (*(/ (- a b) c) d)
      (*(/ e (- f g)) h)
    )
)

(defun bar (f)
    (car( cdr( cdr f)))
)

;(write (foo 1 2 3 4 5 6 7 8))
;(terpri)
;(write (foo 8 7 6 5 4 3 2 1))
;(terpri)
;(write (bar '(1 2 3)))


(defun lol(a b)
    (apply a b)
)

( write (lol '+ '(1 2 3)))
(terpri)
(format t "~$~%" pi)
(format t "~5$~%" pi)
(format t "a: ~a, b: ~a~%" 1 2)

(defun hello ()
  (format t "hello ")
  (format t "world~%")
)

(hello)
