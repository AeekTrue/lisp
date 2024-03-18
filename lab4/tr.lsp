(defun f(n)
  (cond
    ((> n 0) (g (- n 1)))
    (T 1)
  )
)

(defun g(n)
  (cond
    ((> n 0) (f (- n 1)))
    (T 0)
  )
)
(trace f)
(trace g)
