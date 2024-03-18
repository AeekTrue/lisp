(defun test(f p ans)()
    (cond ((= (apply f p) ans) (format t "Test ~a = ~a [OK]~%" (cons f p) ans))
          (T (format t "Test ~a = ~a [FAIL]~%" (cons f p) ans))
    )
)

(defun run(f p)
    (format t "~a -> ~a~%" (cons f p) (apply f p))
)

(defun fmt(msg val)
    (format t "~a ~a~%" msg val)
)

;(defun cnt(n)
;    (setq c 0)
;    (setq r nil)
;    (loop ((= c n) r)
;          (push c r)
;          (setq c (+ c 1))
;    )
;)

(defun series(x eps)
    (let(
        (s 0)
        (f 1)
        (k 0)
        (p 0))
    (loop
        (setq p ( / x f))
        (when (< p eps) (return s))
        (setq k (+ k 1))
        (setq f (* f k))
        (setq s (+ s p))
    ))
)

(defun lst(l)
    (loop
        (when (null (cdr l)) (return (car l)))
        (setq l (cdr l))
    )
)

(defun rev(l)
    (let ((r nil))
    (loop
        (when (null l) (return r)) 
        (cond
            ((atom (car l)) (push (pop l) r ))
            (T (push (rev (pop l)) r))
        )
    ))
)

(defun filt(l a b)
    (let ((r nil))
    (loop
        (when (null l) (return r))
        (when (and (>= (car l) a) (<= (car l) b)) (setq r (append r (list (car l)))))
        (pop l)
    ))
)

(format T "~%Series: ~F~%" (series 1 0.0000001))
(run 'lst '((1 2 4 3 5 6 8 7)));
(run 'rev '(((1 2 (3 4 (5 6))))))
(run 'filt '((1 2 3 4 5 6 7 8 9) 3 6))
;(defun seq(x eps)

