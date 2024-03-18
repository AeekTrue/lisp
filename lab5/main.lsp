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

(defun filter_idx(l f)
    (let(
            (cnt 0)
            (r nil)
        )
        (loop
            (when (null (cdr l)) (return r))
            (if (funcall f (car l)) (push cnt r))
            (setq cnt (+ cnt 1))
            (pop l)
        )
    )
)
(run 'filter_idx '((1 -1 1 -1 1 -1 1 -1) plusp))
