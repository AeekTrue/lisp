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
        (when (null (cdr l)) (return (reverse r)))
        (if (funcall f (car l)) (push cnt r))
        (setq cnt (+ cnt 1))
        (pop l)
    )
    )
)

(defun sums(l)
    (let(
            (s (reverse l))
            (r nil)
        )
        (loop
            (when (null s) (return r))
            (if (null r) (push (car s) r ) (push (+ (car s) (car r)) r))
            (pop s)
        )
    )
)

(defun clr(l e)
    (let(
         (r nil)
        )
        (loop
            (when (null l) (return (reverse r)))
            (if (not (eq (car l) e)) (push (car l) r))
            (pop l)
        )
    )
)

(defun unique(l)
    (let(
         (r nil)
        )
        (loop
            (when (null l) (return (reverse r)))
            (push (car l) r)
            (setq l (clr (cdr l) (car l)))
        )
    )
)

(defun stack(l q)
    (let(
         (r nil)
        )
        (loop
            (when (or (null l) (null q)) (return (reverse r)))
            (if (not (eq (car l) (car q))) (push (cons (car l) (car q)) r))
            (pop l)
            (pop q)
        )
    )
)
(trace unique)
(run 'filter_idx '((1 -1 1 -1 1 -1 1 -1) plusp))
(run 'sums '((1 2 3 4 5)))
(run 'clr '((a b c b b d e f b b b h) b))
(run 'unique '((a b a c b d c e d f g h r a b c)))
(run 'stack '((1 2 3 4 5 6 7 8 9) (9 8 7 6 5 4 3 2 1)))
(run 'stack '((1 2 3) (1 4 3)))
(run 'stack '((1 2 3) (1 2 3)))
(run 'stack '((1 2 3 4) (4 3 2)))
