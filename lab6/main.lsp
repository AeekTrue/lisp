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

(defvar d0 '((a . 1) (b . 2) (c . 3)))
(defvar d1 '((1 . a) (-2 . b) (-3 . c) (4 . d)))
(defvar d2 '((10 . 1) f1 f2))
(defvar d3 '((a . 1) (b . 2) (isa . ((a . 0) (b . 0) (c . 0)))))
(defvar d4 '((10 . 1) f1 f2))

(defun filt(d p)
    (let
        ((r nil))
        (loop
            (when (null d) (return r))
            (when (funcall p (caar d)) (push (car d) r))
            (pop d)
        )
    )
)

(defun cnt(l)
    (let
        ((r nil) (c 0) (ob nil))
        (loop
            (when (null l) (return (reverse r)))
            (setq ob (eval (car l)))
            (setq c 0)
            (push
                (loop
                    (when (null ob) (return c))
                    (when (listp (car ob)) (setq c (+ c 1))) 
                    (setq ob (cdr ob))
                )
                r
            )
            (pop l)
        )
    )
)

(defun get-isa(prop l)
    (if (assoc prop l) (cdr (assoc prop l))
        (if (assoc 'ISA l)
            (get-isa prop (cdr (assoc 'ISA l)))
            nil
        )
    )
)

;(run 'filt '(d1 plusp))
(trace filt)
(trace cnt)
(trace get-isa)

(filt d1 'plusp)
(cnt '(d0 d2 d1))
(get-isa 'a d3)
(get-isa 'c d3)
