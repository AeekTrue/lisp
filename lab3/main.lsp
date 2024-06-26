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

(defun div(a b)
    (/ (- a (mod a b)) b)
)

(defun factor(x s)
    (cond ((= x s) (list x)) 
          ((zerop (mod x s))
                            (append (list s) (factor (div x s) s)))
          (T (factor x (+ s 1)))
    )
)

(defun factorize(x)
    (factor x 2)
)

(defun rev(l)
    (cond 
        ((atom l) l)
        ((null (cdr l)) (list (rev (car l))))
        (T (append (rev (cdr l)) (list (rev (car l)))))
    )
)

(defun lin(l)
    ;(format t "~a~%" l)
    (cond
        ((null l) nil)
        ((atom (car l)) (cons (car l) (lin (cdr l))))
        (T (append (lin (car l)) (lin (cdr l)))) 
    )
)

(defun inter(l1 l2 res)
    (cond ((and (null l1) (null l2)) res)
          ((null l1) (inter l2 l1 res))
          ((and (member (car l1) l2) (not (member (car l1) res)))
              (inter (cdr l1) l2 (append res (list (car l1)))))
          (T (inter (cdr l1) l2 res))
    )
)

(defun fib(l n d a b)
    (cond
        ((< n 1) nil)
        ((= n 1) '(1))
        ((= n 2) '(1 2))
        ((= n d) l)
        (T (fib (append l (list (+ a b))) n (+ d 1) b (+ a b)))
    )
)

(defun fibo(n)
    (fib '(1 2) n 2 1 2)
)

(run 'factorize '(2684))
(run 'factorize '(13))
(run 'factorize '(98))

(run 'rev '((1 2 3 4)))
(run 'rev '(((1 2) (3 4))))
(run 'rev '(((1 2) (3 (4)))))

(run 'lin '(((1 (2 (3 (4)))))))
(run 'lin '(((1) (2) (3))))
(run 'lin '(((1 2 3) 4)))

(run 'fibo '(5))
(run 'fibo '(100))
(run 'fibo '(2))
(run 'fibo '(-1))

(run 'inter '((1 2 3 4 5 4 6 7) (4 5 6 7 7 8 9) nil))
