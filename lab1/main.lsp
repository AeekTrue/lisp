(defun test(f p ans)()
    (cond ((= (apply f p) ans) (format t "Test ~a = ~a [OK]~%" (cons f p) ans))
          (T (format t "Test ~a=~a [FAIL]~%" (cons f p) ans))
    )
)

(DEFUN KV(N M)
    (COND
        ((> N M) 0)
        (T (+ (* N N) (KV (+ N 1) M)))
    )
)   

(defun mygcd(a b)
    ( cond
        ((zerop b) a) 
        ( T (mygcd b (mod a b)))
    )
)

(defun mylcm(a b)
    (/ (* a b) (mygcd a b))
)

(defun findroot(x s eps)
    (cond
        ( (<(abs (-(* s s) x)) eps) s)
        ( T (findroot x (- s (/ (- (* s s) x) (* 2 s))) eps))
    )
)

(defun root(x eps)
    (cond ((< x 0) "Некорректное значение")
        (T (findroot x x eps))
    )
)

(defun fib(n)
    (cond
        ((< n 1) "Некорректное значение")
        ((= n 1) 1)
        ((= n 2) 2)
        (T (+ (fib (- n 1)) (fib (- n 2))))
    )
)

(test 'kv '(2 4) 29)
(test 'mygcd '(32 64) 32)
(test 'mylcm '(7 6) 42)
(test 'fib '(3) 3)
(test 'fib '(5) 8)
(format t "sqrt(121) = ~f~%" (root 121 0.001))
(format t "sqrt(10000) = ~f~%" (root 10000 0.001))
(format t "sqrt(16) = ~f~%" (root 16 0.001))
