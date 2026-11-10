;; test

(defdynfun dynfun-test-1 (x y)
  (+ x y))

(dynfun-test-1 1 2)

(dflet ((dynfun-test-1 (x y)
                       (* x y)))
  (dynfun-test-1 1 2))

(progn
  (print (dynfun-test-1 1 2))
  (dflet ((dynfun-test-1 (x y)
                         (* x y)))
    (print (dynfun-test-1 1 2)))
  (print (dynfun-test-1 1 2)))


;; see that dflambda is needed:
(let (fn)
  (dflet ((dynfun-test-1 (x y)
                         (* x y)))
    (setf fn
          (lambda ()
            (dynfun-test-1 1 2))))
  (funcall fn))

;; correct version using dflambda:
(let (fn)
  (dflet ((dynfun-test-1 (x y)
                         (* x y)))
    (setf fn
          (dflambda ()
            (dynfun-test-1 1 2))))
  (funcall fn))
