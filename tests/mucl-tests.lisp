(in-package :mucl)

(sb-walker:macroexpand-all 
 '(let* (((:accessors x y) foo))
   (list x y)))

(sb-walker:macroexpand-all 
 '(let* (((:slots x y) foo))
   (list x y)))

(sb-walker:macroexpand-all 
 '(let* (((:accessors x y) foo)
         ((:slots w z) bar))
   (list x y)))
