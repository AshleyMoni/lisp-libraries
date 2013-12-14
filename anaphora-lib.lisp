(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body body)
  `(aif ,test-form
	(progn ,@body)))

(defmacro awhile (expr &body body)
  `(do ((it ,expr ,expr))
       ((not it))
     ,@body))

(defmacro aprogn (&rest args)
  (cond ((null args) nil)
	((null (cdr args)) (car args))
	(t `(let ((it ,(car args)))
	      (aprogn ,@(cdr args))))))

(defmacro acond (&rest clauses)
  (if (null clauses)
      nil
    (let ((cl1 (car clauses))
	  (sym (gensym)))
      `(let ((,sym ,(car cl1)))
	 (if ,sym
	     (let ((it ,sym)) ,@(cdr cl1))
	   (acond ,@(cdr clauses)))))))

(defmacro alambda (parms &body body)
  `(labels ((self ,parms ,@body))
     #'self))

(defmacro ablock (tag &rest args)
  `(block ,tag
     ,(funcall (alambda (args)
			(case (length args)
			  (0 nil)
			  (1 (car args))
			  (t `(let ((it ,(car args)))
				,(self (cdr args))))))
	       args)))

(defmacro aif* (test &optional then else)
  (let ((win (gensym)))
    `(multiple-value-bind (it ,win) ,test
       (if (or it ,win) ,then ,else))))

(defmacro awhen* (test &body body)
  `(aif* ,test
	 (progn ,@body)))

(defmacro awhile* (test &body body)
  (let ((flag (gensym)))
    `(let ((,flag t))
       (while ,flag
	 (aif* ,test
	       (progn ,@body)
	       (setq ,flag nil))))))

(defmacro acond* (&rest clauses)
  (if (null clauses)
      nil
    (let ((cl1 (car clauses))
	  (val (gensym))
	  (win (gensym)))
      `(multiple-value-bind (,val ,win) ,(car cl1)
	 (if (or ,val ,win)
	     (let ((it ,val)) ,@(cdr cl1))
	   (acond* ,@(cdr clauses)))))))

(provide 'anaphora)