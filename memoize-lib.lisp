; NEEDS WORK; repetitive code.

(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    (lambda (&rest args)
      (multiple-value-bind (val win) (gethash args cache)
	(if win val
	    (setf (gethash args cache)
		  (apply fn args)))))))

(defmacro defmem (name lambda-list &rest body)
  `(let ((cache (make-hash-table :test #'equal)))
     (defun ,name ,lambda-list
       (multiple-value-bind (val win) (gethash (list ,@lambda-list) cache)
	 (if win val
	     (setf (gethash (list ,@lambda-list) cache)
		   ,(if (null (cdr body))
			(car body)
			`(progn ,@body))))))))

(provide 'memoize)