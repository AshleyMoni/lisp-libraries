(defun curry (arity fn &optional (args nil))
  (setf fn (coerce fn 'function))
  (if (zerop arity)
      (apply fn args)
      (lambda (&rest r)
	(if (> (length r) arity)
	    (apply (curry 0 fn (ldiff r (nthcdr arity r)))
		   (nthcdr arity r))
	    (curry (- arity (length r)) fn (append args r))))))

(provide 'curry)