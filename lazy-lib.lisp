(defclass promise ()
  ((expr :initarg :expr
	 :initform nil
	 :accessor expr)
   (done :initform nil
	 :accessor done)))

(defun promisep (x)
  (equal (type-of x) 'promise))

(defmacro promise (form)
  `(make-instance 'promise :expr (lambda () ,form)))

(defgeneric force (expr))
(defmethod force (expr) expr)
(defmethod force ((promise promise))
  (if (done promise)
      (expr promise)
      (setf (done promise) t
	    (expr promise) (funcall (expr promise)))))

;; (set-macro-character #\%
;;     (lambda (stream char)
;;             (declare (ignore char))
;; 	    (list 'force (read stream t nil t))))

(provide 'lazy)