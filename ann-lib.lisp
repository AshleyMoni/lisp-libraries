;; NEEDS WORK.

(defclass node ()
  (output :initform nil
	  :initarg :output
	  :accessor output))
(defclass functional-node (node)
  ((inputs :initarg :inputs
	   :initform nil
	   :accessor inputs)
   (weights :initarg :weights
	    :initform nil
	    :accessor weights)
   (fn :initarg :fn
       :initform (lambda () (inputs weights) 0)
       :accessor fn)
   (delta :initarg :delta
	  :initform 0
	  :accessor delta)))
(defclass perceptron (functional-node)
  ((fn :initform (lambda (inputs weights)
		   (apply '+ (mapcar '* weights
		      (loop
		       for i in inputs
		       collect (evaluate i))))))))
(defclass sigmoid (functional-node)
  ((fn :initform (lambda (inputs weights)
		   (/ 1 (+ 1 (exp (* -1 (apply '+ (mapcar '* weights
		      (loop
		       for i in inputs
		       collect (evaluate i))))))))))))

(defmethod evaluate ((node functional-node))
  (if (null (output node))
      (setf (output node) (funcall (fn node) (inputs node) (weights node)))
    (output node)))
(defmethod evaluate ((node node))
  (resolve (output node)))
(defmethod reset ((node functional-node))
  (setf (output node) nil)
  (loop
   for i in (inputs node)
   do (reset i)))
(defmethod reset (node node))