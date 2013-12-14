; INCOMPLETE.

(defclass binary-tree ()
  ((key :accessor key
	:initarg :key)
   (value :accessor value
	  :initarg :value)
   (left :accessor left
	 :initarg :left
	 :initform nil)
   (right :accessor right
	  :initarg :right
	  :initform nil)))

(defgeneric insert-tree (item tree))
(defgeneric search-tree (item tree))
(defgeneric delete-tree (item tree))
(defgeneric traverse-tree (tree &optional order))
(defgeneric balance-tree (tree))

(provide 'tree)