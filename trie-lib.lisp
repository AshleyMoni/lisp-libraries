(require 'anaphora)

(defclass trie ()
  ((tag :initarg :tag
	:accessor tag)
   (items :initarg :items
	  :initform nil
	  :accessor items)
   (sub-tries :initarg :sub-tries
	      :initform nil
	      :accessor sub-tries)))
(defmacro make-trie (&rest rest)
  `(make-instance 'trie ,@rest))
(defmethod print-object ((trie trie) stream)
  (format stream "<Trie:~:[~*~;~A ~]~A ~A>"
	  (slot-boundp trie 'tag)
	  (if (slot-boundp trie 'tag) (tag trie))
	  (items trie)
	  (sub-tries trie)))

(defgeneric node-trie (trie s))
(defgeneric node-trie-setf (trie s x))
(defmethod node-trie ((trie trie) s)
  ";;;; (NODE-TRIE trie s)
;; Searches the trie `trie' for the specific sub-trie that holds the elements that fall under the tag-list `s'"
  (unless (listp s) (setf s (coerce s 'list)))
  (if (null s) trie
      (loop
	 for i in (sub-tries trie)
	 if (equal (tag i) (car s))
	 return (node-trie i (cdr s)))))
(defmethod node-trie-setf ((trie trie) s x)
  (unless (listp s) (setf s (coerce s 'list)))
  (if (null s) (progn (cerror "Ignore setf call and continue."
			      "Cannot set root node of ~A to ~A!" trie x) x)
      (aif (member (car s) (sub-tries trie) :key 'tag)
	   (if (cdr s) (node-trie-setf (car it) (cdr s) x)
	       (setf (car it) x))
	 (aprogn (push (make-trie :tag (car s)) (sub-tries trie))
		 (if (cdr s) (node-trie-setf (car it) (cdr s) x)
		     (setf (car it) x))))))
(defsetf node-trie node-trie-setf)

(defgeneric get-trie (trie s))
(defgeneric get-trie-setf (trie s x))
(defmethod get-trie ((trie trie) s)
  (aprogn (node-trie trie s)
	  (values (if it (items it)) (if it t))))
(defmethod get-trie-setf ((trie trie) s x)
  (aif (node-trie trie s)
       (setf (items it) x)
       (setf (node-trie trie s) (make-trie :tag (aprogn (if (listp s) s (coerce s 'list))
							(car (last it)))
					   :items `(,@x)))))
(defsetf get-trie get-trie-setf)

(provide 'trie)