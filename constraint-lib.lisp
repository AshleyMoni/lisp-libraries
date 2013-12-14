;; INCOMPLETE.

(if (member "NDF" *modules* :test #'equal)
    (cerror "Ignore warning and load file"
	    "NDF-LIB loaded. This is incompatible with CONSTRAINT-LIB."))

; A constraint, when evaluated, produces:

; nil if the constraint has not been violated)
; (list-of (list-of variables)) otherwise. This is a product-of-sums of the offending variable bindings.

; If all the necessary variables for the constraint to be evaluated haven't been bound, it defaults to nil.

(defun isomorphicp (list1 list2) ; Checks if two lists are isomorphic.
  (and (= (length list1) (length list2))
       (every (lambda (x) (member x list1)) list2)))

(defun cull (list) ; Removes any duplicated (isomorphic sub-lists count) sub-lists in a list.
  (if list
      (append (if (notany
		   (lambda (x)
		     (isomorphicp x (car list))) (cdr list))
		  (list (car list))) (cull (cdr list)))))

(defun c-and (&rest r)		 ; And, as applied to Constraint POSs.
  (if r
      (union (cull (car r)) (apply #'c-and (cdr r))
	     :test #'isomorphicp)))
(defun c-or (&rest r)		  ; Or, as applied to Constraint POSs.
  (if (cdr r)
      (cull (apply #'append
	(loop
	   for a in (car r)
	   collect (loop
		      for b in (apply #'c-or (cdr r))
		      collect (union a b)))))
      (car r)))

(defclass constraint ()
  ((vars :accessor vars
	 :initform nil
	 :initarg :vars)
   (fn :accessor fn
       :initform (lambda () ())
       :initarg :fn)))
(defmacro constraint (vars-list &rest exprs)
  `(make-instance 'constraint
     :vars (lambda ()
	     ,(vars-parse vars-list))
     :fn ()))

(defun c-boundp (var)
  (if (member var *bindings*) t))
(defun c-binding (var)
  var)

(defun and-union (list)
  (if (notany #'null list)
      (apply #'union list)))
(defun vars-parse (list)
  `(,(case (car list)
       ((and) 'and-union)
       ((or) 'union)
       (t (car list)))
     ,@(mapcar (lambda (arg)
		 (if (listp arg)
		     (vars-parse arg)
		     `(if (c-boundp ',arg) (list ,arg))))
	       (cdr list))))

(defvar *constraint-store* nil)
(defvar *bindings* nil)
(defvar *domain* nil)

(defun fail (vars &optional (bindings *bindings*))
  ())

(provide 'constraint)