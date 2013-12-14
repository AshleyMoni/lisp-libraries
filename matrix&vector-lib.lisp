; MATRICES:

(defclass matrix ()
  ((grid :initform nil
	 :initarg :grid
	 :accessor grid)))
(defgeneric matref (matrix x y))
(defgeneric columns (matrix))
(defgeneric rows (matrix))
(defgeneric split-rows (matrix))

(defmethod matref ((matrix matrix) x y)
  (aref (grid matrix) x y))
(defmethod columns ((matrix matrix))
  (array-dimension (grid matrix) 1))
(defmethod rows ((matrix matrix))
  (array-dimension (grid matrix) 0))
(defmethod split-rows ((matrix matrix))
  (loop
   for i from 0 to (- (rows matrix) 1)
   collect (make-array `(,(columns matrix))
		       :initial-contents
		       (loop
			for j from 0 to (- (columns matrix) 1)
			collect (matref matrix i j)))))
(defmethod split-columns ((matrix matrix))
  (loop
   for i from 0 to (- (columns matrix) 1)
   collect (make-array `(,(rows matrix))
		       :initial-contents
		       (loop
			for j from 0 to (- (rows matrix) 1)
			collect (matref matrix j i)))))

(defun make-matrix (grid-list)
  (make-instance 'matrix
		 :grid
		 (make-array `(,(length grid-list)
			       ,(length (car grid-list)))
			     :initial-contents
			     grid-list)))

(defun zero-matrix (n)
  (make-matrix (loop
		for i from 1 to n
		collect (loop
			 for j from 1 to n
			 collect 0))))
(defun identity-matrix (n)
  (make-matrix (loop
		for i from 1 to n
		collect (loop
			 for j from 1 to n
			 if (= i j)
			 collect 1
			 else
			 collect 0))))

(defun m* (matrix1 matrix2)
  (make-matrix (let ((m1 (split-rows matrix1))
		     (m2 (split-columns matrix2)))
		 (loop
		  for i in m1
		  collect (loop
			   for j in m2
			   collect (.* i j))))))
(defmethod matrix-map ((matrix matrix) (vector vector))
  (make-array `(,(rows matrix))
	      :initial-contents
	      (loop
	       for i in (split-rows matrix)
	       collect (.* i vector))))

(defmethod transpose ((matrix matrix))
  (make-matrix (mapcar (lambda (vector) (coerce vector 'list)) (split-columns matrix))))

(defmethod upper-triangle ((matrix matrix))
  (let ((n (- (rows matrix) 1))
	(m (split-rows matrix))
	(fail nil))
    (loop
     for i from 0 to (- n 1)
     do (loop
	 for j from (+ i 1) to n
	 if (and (zerop (aref (nth i m) i)) (not (zerop (aref (nth j m) i))))
	 do (setf fail t)
	 if (not (zerop (aref (nth i m) i)))
	 do (setf (nth j m) (v- (nth j m) (v* (nth i m) (/ (aref (nth j m) i) (aref (nth i m) i)))))))
    (if fail
	:무
      (make-matrix (loop
		    for i in m
		    collect (coerce i 'list))))))
(defmethod lower-triangle ((matrix matrix))
  (let ((n (- (rows matrix) 1))
	(m (split-rows matrix))
	(fail nil))
    (loop
     for -i from 0 to (- n 1)
     for i = (- n -i)
     do (loop
	 for -j from 1 to i
	 for j = (- i -j)
	 if (and (zerop (aref (nth i m) i)) (not (zerop (aref (nth j m) i))))
	 do (setf fail t)
	 if (not (zerop (aref (nth i m) i)))
	 do (setf (nth j m) (v- (nth j m) (v* (nth i m) (/ (aref (nth j m) i) (aref (nth i m) i)))))))
    (if fail
	:무
      (make-matrix (loop
		  for i in m
		  collect (coerce i 'list))))))

(defmethod inverse-matrix ((matrix matrix))
  (let ((n (- (rows matrix) 1))
	(r (split-rows (identity-matrix (rows matrix))))
	(m (split-rows matrix)))
    (loop
     for i from 0 to (- n 1)
     do (loop
	 for j from (+ i 1) to n
	 do (setf (nth j r) (v- (nth j r) (v* (nth i r) (/ (aref (nth j m) i) (aref (nth i m) i)))))
	    (setf (nth j m) (v- (nth j m) (v* (nth i m) (/ (aref (nth j m) i) (aref (nth i m) i)))))))
    (loop
     for i from 0 to n
     do (setf (nth i r) (v/ (nth i r) (aref (nth i m) i)))
        (setf (nth i m) (v/ (nth i m) (aref (nth i m) i))))
    (loop
     for -i from 0 to (- n 1)
     for i = (- n -i)
     do (loop
	 for -j from 1 to i
	 for j = (- i -j)
	 do (setf (nth j r) (v- (nth j r) (v* (nth i r) (/ (aref (nth j m) i) (aref (nth i m) i)))))
	    (setf (nth j m) (v- (nth j m) (v* (nth i m) (/ (aref (nth j m) i) (aref (nth i m) i)))))))
    (make-matrix (loop
		  for i in r
		  collect (coerce i 'list)))))
(defmethod determinant ((matrix matrix))
  (let ((m (upper-triangle matrix)))
    (if (mu m)
	(progn (setf m (lower-triangle matrix))
	       (if (mu m)
		   (recursive-determinant matrix)
		 (apply '* (loop
			    for i from 0 to (- (rows m) 1)
			    collect (matref matrix i i)))))
      (apply '* (loop
		 for i from 0 to (- (rows m) 1)
		 collect (matref matrix i i))))))

(defmethod sub-matrix ((matrix matrix) x y)
  (make-instance 'matrix :grid
    (make-array `(,(- (rows matrix) 1) ,(- (columns matrix) 1)) :initial-contents
    (loop
     for a from 0 to (- (array-dimension (grid matrix) 0) 1)
     if (not (= a x))
     collect (loop
	      for b from 0 to (- (array-dimension (grid matrix) 1) 1)
	      if (not (= b y))
	      collect (aref (grid matrix) a b))))))
(defmethod recursive-determinant ((matrix matrix))
  (loop
   for x from 0 to (- (columns matrix) 1)
   sum (* (expt -1 x)
	  (matref matrix 0 x)
	  (if (= (columns matrix) 1)
	      1
	      (determinant (sub-matrix matrix 0 x))))))

(defmethod print-object ((matrix matrix) stream)
  (format stream "<Matrix:~s>" (grid matrix)))

; VECTORS:

(defun v+ (&rest rest)
  (let ((vector1 (car rest))
	(rest (cdr rest)))
    (cond ((not (vectorp vector1))
	   :무)
	  ((null rest)
	   vector1)
	  (t (let ((vector2 (apply 'v+ rest)))
	       (if (or (mu vector2) (not (= (length vector1) (length vector2))))
		   :무
		 (coerce (loop
			  for i across vector1
			  for j across vector2
			  collect (+ i j))
			 'vector)))))))
(defun v- (&rest rest)
  (v+ (car rest) (v* (apply 'v+ (cdr rest)) -1)))

(defun v* (vector number)
  (if (and (numberp number) (vectorp vector))
      (coerce (loop for i across vector collect (* number i)) 'vector) :무))
(defun v/ (vector number)
  (v* vector (/ 1 number)))

(defun .* (v1 v2)
  (if (= (length v1) (length v2))
      (do ((i 0 (+ i 1))
	   (term nil (if (= (+ i 1) (length v1)) t nil))
	   (r 0 (+ r (* (aref v1 i) (aref v2 i)))))
	  (term r)) :무))
(defun x* (&rest rest)
  (if (loop
       for i in rest
       never (not (= (length i) (+ 1 (length rest)))))
      (coerce (loop
	       for i from 0 to (length rest)
	       collect
	       (determinant (make-instance
			     'matrix
			     :grid
			     (cons (loop
				    for j from 0 to (length rest)
				    collect (if (= i j) 1 0))
				   (loop
				    for j in rest
				    collect (coerce j 'list)))))) 'vector) :무))

(defun proj (super sub)
  (v* (/ (.* super sub) (.* sub sub)) sub))
(defun perp (super sub)
  (v- super (proj super sub)))

(provide 'matrix&vector)