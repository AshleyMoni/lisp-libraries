;; SHOULD USE BUILT IN ASSOCIATION LIST FUNCTIONS. NEEDS WORK!

(defun alist++ (alist item)
  (cond ((null alist)
	 `((,item . 1)))
	((equal item (caar alist))
	 `((,item . ,(1+ (cdar alist))) ,@(cdr alist)))
	(t `(,(car alist) ,@(alist++ (cdr alist) item)))))
(defun alist-- (alist item)
  (cond ((null alist)
	 (cerror "Ignore and return nil"
		 "Cannot alist-- ~S from ~S: not found." item alist))
	((equal item (caar alist))
	 (if (= (cdar alist) 1)
	     (cdr alist)
	     `((,item . ,(1- (cdar alist))) ,@(cdr alist))))
	(t `(,(car alist) ,@(alist-- (cdr alist) item)))))
(defun alist-total (alist)
  (if (null alist) 0 (+ (cdar alist) (alist-total (cdr alist)))))
(defun alist-pick (elements &optional (total (alist-total elements)))
";;;;(ALIST-PICK elements &optional (total (alist-total elements)))
;; Uses the alist as a probability density function and picks a random element."
  (do* ((counter (random total) (- counter (cdar alist)))
	(alist elements (cdr alist)))
       ((< counter (cdar alist)) (caar alist))))

(provide 'alist)