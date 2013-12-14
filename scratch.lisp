;-;-;-;-;-; Undocumented Functions

;; CHAINS

(defclass chain () ((car :initarg :car) (cdr :initarg :cdr)))
(defun chain (car cdr)
  (make-instance 'chain :car car :cdr cdr))
(defun car (cons)
  (if (equal (type-of cons) 'chain)
      (slot-value cons 'car)
      (cl:car cons)))
(defun cdr (cons)
  (if (equal (type-of cons) 'chain)
      (with-slots (car cdr)
		  cons
		  (chain (apply cdr `(,cons))
			 cdr))
      (cl:cdr cons)))

;------------- CONTINUATIONS

(setq *cont* #'identity)
(defmacro =lambda (parms &body body)
  `#'(lambda (*cont* ,@parms) ,@body))
(defmacro =defun (name parms &body body)
  (let ((f (intern (concatenate 'string
				"=" (symbol-name name)))))
    `(progn
       (defmacro ,name ,parms
	 `(,',f *cont* ,,@parms))
       (defun ,f (*cont* ,@parms) ,@body))))
(defmacro =bind (parms expr &body body)
  `(let ((*cont* #'(lambda ,parms ,@body))) ,expr))
(defmacro =values (&rest retvals)
  `(funcall *cont* ,@retvals))
(defmacro =funcall (fn &rest args)
  `(funcall ,fn *cont* ,@args))
(defmacro =apply (fn &rest args)
  `(apply ,fn *cont* ,@args))

;------------------------
;------------------------ PROLOG
;------------------------

;requires match-lib.lisp, continuations, 

(defmacro with-inference (query &body body)
  `(progn
     (setq *paths* nil)
     (=bind (binds) (prove-query ',(rep_ query) nil)
	    (let ,(mapcar #'(lambda (v)
			      `(,v (fullbind ',v binds)))
			  (vars-in query #'atom))
	      ,@body
	      (fail)))))
(defun rep_ (x)
  (if (atom x)
      (if (eq x '_) (gensym "?") x)
    (cons (rep_ (car x)) (rep_ (cdr x)))))
(defun fullbind (x b)
  (cond ((varsym? x) (aif2 (binding x b)
			   (fullbind it b)
			   (gensym)))
	((atom x) x)
	(t (cons (fullbind (car x) b)
		 (fullbind (cdr x) b)))))

(=defun prove-query (expr binds)
	(case (car expr)
	  (and (prove-and (cdr expr) binds))
	  (or (prove-or (cdr expr) binds))
	  (not (prove-not (cadr expr) binds))
	  (t (prove-simple expr binds))))
(=defun prove-and (clauses binds)
	(if (null clauses)
	    (=values binds)
	  (=bind (binds) (prove-query (car clauses) binds)
		 (prove-and (cdr clauses) binds))))
(=defun prove-or (clauses binds)
	(choose-bind c clauses
		     (prove-query c binds)))
(=defun prove-not (expr binds)
	(let ((save-paths *paths*))
	  (setq *paths* nil)
	  (choose (=bind (b) (prove-query expr binds)
			 (setq *paths* save-paths)
			 (fail))
		  (progn
		    (setq *paths* save-paths)
		    (=values binds)))))
(=defun prove-simple (query binds)
	(choose-bind r *rlist*
		     (implies r query binds)))

(defmacro choose (&rest choices)
  (if choices
      `(progn
	 ,@(mapcar #'(lambda (c)
			`(push #'(lambda () ,c) *paths*))
		    (reverse (cdr choices)))
	 ,(car choices))
    '(fail)))
(defmacro choose-bind (var choices &body body)
  `(cb #'(lambda (,var) ,@body) ,choices))
(defun cb (fn choices)
  (if choices
      (progn
	(if (cdr choices)
	    (push #'(lambda () (cb fn (cdr choices)))
		  *paths*))
	(funcall fn (car choices)))
    (fail)))
(defun fail ()
  (if *paths*
      (funcall (pop *paths*))
    failsym))

(defvar *rlist* nil)
(defmacro <- (con &rest ant)
  (let ((ant (if (= (length ant) 1)
		 (car ant)
	       `(and ,@ant))))
    `(length (conc1f *rlist* (rep_ (cons ',ant ',con))))))
(=defun implies (r query binds)
	(let ((r2 (change-vars r)))
	  (aif2 (match query (cdr r2) binds)
		(prove-query (car r2) it)
		(fail))))
(defun change-vars (r)
  (sublis (mapcar #'(lambda (v)
		      (cons v (symb '? (gensym))))
		  (vars-in r #'atom))
	  r))

(define-modify-macro conc1f (obj)
  (lambda (place obj)
    (nconc place (list obj))))
(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))
(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defmacro with-inference (query &rest body)
  (let ((vars (vars-in query #'simple?)) (gb (gensym)))
    `(OnLisp-with-gensyms ,vars
		   (setq *paths* nil)
		   (=bind (,gb) ,(gen-query (rep_ query))
			  (let ,(mapcar #'(lambda (v)
					    `(,v (fullbind ,v ,gb)))
					vars)
			    ,@body)
			  (fail)))))

(defun gen-query (expr &optional binds)
  (case (car expr)
    (and (gen-and (cdr expr) binds))
    (or (gen-or (cdr expr) binds))
    (not (gen-not (cadr expr) binds))
    (t `(prove (list ',(car expr)
		     ,@(mapcar #'form (cdr expr)))
	       ,binds))))
(defun gen-and (clauses binds)
  (if (null clauses)
      `(=values ,binds)
    (let ((gb (gensym)))
      `(=bind (,gb) ,(gen-query (car clauses) binds)
	      ,(gen-and (cdr clauses) gb)))))
(defun gen-or (clauses binds)
  `(choose
    ,@(mapcar #'(lambda (c) (gen-query c binds))
	       clauses)))
(defun gen-not (expr binds)
  (let ((gpaths (gensym)))
    `(let ((,gpaths *paths*))
       (setq *paths* nil)
       (choose (=bind (b) ,(gen-query expr binds)
		      (setq *paths* ,gpaths)
		      (fail))
	       (progn
		 (setq *paths* ,gpaths)
		 (=values ,binds))))))
(=defun prove (query binds)
	(choose-bind r *rules* (=funcall r query binds)))
(defun form (pat)
  (if (simple? pat)
      pat
    `(cons ,(form (car pat)) ,(form (cdr pat)))))


(defvar *rules* nil)
(defmacro <- (con &rest ant)
  (let ((ant (if (= (length ant) 1)
		 (car ant)
	       `(and ,@ant))))
    `(length (conc1f *rules*
		     ,(rule-fn (rep_ ant) (rep_ con))))))
(defun rule-fn (ant con)
  (OnLisp-with-gensyms (val win fact binds)
		`(=lambda (,fact ,binds)
			  (OnLisp-with-gensyms ,(vars-in (list ant con) #'simple?)
					(multiple-value-bind
					 (,val ,win)
					 (match ,fact
						(list ',(car con)
						      ,@(mapcar #'form (cdr con)))
						,binds)
					 (if ,win
					     ,(gen-query ant val)
					   (fail)))))))


;;??:??

(defun rule-fn (ant con)
  (OnLisp-with-gensyms (val win fact binds paths) ;
		`(=lambda (,fact ,binds ,paths) ;
			  (OnLisp-with-gensyms ,(vars-in (list ant con) #'simple?)
					(multiple-value-bind
					    (,val ,win)
					    (match ,fact
						   (list ',(car con)
							 ,@(mapcar #'form (cdr con)))
						   ,binds)
					  (if ,win
					      ,(gen-query ant val paths) ;
					    (fail)))))))

(defmacro with-inference (query &rest body)
  (let ((vars (vars-in query #'simple?)) (gb (gensym)))
    `(OnLisp-with-gensyms ,vars
		   (setq *paths* nil)
		   (=bind (,gb) ,(gen-query (rep_ query) nil '*paths*) ;
			  (let ,(mapcar #'(lambda (v)
					    `(,v (fullbind ,v ,gb)))
					vars)
			    ,@body)
			  (fail)))))

(defun gen-query (expr binds paths)	;
  (case (car expr)
    (and (gen-and (cdr expr) binds paths)) ;
    (or (gen-or (cdr expr) binds paths)) ;
    (not (gen-not (cadr expr) binds paths)) ;
    (lisp (gen-lisp (cadr expr) binds)) ;
    (is (gen-is (cadr expr) (third expr) binds)) ;
    (cut `(progn (setq *paths* ,paths)	;
		 (=values ,binds)))	;
    (t `(prove (list ',(car expr)
		     ,@(mapcar #'form (cdr expr)))
	       ,binds *paths*))))	;
(=defun prove (query binds paths)	;
	(choose-bind r *rules*
		     (=funcall r query binds paths))) ;

(defun gen-and (clauses binds paths)	;
  (if (null clauses)
      `(=values ,binds)
    (let ((gb (gensym)))
      `(=bind (,gb) ,(gen-query (car clauses) binds paths) ;
	      ,(gen-and (cdr clauses) gb paths))))) ;
(defun gen-or (clauses binds paths)	;
  `(choose
    ,@(mapcar #'(lambda (c) (gen-query c binds paths)) ;
	       clauses)))
(defun gen-not (expr binds paths)	;
  (let ((gpaths (gensym)))
    `(let ((,gpaths *paths*))
       (setq *paths* nil)
       (choose (=bind (b) ,(gen-query expr binds paths) ;
		      (setq *paths* ,gpaths)
		      (fail))
	       (progn
		 (setq *paths* ,gpaths)
		 (=values ,binds))))))
(defmacro with-binds (binds expr)
  `(let ,(mapcar #'(lambda (v) `(,v (fullbind ,v ,binds)))
		 (vars-in expr))
     ,expr))
(defun gen-lisp (expr binds)
  `(if (with-binds ,binds ,expr)
       (=values ,binds)
     (fail)))
(defun gen-is (expr1 expr2 binds)
  `(aif2 (match ,expr1 (with-binds ,binds ,expr2) ,binds)
	 (=values it)
	 (fail)))
