;; NOTE: This library needs to be rewritten to use collector-lib.lisp

;;;;;; Nondeterministic functions

;; A global variable and a constant are defined here: *paths* and :무
;; :무 is used as a failure signal

(defvar *paths* nil
";; *paths* is used as a global stack for recursively checking the next element in the choose|choose-bind|cb functions. It's global in order to allow the nondeterministic blocks to be nested")
(defmacro choose ((&rest bindings) &rest body)
";;;; (CHOOSE (&rest bindings) &rest body)
;; Format similar to that of (let). However, instead of scoping a dynamic variable using the format {symbol value}, a symbol is assigned a list of possible states it can be {symbol list}. Every time (fail) is called, the most recent variable of the most recently created choose-block is reassigned the next value in its list, and the choose-block re-evaluated. If a list is exhausted, the symbol is reset to its original list and the symbol above it in the declaration must progress to its cdr. In this way, the block iterates through all the possible combinations of the elements in the lists given until it finds a solution that does not signal (fail). If no solution is found, a :무 is returned
;; See Library.lisp for more information on Nondeterministic Functions"
    (if bindings
        `(choose-bind ,(caar bindings) ,(cadar bindings) (choose ,(cdr bindings) ,@body))
        `(progn ,@body)))
(defmacro choose-bind (var choices &body body)
";;;; (CHOOSE-BIND var choices &body body)
;; See (choose)"
    `(cb #'(lambda (,var) ,@body) ,choices))
(defun cb (fn choices)
";;;; (CB fn choices)
;; See (choose), and see Library.lisp for more information on Nondeterministic Functions"
    (if choices
        (progn (if (cdr choices)
                   (push #'(lambda () 
                                   (cb fn (cdr choices)))
                         *paths*))
               (funcall fn (car choices)))
        (fail)))
(defun fail (&optional (n 1))
";;;; (FAIL &optional (n 1))
;; Backtracks `n' times through *paths*
;; See (choose), and see Library.lisp for more information on Nondeterministic Functions"
    (cond ((> n 1)
	   (progn (pop *paths*) (fail (- n 1))))
	  (*paths*
	   (funcall (pop *paths*)))
	  (t :무)))
(defun epic-fail ()
";;;; (EPIC-FAIL)
;; Completely purges *paths* without evaluating any of the paths, and returns `:무'"
  (setf *paths* nil)
  :무)
(defmacro exhaust (fn)
";;;; (EXHAUST fn)
;; This macro evaluates `fn' and continuously calls (fail) until `:무' is reached
;; It then concatenates all these outputs into a list and returns it"
  `(do ((i ,fn (fail))
	(result nil (cons i result)))
       ((mu i) result)))

(provide 'ndf)