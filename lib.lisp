;; USING INBUILT FUNCTIONS. NEEDS WORK!

(setf *print-length* 10 *print-level* 10)
(defun mu (mu) (equal mu :무))

;;;; The debug character-macro
;; ?<whatever> will macro-expand into (print <whatever>)
;; (set-macro-character #\?
;;     (lambda (stream char)
;;             (declare (ignore char))
;; 	    (list 'print (read stream t nil t))))
;;;; The shell character-macro
;; $(<whatever>) will macro-expand into (captured-shell "<whatever>") [pretty much exactly like Bash's $() evalation]
;; (Note: Cannot handle #\'; avoid its use)
(set-macro-character #\$
    (lambda (stream char)
            (declare (ignore char))
	    (list 'captured-shell (princ-to-string (read stream t nil t)))))
;;;; The delimeter macros
;; [] acts as a quoted list, {} acts as a list with implicit progn
(set-macro-character #\[ (lambda (stream char) (declare (ignore char)) `',(read-delimited-list #\] stream t)))
(set-macro-character #\] (get-macro-character #\)))

(set-macro-character #\{ (lambda (stream char) (declare (ignore char)) `(progn ,@(read-delimited-list #\} stream t))))
(set-macro-character #\} (get-macro-character #\)))

(defun after (x sym)
";;;; (AFTER x sym)
;; Takes a list `x' and an element `sym'...
;; It then searches for the first instance of sym in x and returns the cdr of the cons holding that first sym"
    (loop for a on x if (equal (car a) sym) return (cdr a)))
(defun before (x sym)
";;;; (BEFORE x sym)
;; Takes a list `x' and an element `sym'...
;; It then searches for the first instance of sym in x and returns the list of elements before it"
    (loop for a in x until (equal a sym) collect a))
(defmacro capture (&rest list)
";;;; (CAPTURE &rest list)
;; Takes a list of forms that it evaluates in order, kinda like (progn) or (list)
;; It then grabs all the output that should've gone to *standard-output* and returns it as a string
;; The actual return-value of the last form in `list' is appended as a second return-value [you can catch it if you want]
;; (Note: The stream that has all the captured outputs is called *captured-output*. (capture) is an unhygienic macro"
  `(let ((*captured-output* (make-string-output-stream)))
     (apply 'values
	    (reverse
	     (list (let ((*standard-output* *captured-output*))
		     ,@list)
		   (get-output-stream-string *captured-output*))))))
;; (defun captured-shell (string)
;; ";;;; (CAPTURED-SHELL string)
;; ;; Executes (run-shell-command string) and captures the output to stdout.
;; ;; The lines are individually (read-line)d and concatenated into a list, which is returned."
;;   (let ((stream (run-shell-command string :output :stream))
;; 	(a ""))
;;     (loop
;;      while (setf a (read-line stream nil))
;;      collect a)))
(defmacro data (&rest r)
";;;; (DATA &rest r)
;; Returns the exact function sexp
;; Think of it as a (quote) that's invulnerable to evaluation"
  `'(data ,@r))
(defun except (x e)
";;;; (EXCEPT x e)
;; Takes a list `x' and an element `e'. Returns `x' after removing all occurances of `e'
;; It does not traverse sub-lists in its search for `e'"
    (loop for a in x if (not (equal a e)) collect a))
(defun collapse (x)
";;;; (COLLAPSE x)
;; Locates all repeated elements in string, and eliminates the latter occurances. Returns the resultant list
;; If it locates any sub-lists, it applies (collapse) to them too
;; (Note: May not be very efficient. Consider rewriting)"
    (cond ((null x) nil)
          ((listp (car x)) (cons (collapse (car x)) (collapse (except (cdr x) (car x)))))
          (t (cons (car x) (collapse (except (cdr x) (car x)))))))
(defun input (str)
";;;; (INPUT str) [DEPRECATED]
;; Takes a string and converts it into a list of words
;; FUNCTION HAS BEEN RENDERED OBSOLETE BY (STRING-TO-LIST). It has not been deleted however, because references to it exist within Halcyon. Reccomendation: Rename String-to-list as input, and delete current input"
   (do* ((stringstream (make-string-input-stream str))
         (result nil (cons (string-downcase (symbol-name next)) result))
         (next (read stringstream nil 'eos)
               (read stringstream nil 'eos)))
        ((equal next 'eos) (reverse result))))
(defun interpret ()
";;;; (INTERPRET)
;; Runs a mini-interpreter. Useful for debugging a complicated interactive program"
    (let ((terminator t)
          (x nil))
         (loop
          while terminator
          do
          (format t "~%] ")
          (setf x (read))
          (if (equal x :exit)
              (setf terminator nil)
              (format t "~%=> ~s" (eval x))))))
(defun list-lines-in-file (pathname)
";;;; (LIST-LINES-IN-FILE pathname)
;; takes a pathname `pathname', (read-line)s every line in the file, and concatenates it into a string. Returns the string"
  (with-open-file (stream pathname)
		  (loop
		   for a = ""
		   while (setf a (read-line stream nil))
		   collect a)))
(defun ring (&rest r)
";;;; (RING &rest r)
;; Works identically to (list), except the output is circular; the last element's (cdr) points to the first element"
  (setf (cdr (last r)) r))
(defun string-split (x)
";;;; (STRING-SPLIT x)
;; Converts a string into a list of characters"
    (coerce x 'list))
(defun string-to-list (x &optional (d #\ ))
";;;; (STRING-TO-LIST x &optional d)
;; two inputs: a list of characters `x' [a string-split string] and a demarcator `d' [a character]
;; it parses the characters into separate lists by using the `d's as separators
;; it then coerces them into a string, and splices the lists together
;; So it converts a string-split string into a list of words
;; (Note: `d' is a whitespace by default)"
  (let ((s x))
       (if (loop
	    for a in s
	    never (eq a d))
	   `(,(coerce s 'string))
	   `(,(coerce (before s d) 'string)
	     ,@(string-to-list (after s d) d)))))
(defun substr (s i &optional (j (- (length s) 1)))
";;;; (SUBSTR s i &optional j)
;; returns the substring of `s' from positions `i' to `j'
;; `j' is optional; by default it is the end of the string"
  (coerce (loop
	   for a from i to j
	   collect (char s a))
	  'string))
(defun to (a b)
";;;; (TO x b)
;; Creates a list of natural numbers from `a' to `b'"
  (if (<= a b)
      (loop for x from a to b collect x)
      (reverse (loop for x from b to a collect x))))

(provide 'default)