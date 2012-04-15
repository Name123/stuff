#!/usr/bin/sbcl --script


(defparameter *operators-list* (list #\+ #\- #\* #\/ #\^ #\m ))

(define-condition parse-err (error)
  ((text :initarg :text :reader text)))


(defun operator? (char)
  (member char *operators-list*))


(defun space? (char)
  (char= char #\space))

    


(defun read-number (str pos)
  (multiple-value-bind (number next-pos) 
      (parse-integer str :start pos :junk-allowed t)
     (if (and
	  (< next-pos (length str))
	  (char= (elt str next-pos) #\.))
	(multiple-value-bind (number2 last-pos)
	    (parse-integer str :start (+ next-pos 1) :junk-allowed t)
	  (list last-pos (+ number (float (/ number2 (expt 10 (- last-pos next-pos 1)))))))
	(list next-pos number))))


(defun tokenize (str pos res)
   (cond ((>= pos (length str)) res)
	  ((or (operator? (elt str pos))
	       (char= (elt str pos) #\))
	       (char= (elt str pos) #\())
	   (tokenize str (+ pos 1) (cons (elt str pos) res)))
	  ((space? (elt str pos)) (tokenize str (+ pos 1)  res))
	  ((digit-char-p (elt str pos))
	   (let ((temp (read-number str pos)))
	     (tokenize str (car temp) (cons (cadr temp) res))))
	  (t (error 'parse-err :text (format nil "Unknown character at position: ~a" pos)))))

(defun charp (c)
  (lambda (x)
    (and (characterp x) (char= x c))))

	  
(defun convert (str)
   (let ((tokens (reverse (tokenize str 0 '())))
	(result '())
	(stack '()))
     (defun unary? (op)
       (char= op #\m))
     (defun unwind (ch pred)
       (loop while (and (not (null stack)) (funcall pred ch)) do
	    (push (pop stack) result)))
     (defun need-unwind (op)
        (or 
	  (and (unary? op) (> (prior (car stack)) (prior op)))
	  (and (not (unary? op)) (>= (prior (car stack)) (prior op)))))
     (defun prior (op)
	 (cond ((member op (list #\+ #\-)) 2)
	       ((member op (list #\* #\/)) 3)
	       ((char= op #\^) 4)
	       ((char= op #\m) 5)
	       ((char= op #\)) 1)
	       ((char= op #\() 0)))
		 
  (loop 
       for tok in tokens 
       with un-m-expected = t
       do (cond ((numberp tok) (progn (push tok result) (setq un-m-expected nil)))
		  ((funcall (charp #\() tok) (progn (push tok stack) (setq un-m-expected t)))
		  ((operator? tok)
		   (progn
		     (when (and (char= tok #\-) un-m-expected)
			 (setq tok #\m))
		     (unwind tok #'need-unwind)
		     (push tok stack)
		     (setq un-m-expected t)))
		   ((char= tok #\)) (progn
				     (unwind tok #'(lambda (x) (char/= (car stack) #\()))
				     (pop stack)
				     (setq un-m-expected nil)))))
       
     (unwind 1 #'(lambda (x) t))
     (reverse result)))

(defun expr-eval (expr)
  (let ((stack '()))
    (defun stack-pop ()
      (if (null stack)
	  (error 'parse-err :text "Incorrect expression")
	  (pop stack)))
    (defun stack-eval-binary (tok)
      (let ((op1 (stack-pop))
	    (op2 (stack-pop)))
	(cond ((funcall (charp #\+) tok) (push (+ op2 op1) stack))
	      ((funcall (charp #\-) tok) (push (- op2 op1) stack))
	      ((funcall (charp #\*) tok) (push  (* op2 op1) stack))
	      ((funcall (charp #\/) tok)
	       (if (= op1 0) 
		   (error 'eval-error :text "Division by zero") 
		   (push (/ op2 op1) stack)))
	      ((funcall (charp #\^) tok)
	       (if (and (= op2 0) (<= op1 0)) 
		   (error 'eval-error :text "Incorrect power usage") 
		   (push (expt op2 op1) stack)))
	      (t (error 'parse-error :text "Internal error")))))
		   
    (defun stack-eval (tok)
      (if (funcall (charp #\m) tok)
	  (push (- (stack-pop)) stack)
	  (stack-eval-binary tok)))
     (loop for tok in expr
       do (cond ((numberp tok) (push tok stack))
		 ((operator? tok) (stack-eval tok))
		 (t (error 'parse-err :text  "Incorrect expression"))))
     (car stack)))


(handler-case
    (let ((args sb-ext:*posix-argv*))
      (if (< (length args) 2)
	  (format t "Not enough args")
	  (format t "~a" (expr-eval (convert (cadr args))))))
  (Parse-err (se) (format t se))
  (eval-error (se) (format t se)))
 
	  


