#!/usr/bin/sbcl --script

;(format t (caddr sb-ext:*posix-argv*))
;(defun convert (str)
 ; (let ((result #()))
  ;  (loop for ch across str with stack '()
;	  if (null? stack)

(defparameter *operators-list* (list #\+ #\- #\* #\/ #\^ #\( #\)))

(define-condition unknown-character (error)
  ((text :initarg :text :reader text)))

(defun char->digit (char)
  (- (char-int char) 48))

(defun operator? (char)
  (member char *operators-list*))

(defun number? (char)
  (and (char>=  char #\0) (char<= char #\9)))

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
	  ((operator? (elt str pos)) 
	   (tokenize str (+ pos 1) (cons (elt str pos) res)))
	  ((space? (elt str pos)) (tokenize str (+ pos 1)  res))
	  ((number? (elt str pos))
	   (let ((temp (read-number str pos)))
	     (tokenize str (car temp) (cons (cadr temp) res))))
	  (t (error 'unknown-character :text (format nil "Unknown character at position: ~a" pos)))))
	    
;(tokenize "(1+2)*(3.55-6.88+(1))" 0 '())

