;;;; util.lisp
;;;;
;;;; Copyright 2018 Alexander Gutev
;;;;
;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation
;;;; files (the "Software"), to deal in the Software without
;;;; restriction, including without limitation the rights to use,
;;;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;;;; copies of the Software, and to permit persons to whom the
;;;; Software is furnished to do so, subject to the following
;;;; conditions:
;;;;
;;;; The above copyright notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;; OTHER DEALINGS IN THE SOFTWARE.

(in-package :cl-environments)


;;; Conditions

(define-condition walk-program-error (program-error)
  ((message :initarg :message
	    :reader message
	    :initform nil))
  
  (:documentation
   "Condition raised when a syntax error is encountered in code being
    walked."))

(defun skip-walk (&optional c)
  "Invokes the SKIP-WALK restart. The SKIP-WALK restart skips walking
   over the form and simply returns it as is."

  (declare (ignore c))
  (invoke-restart 'skip-walk))


(defun walk-error (form msg)
  "Signals a WALK-PROGRAM-ERROR with a SKIP-WALK restart established,
   which returns FORM if invoked. MSG is the value of the MESSAGE slot
   of the WALK-PROGRAM-ERROR condition object which is to be
   signaled."
  
  (restart-case
      (error 'walk-program-error :message msg)
    (skip-walk () form)))


;;; Functions

(defun next-2 (list)
  "Equivalent to CDDR however if LIST has only one element left (that
   is the original list being traversed is not of even length) signals
   a WALK-PROGRAM-ERROR. Does not establish any restarts."
  
  (when list
    (aif (cdr list)
	 (cdr it)
	 (error 'walk-program-error :message "List of uneven length"))))



;;; Optima Patterns

(defpattern optional (arg)
  `(or ,arg nil))


;;; Macros

(defmacro! skip-walk-errors (&body body)
  "Surrounds BODY in a HANDLER-BIND which invokes the SKIP-WALK
   restart when the WALK-PROGRAM-ERROR condition is signaled."
  
  `(handler-bind ((walk-program-error #'skip-walk))
     ,@body))

(defmacro! match-form (pattern o!form &body body)
  "Performs list destructuring on FORM with PATTERN being the list
   structure. If FORM does not match the structure described by
   PATTERN a WALK-PROGRAM-ERROR is signaled, with a SKIP-WALK restart
   established. PATTERN may contain nested lists, dotted lists,
   &OPTIONAL (without init-forms or supplied-p variables) and &REST
   parameters. The difference between &REST and a dotted list is that
   the &REST var is checked to be a proper list whereas no type
   checking is performed for a dotted list."
  
  (labels ((list->cons (list)
	     (match list
	       ((list '&rest var)
		`(guard ,var (typep ,var 'proper-list)))

	       ((cons '&optional rest)
		`(or ,(list->cons rest)
		     nil))
	       
	       ((list 'quote arg)
		(list 'quote arg))
	       
	       ((cons item rest)
		`(cons ,(list->cons item)
		       ,(list->cons rest)))
	       
	       (_ list))))
    
    `(match ,g!form
       (,(list->cons pattern)
	 ,@body)
       (_ (walk-error ,g!form "Invalid form syntax")))))

(defmacro! check-list (o!thing &body body)
  "Checks that THING is a proper list and evaluates the forms in
   BODY. If THING is not a proper list, a WALK-ERROR is signaled, with
   a SKIP-WALK restart established (which simply returns THING)."
  
  `(if (proper-list-p ,g!thing)
       (progn ,@body)
       (walk-error ,g!thing "Invalid form syntax")))
