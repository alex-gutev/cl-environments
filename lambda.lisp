;;;; lambda.lisp
;;;;
;;;; Copyright 2017 Alexander Gutev
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

(defun lambda-list-keyword-p (sym)
  "Checks wether SYM names a lambda list keyword, i.e. checks whether
   SYM is a member of LAMBDA-LIST-KEYWORDS."
  
  (member sym lambda-list-keywords))

(defun walk-lambda-list (list env &key ((:destructure destructurep)) ((:env envp)) ((:generic genericp)))
  "Walks the lambda list LIST, augments the environment ENV with the
   bindings introduced by the lambda list, and encloses the initforms
   of the arguments in the environments augmented with the bindings
   introduced till that argument in the lambda list. Returns the new
   lambda list and the augmented environment. The keyword
   argument :DESTRUCTURE indicates whether the lambda list should be
   treated as a destructuring lambda list, :ENV whether &ENVIRONMENT
   parameters are accepted and :GENERIC whether the lambda list is a
   DEFMETHOD lambda list. The lambda lists are not checked for
   correctness as that is left to the CL implementation."
  
  (declare (special envp))
  
  (let* (new-list
	 (collector (make-simple-collector-to-place new-list))
	 (ext-env (copy-environment env)))
    (declare (special collector))
    
    (labels
	((collect (&rest things)
	   (apply collector things))
	 
	 (var-p (sym)
	   (and sym
		(symbolp sym)
		(not (keywordp sym))))
	 
	 (add-var (var &optional copy)
	   (when (var-p var)
	     (when copy
	       (setf ext-env (copy-environment ext-env)))
	     (add-variable var ext-env)))
	 
	 (enclose-initform (form)
	   (when form
	     (enclose-in-env ext-env (list (enclose-form form))))))

      (match-state list
	  (&whole
	   (cons '&whole rest)

	   (collect '&whole)
	   (next rest))

	  (&optional
	   (cons '&optional rest)

	   (collect '&optional)
	   (next rest))

	  (&rest
	   (cons (and (or '&rest '&body) keyword) rest)

	   (collect keyword)
	   (next rest))
	  
	  (&key
	   (cons '&key rest)

	   (collect '&key)
	   (next rest))

	  (&allow-other-keys
	   (cons '&allow-other-keys rest)
	   :from (&key keyword)

	   (collect '&allow-other-keys)
	   (next rest))

	  (&aux
	   (cons '&aux rest)

	   (collect '&aux)
	   (next rest))

	  (&environment
	   (list* '&environment var rest)

	   (when envp
	     (add-var var))

	   (setf envp nil)
	   (collect '&environment var)

	   (next rest (list 'end from-state)))

	  (unknown-keyword
	   (cons (guard keyword (lambda-list-keyword-p keyword)) rest)

	   (collect keyword)
	   (next rest))

	  
	  ;; Argument states

	  (whole
	   (cons var rest)
	   :from &whole

	   (collect var)
	   
	   (when destructurep
	     (add-var var))

	   (next rest))

	  (required
	   (guard
	    (cons
	     (guard var-list (listp var-list))
	     rest)
	    destructurep)
	   :from (:start (end :start) whole (end whole) required)

	   (let (new-list)
	     (let ((collector (make-simple-collector-to-place new-list))
		   envp)
	       (declare (special collector envp))
	       (next var-list :start))
	     (collect new-list))

	   (next rest))

	  (required
	   (cons
	    (or (guard (list var _) genericp)
		var)
	    rest)
	   :from (:start (end :start) whole (end whole) required)

	   (add-var var)
	   (collect var)
	   (next rest))

	  (optional
	   (cons
	    (or
	     (list var initform var-sp)
	     (list var initform)
	     (list var)
	     var)
	    rest)
	   :from (&optional optional)

	   (collect (list var (enclose-initform initform) var-sp))

	   (add-var var initform)
	   (add-var var-sp)
	   
	   (next rest))

	  (rest
	   (cons var rest)
	   :from &rest

	   (collect var)
	   (add-var var)

	   (next rest))
	  
	  (keyword
	   (cons
	    (or (cons
		 (and
		  (or
		   (list keyword var)
		   var)
		  arg)
		 (or
		  (list initform var-sp)
		  (list initform)
		  nil))
		(and var arg))
	    rest)
	   
	   :from (&key keyword)

	   (collect (list arg (enclose-initform initform) var-sp))
	   
	   (add-var var initform)
	   (add-var var-sp)
	   
	   (next rest))

	  (aux
	   (cons (or (list var initform) var) rest)
	   :from (&aux aux)

	   (collect (list var (enclose-initform initform)))
	   (add-var var initform)
	   (next rest))

	  (unknown
	   (cons arg rest)
	   :from unknown-keyword

	   (collect arg)
	   (next rest))

	  (end-of-list nil)

	  (dotted-list
	   (guard var (atom var))

	   (nconc new-list var)
	   
	   (when destructurep
	     (add-var var)))

	  (else
	   rest
	   (append new-list rest)))
	
      (values new-list ext-env))))
