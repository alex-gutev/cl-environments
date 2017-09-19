;;;; declarations.lisp
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

(defgeneric walk-declaration (decl args ext-env &optional global)
  (:documentation
   "Walks the declaration DECL with arguments ARGS and adds the
    information to EXT-ENV. If GLOBAL is true, the declarations are
    treated as global declarations and only declarations which can
    appears as GLOBAL are processed."))


;;; Type Declarations

(defmethod walk-declaration ((decl (eql 'type)) args ext-env &optional global)
  "Adds type information to the information list of the variables in ARGS."
  
  (declare (ignore global))
  (destructuring-bind (type . vars) args
    (add-variables-info vars 'type type ext-env))
  (cons decl args))

(defmethod walk-declaration ((decl (eql 'ftype)) args ext-env &optional global)
  "Adds type information to the information list of the functions in ARGS."
  
  (declare (ignore global))
  (destructuring-bind (type . fns) args
    (add-functions-info fns 'ftype type ext-env))
  (cons decl args))


;;; Special declarations

(defmethod walk-declaration ((decl (eql 'special)) args ext-env &optional global)
  "Changes the binding types of the variables in ARGS to :SPECIAL. If
   a variable does not exist in the environment a new variable with
   binding type :SPECIAL is added."
  
  (declare (ignore global))
  (mapc (rcurry #'ensure-special-variable ext-env) args)
  (cons decl args))


;;; Dynamic extent declarations

(defmethod walk-declaration ((decl (eql 'dynamic-extent)) args ext-env &optional global)
  "Adds (DYNAMIC-EXTENT . T) to the information list of the variables
   and functions in ARGS."
  
  (unless global
    (dolist (arg args)
      (match arg
	((list 'function fn)
	 (add-function-info fn 'dynamic-extent t ext-env))
	(var
	 (add-function-info var 'dyanmic-extent t ext-env)))))
  (cons decl args))


;;; Ignore declarations

(defmethod walk-declaration ((decl (eql 'ignore)) args ext-env &optional global)
  "Adds (IGNORE . T) to the information list of the variables in ARGS."
  
  (unless global
    (add-variables-info args 'ignore t ext-env))
  (cons decl args))

(defmethod walk-declaration ((decl (eql 'ignorable)) args ext-env &optional global)
  "Currently does nothing (other than simply returning the
   declaration) as IGNORABLE declarations are not mentioned in CLTL2."
  
  (declare (ignore ext-env global))
  (cons decl args))


;;; Inlining declarations

(defmethod walk-declaration ((decl (eql 'inline)) args ext-env &optional global)
  "Adds (INLINE . INLINE) to the information list of the functions in ARGS."
  
  (declare (ignore global))
  (add-functions-info args 'inline 'inline ext-env)
  (cons decl args))

;; Local and global
(defmethod walk-declaration ((decl (eql 'notinline)) args ext-env &optional global)
  "Adds (INLINE . NOTINLINE) to the information list of the functions in ARGS."
  
  (declare (ignore global))
  (add-functions-info args 'inline 'notinline ext-env)
  (cons decl args))


;;; Optimization declarations

(defmethod walk-declaration ((decl (eql 'optimize)) args ext-env &optional global)
  "Normalizes the optimization qualities (in ARGS) to a list of 6
   elements, one for each quality, of the form (QUALITY . PPRIORITY)
   and adds the optimization information to the general declaration
   information in the environment. If ARGS does not contain an element
   for each quality, the priority for the quality in EXT-ENV is used
   instead."
  
  (declare (ignore global))
  (labels ((find-assoc (item list)
	     (find item list :key #'ensure-car))
	   (priority (quality)
	     (or (find-assoc quality args)
		 (get-declaration-info 'optimize ext-env)))
	   (ensure-quality (quality)
	     (if (symbolp quality)
		 (list quality 3)
		 quality))
	   (get-priority (quality)
	     (ensure-quality (priority quality))))
    (setf (declaration-info 'optimize ext-env)
	  (mapcar #'get-priority +optimize-qualities+)))
  (cons decl args))


;;; Non-standard and user-defined declarations

(defmethod walk-declaration ((decl (eql 'declaration)) args ext-env &optional global)
  "Adds the declaration to the list of valid declarations."

  (when global
    (unionf (declaration-info 'declaration ext-env) args :test #'eq))
  (cons decl args))


(defmethod walk-declaration (decl args ext-env &optional global)
  "If there is a declaration function (defined using
   DEFINE-DECLARATION) in EXT-ENV, it is called, the information
   returned by the function is added to the environment and NIL is
   returned. If there is no function for DECL then DECL is assumed to
   be an implementation specific declaration and is simply returned."
  
  (declare (ignore global))
  
  (acond
    ((declaration-function decl ext-env)
     (multiple-value-call #'add-decl-info (funcall it args ext-env) ext-env))
    (t
     (cons decl args))))

(defun add-decl-info (type info ext-env)
  "Adds the information returned by a declaration function to the
   environment. TYPE is the first return
   value (either :VARIABLE :FUNCTION or :DECLARE), INFO is the second
   return value."
  
  (flet ((add-binding-info (add-info)
	   (loop
	      for (sym key value) in info
	      do (funcall add-info sym key value ext-env))))
    (case type
      (:variable
       (add-binding-info #'add-variable-info))
      (:function
       (add-binding-info #'add-function-info))
      (:declare
       (setf (declaration-info (car info) ext-env) (cdr info))))))
     
