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

(in-package :cl-environments.walker)

(defun walk-declarations (decl ext-env)
  "Walks the declare expressions in DECL and adds the information to
   EXT-ENV."
  
  (labels ((walk-decl (decl)
	     (match decl
	       ((list* decl args)
		(walk-declaration decl args ext-env))))
	   
	   (walk-declare (decl)
	     (mapc-ite #'walk-decl decl)))

    (mapc-ite #'walk-declare decl)))


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
  
  (match args
    ((list* type vars)
     (ignore-type-errors ()
       (add-variables-info vars 'type type ext-env)))))

(defmethod walk-declaration ((decl (eql 'ftype)) args ext-env &optional global)
  "Adds type information to the information list of the functions in ARGS."
  
  (declare (ignore global))

  (match args
    ((list* type fns)
     (ignore-type-errors ()
       (add-functions-info fns 'ftype type ext-env)))))


;;; Special declarations

(defmethod walk-declaration ((decl (eql 'special)) args ext-env &optional global)
  "Changes the binding types of the variables in ARGS to :SPECIAL. If
   a variable does not exist in the environment a new variable with
   binding type :SPECIAL is added."

  (mapc-ite
   (if global
       (rcurry #'ensure-variable-type ext-env :type :special :global t)
       (rcurry #'ensure-special-variable ext-env))
   args))


;;; Dynamic extent declarations

(defmethod walk-declaration ((decl (eql 'dynamic-extent)) args ext-env &optional global)
  "Adds (DYNAMIC-EXTENT . T) to the information list of the variables
   and functions in ARGS."
  
  (unless global
    (dolist-ite (arg args)
      (match arg
	((list 'function fn)
	 (add-function-info fn 'dynamic-extent t ext-env))
	((guard var #'symbolp)
	 (add-variable-info var 'dynamic-extent t ext-env))))))


;;; Ignore declarations

(defmethod walk-declaration ((decl (eql 'ignore)) args ext-env &optional global)
  "Adds (IGNORE . T) to the information list of the variables in ARGS."
  
  (unless global
    (ignore-type-errors ()
      (add-variables-info args 'ignore t ext-env))))

(defmethod walk-declaration ((decl (eql 'ignorable)) args ext-env &optional global)
  "Currently does nothing (other than simply returning the
   declaration) as IGNORABLE declarations are not mentioned in CLTL2."
  
  (declare (ignore args ext-env global)))


;;; Inlining declarations

(defmethod walk-declaration ((decl (eql 'inline)) args ext-env &optional global)
  "Adds (INLINE . INLINE) to the information list of the functions in ARGS."
  
  (declare (ignore global))
  (ignore-type-errors ()
    (add-functions-info args 'inline 'inline ext-env)))

;; Local and global
(defmethod walk-declaration ((decl (eql 'notinline)) args ext-env &optional global)
  "Adds (INLINE . NOTINLINE) to the information list of the functions in ARGS."
  
  (declare (ignore global))
  (ignore-type-errors ()
    (add-functions-info args 'inline 'notinline ext-env)))


;;; Optimization declarations

(defmethod walk-declaration ((decl (eql 'optimize)) args ext-env &optional global)
  "Normalizes the optimization qualities (in ARGS) to a list of 6
   elements, one for each quality, of the form (QUALITY . PPRIORITY)
   and adds the optimization information to the general declaration
   information in the environment. If ARGS does not contain an element
   for each quality, the priority for the quality in EXT-ENV is used
   instead."
  
  (declare (ignore global))

  (ignore-type-errors ()
    (let ((info (declaration-info 'optimize ext-env)))
      (labels ((find-assoc (item list)
		 (find item list :key #'ensure-car))
	   
	       (priority (quality)
		 (or (find-assoc quality args)
		     (find-assoc quality info)))
	     
	       (ensure-quality (quality)
		 (if (symbolp quality)
		     (list quality 3)
		     quality))
	   
	       (get-priority (quality)
		 (ensure-quality (priority quality))))
    
	(setf (declaration-info 'optimize ext-env)
	      (mapcar #'get-priority +optimize-qualities+))))))


;;; Non-standard and user-defined declarations

(defmethod walk-declaration ((decl (eql 'declaration)) args ext-env &optional global)
  "Adds the declaration to the list of valid declarations."

  (when global
    (ignore-type-errors ()
      (unionf (declaration-info 'declaration ext-env) args :test #'eq))))


(defmethod walk-declaration (decl args ext-env &optional global)
  "If there is a declaration function (defined using
   DEFINE-DECLARATION) in EXT-ENV, it is called and the information
   returned by the function is added to the environment."
  
  (declare (ignore global))

  (awhen (declaration-function decl ext-env)
    (multiple-value-call #'add-decl-info (funcall it args ext-env) ext-env)))

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
     
