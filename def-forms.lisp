;;;; def-forms.lisp
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

;;; Defun

(defmethod walk-fn-form ((op (eql 'cl:defun)) args env)
  "Walks DEFUN forms, adds the functon to the global environment,
   creates a new local environment containing the function arguments
   and encloses the body in those arguments."
  
  (match-form (name . def) args
    (let ((env (get-environment env)))
      (ensure-function-type name *global-environment* :type :function :local nil)
      `(cl:defun ,name ,@(walk-fn-def def env)))))


;;; Defmacro

(defmethod walk-fn-form ((op (eql 'cl:defmacro)) args env)
  "Walks DEFMACRO forms, adds the the macro to the global environment,
   creates a new local environment containing the macro arguments and
   encloses the body in those arguments."
  
  (match-form (name . def) args
    (let ((env (get-environment env)))
      (ensure-function-type name *global-environment* :type :macro :local nil)
      `(cl:defmacro ,name ,@(walk-macro-def def env)))))


;;;; Generic Functions

;;; Defgeneric

(defmethod walk-fn-form ((op (eql 'cl:defgeneric)) args env)
  "Walks DEFGENERIC forms, adds a function binding to the global
   environment. The generic function options are walked, the body
   of :METHOD options are enclosed in an environment containing the
   method arguments."
  
  (match-form (name lambda-list . options) args
    (let ((env (get-environment env)))
      (ensure-function-type name *global-environment* :type :function :local nil)
      `(cl:defgeneric ,name ,lambda-list
	 ,@(mapcar (rcurry #'walk-defgeneric-option env) options)))))

(defun walk-defgeneric-option (option env)
  "Walks a DEFGENERIC option. All options, except the :METHOD option,
   are simply returned. The bodies of :METHOD options are enclosed in
   a local environment containing the method arguments."
  
  (match option
    ((list* :method def)
     `(:method ,@(walk-method-def def env)))
    (_ option)))


;;; Defmethod

(defmethod walk-fn-form ((op (eql 'cl:defmethod)) args env)
  "Walks DEFMETHOD forms. If the global environment does not contain a
   binding for the generic function, a binding is added to the global
   environment. The body is enclosed in an environment containing the
   method arguments."
  
  (match-form (name . def) args
    (ensure-function-type name *global-environment*
			  :type :function
			  :local nil)
    (let ((env (get-environment env)))
      `(cl:defmethod ,name ,@(walk-method-def def env)))))

(defmethod walk-method-def (def env)
  "Walks a method definition, where DEF is the rest of the DEFMETHOD
   form (or :METHOD option), where the first element is either the
   method lambda list or method type."
  
  (match def
    ((or
      (list* (list* args) body)
      (list* type args body))
     
     (multiple-value-bind (lambda-list env)
	 (walk-lambda-list args env :generic t)
       `(,lambda-list
	 ,@(walk-body body env t))))
    
    (_ def)))
	   