;;;; cl-overrides.lisp
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

;;;; Contains implementations of standard CL functions which work on
;;;; extended environment objects.

(in-package :cl-environments)


;;; MACRO-FUNCTION

(defun macro-function (symbol &optional env)
  (typecase env
    (environment
     (or (macro-fn symbol env)
	 (cl:macro-function symbol (lexical-environment env))))
    
    (otherwise
     (cl:macro-function symbol env))))

(defun (setf macro-function) (fn symbol &optional env)
  (typecase env
    (environment
     (setf (macro-fn symbol env) fn))

    (otherwise
     (setf (cl:macro-function symbol env) fn))))


;;; MACROEXPAND

(defun macroexpand (form &optional env)
  (expand-macro form env))

(defun macroexpand-1 (form &optional env)
  (expand-macro-1 form env))


(defgeneric expand-macro (form env))

(defmethod expand-macro (form (env environment))
  (labels ((expand (form expanded)
	     (multiple-value-bind (expansion expandedp)
		 (expand-macro-1 form env)
	       (if expandedp
		   (expand expansion t)
		   (values form expanded)))))
    (expand form nil)))

(defmethod expand-macro (form env)
  (cl:macroexpand form env))

(defgeneric expand-macro-1 (form env))

(defmethod expand-macro-1 (form (env environment))
  (match form
    ((list* macro _)
     (aif (macro-fn macro env)
	  (values (funcall it form env) t)
	  (cl:macroexpand-1 form (lexical-environment env))))
    
    ((guard sym (symbolp sym))
     (aif (macro-form sym env)
	  (values it t)
	  (cl:macroexpand-1 form (lexical-environment env))))
    
    (_ (values form nil))))

(defmethod expand-macro-1 (form env)
  (cl:macroexpand-1 form env))


;;; GET-SETF-EXPANSION

(defun get-setf-expansion (place &optional env)
  (cl:get-setf-expansion
   place
   
   (typecase env
     (environment (lexical-environment env))
     (otherwise env))))


;;; COMPILER-MACRO-FUNCTION

(defun compiler-macro-function (name &optional env)
  (cl:compiler-macro-function
   name

   (typecase env
     (environment (lexical-environment env))
     (otherwise env))))

(defun (setf compiler-macro-function) (fn name &optional (env nil env-sp))
  (unless (typep env 'environment)
    (if env-sp ; This is to prevent errors on some CL implementations
	(setf (compiler-macro-function name env) fn)
	(setf (compiler-macro-function name) fn))))
