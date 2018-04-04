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

(in-package :cl-environments)

;;; Shadow CL special forms with macros, which simply expand into the
;;; special forms, in for them to be walked when *MACROEXPAND-HOOK* is
;;; called.

(cl:defmacro add-cl-form-macros (&rest ops)
  (cl:let ((whole (gensym "WHOLE")))
    (cl:labels ((lambda-list-args (list)
		  (set-difference (flatten list) lambda-list-keywords))

		(shadowing-macro (sym args)
		  (cl:let* ((name (symbol-name sym))
			    (op (intern name :cl)))
		    
		    `(cl:defmacro ,sym (&whole ,whole ,@args)
		       (declare (ignore ,@(lambda-list-args args)))
		       (cons ',op (rest ,whole))))))
      
      `(cl:progn
	 ,@(loop
	      for (sym . args) in ops
	      collect (shadowing-macro sym args))))))


(add-cl-form-macros
  (flet (&rest bindings) &body body)
  (labels (&rest bindings) &body body)
  (let (&rest bindings) &body body)
  (let* (&rest bindings) &body body)
  (locally &body body)
  (macrolet (&rest bindings) &body body)
  (symbol-macrolet (&rest bindings) &body body))



(defgeneric lexical-environment (env)
  (:documentation
   "Returns the implementation-specific lexical environment.")

  (:method (env)
    "Non-specialized method, simply returns ENV. This allows this
     accessor to be used with both `environment' objects and
     implementation-specific lexical environments."
  
    env))



;;; The functions implemented below simply call the CL functions,
;;; however if the environment passed is an `environment' object, the
;;; implementation specific environment stored in the
;;; LEXICAL-ENVIRONMENT slot is passed as the environment parameter
;;; instead.


(defun macro-function (symbol &optional env)
  (cl:macro-function symbol (lexical-environment env)))

(defun (setf macro-function) (fn symbol &optional env)
  (setf (cl:macro-function symbol (lexical-environment env)) fn))


(defun macroexpand-1 (form &optional env)
  (cl:macroexpand-1 form (lexical-environment env)))

(defun macroexpand (form &optional env)
  (cl:macroexpand form (lexical-environment env)))


(defun get-setf-expansion (place &optional env)
  (cl:get-setf-expansion place (lexical-environment env)))


(defun compiler-macro-function (name &optional env)
  (cl:compiler-macro-function name (lexical-environment env)))

(defun (setf compiler-macro-function) (fn name &optional env)
  (declare (ignore env))
  (setf (cl:compiler-macro-function name) fn))
