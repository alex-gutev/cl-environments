;;;; allegro.lisp
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

(defpackage :cl-environments
  (:use :common-lisp
	:sys)

  (:shadow :variable-information
	   :function-information
	   :define-declaration)

  (:export :variable-information
	   :function-information
	   :declaration-information
	   :define-declaration))

(in-package :cl-environments)

(defun variable-information (variable &optional env)
  (multiple-value-bind (type binding declarations local)
      (sys:variable-information variable env t)
    (declare (ignore binding))
    (values type local declarations)))


(defun function-information (function &optional env)
  (let ((env (augment-environment env)))
    (setf (sys::augmentable-environment-kind env) :evaluation)

    (multiple-value-bind (type binding declarations local)
	(sys:function-information function env t nil)
      (declare (ignore binding))
      (values type local declarations))))


(defun convert-declaration (type info)
  (flet ((cons->list (cons)
	   (list (car cons) (cdr cons))))
    (values type
     (case type
       (:declare
	(mapcar #'cons->list info))

       (otherwise
	info)))))

(defmacro define-declaration (decl-name (arg-var &optional (env-var (gensym "ENV"))) &body body)
  `(sys:define-declaration ,decl-name (&rest ,(gensym "args"))
     nil :both
     (lambda (,arg-var ,env-var)
       (declare (ignorable env-var))
       (multiple-value-call #'convert-declaration (progn ,@body)))))


;;; Reexport symbols in CL package

(eval-when (:compile-toplevel :load-toplevel :execute)
  (reexport-all-symbols :cl))
