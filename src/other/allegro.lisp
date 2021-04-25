;;;; allegro.lisp
;;;;
;;;; Copyright 2018-2021 Alexander Gutev
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

(defpackage :cl-environments.cltl2
  (:use :common-lisp)

  (:import-from :cl-environments.util
                :reexport-all-symbols)

  (:import-from :sys :augment-environment)

  (:shadow :variable-information
	   :function-information
	   :define-declaration)

  (:export :variable-information
	   :function-information
	   :declaration-information
	   :define-declaration

	   :augment-environment
	   :enclose
	   :enclose-macro

	   :augmented-macroexpand-1
	   :augmented-macroexpand
	   :augmented-macro-function
	   :augmented-get-setf-expansion

	   :enable-hook
	   :disable-hook

	   :walk-environment))

(defpackage :cl-environments-cl
  (:nicknames :cl-environments)
  (:use :common-lisp
	:cl-environments.util
	:cl-environments.cltl2)

  (:export :variable-information
	   :function-information
	   :declaration-information
	   :define-declaration

	   :augment-environment
	   :enclose
	   :enclose-macro

	   :augmented-macroexpand-1
	   :augmented-macroexpand
	   :augmented-macro-function
	   :augmented-get-setf-expansion

	   :enable-hook
	   :disable-hook

	   :walk-environment))

(in-package :cl-environments.cltl2)

(defun variable-information (variable &optional env)
  (multiple-value-bind (type binding declarations local)
      (sys:variable-information variable env t)
    (declare (ignore binding))
    (values type local declarations)))


(defun function-information (function &optional env)
  (let ((env (sys:augment-environment env)))
    (setf (sys::augmentable-environment-kind env) :evaluation)

    (multiple-value-bind (type binding declarations local)
	(sys:function-information function env t nil)
      (declare (ignore binding))
      (values
       (case type
	 (:special-operator :special-form)
	 (t type))
       local
       declarations))))


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

(defun augmented-macroexpand-1 (form &optional env)
  (macroexpand-1 form env))

(defun augmented-macroexpand (form &optional env)
  (macroexpand form env))

(defun augmented-macro-function (name &optional env)
  (macro-function name env))

(defun augmented-get-setf-expansion (form &optional env)
  (get-setf-expansion form env))

(defun parse-macro (name lambda-list body &optional env)
  (declare (ignorable name lambda-list body env))
  (excl::defmacro-expander `(,name ,lambda-list ,@body) env))

(defun enclose (lambda-expression &optional env)
  (excl:compile-lambda-expr-in-env lambda-expression env))

(defun enclose-macro (name lambda-list body &optional env)
  (enclose (parse-macro name lambda-list body env) env))

(defun enable-hook (&optional (previous-hook *macroexpand-hook*))
  "Does nothing, provided for compatibility with implementations where
   the code walker is required."

  (declare (ignore previous-hook)))

(defun disable-hook (&optional (previous-hook *previous-hook*))
  "Does nothing, provided for compatibility with implementations where
   the code walker is required."

  (declare (ignore previous-hook)))

(defmacro walk-environment (&body forms)
  `(progn ,@forms))


;;; Reexport symbols in CL package

(in-package :cl-environments-cl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (reexport-all-symbols :cl))
