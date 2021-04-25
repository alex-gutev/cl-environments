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
  (:use :common-lisp
	:alexandria
	:hcl
	:cl-environments.util)

  (:shadow :define-declaration)

  (:export :variable-information
	   :function-information
	   :declaration-information
	   :define-declaration

	   :augment-environment
	   :enclose
	   :parse-macro
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

;; Declaration name is included in arguments

(defmacro define-declaration (decl-name (arg-var &optional (env-var (gensym "ENV"))) &body body)
  (with-gensyms (args)
    `(hcl:define-declaration ,decl-name (,args ,env-var)
       (declare (ignorable ,env-var))
       (let ((,arg-var (rest ,args)))
	 ,@body))))

(defun enclose-macro (name lambda-list body &optional env)
  (enclose (parse-macro name lambda-list body env) env))

(defun augmented-macroexpand-1 (form &optional env)
  (macroexpand-1 form env))

(defun augmented-macroexpand (form &optional env)
  (macroexpand form env))

(defun augmented-macro-function (name &optional env)
  (macro-function name env))

(defun augmented-get-setf-expansion (form &optional env)
  (get-setf-expansion form env))


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
