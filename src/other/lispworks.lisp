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
	:alexandria
	:hcl
	:cl-environments.util)

  (:shadow :define-declaration)

  (:export :variable-information
	   :function-information
	   :declaration-information
	   :define-declaration

	   :enable-hook
	   :disable-hook))

(in-package :cl-environments)

;; Declaration name is included in arguments

(defmacro define-declaration (decl-name (arg-var &optional (env-var (gensym "ENV"))) &body body)
  (with-gensyms (args)
    `(hcl:define-declaration ,decl-name (,args ,env-var)
       (declare (ignorable ,env-var))
       (let ((,arg-var (rest ,args)))
	 ,@body))))


;;; Reexport symbols in CL package

(eval-when (:compile-toplevel :load-toplevel :execute)
  (reexport-all-symbols :cl))


(defun enable-hook (&optional (previous-hook *macroexpand-hook*))
  "Does nothing, provided for compatibility with implementations where
   the code walker is required."

  (declare (ignore previous-hook)))

(defun disable-hook (&optional (previous-hook *previous-hook*))
  "Does nothing, provided for compatibility with implementations where
   the code walker is required."

  (declare (ignore previous-hook)))
