;;;; cl-overrides.lisp
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

(defpackage :cl-environments-cl
  (:nicknames :cl-environments)

  (:use :common-lisp
	:alexandria

	:cl-environments.util
	:cl-environments.cltl2)

  (:export :variable-information
	   :function-information
	   :declaration-information
	   :define-declaration

	   :augment-environment
	   :augmented-macroexpand-1
	   :augmented-macroexpand
	   :augmented-macro-function
	   :augmented-get-setf-expansion

	   :enable-hook
	   :disable-hook

	   :walk-environment)

  (:import-from :cl-environments.cltl2
		:enclose-form
		:augmented-environment
		:base-environment)

  (:shadow :flet
	   :labels
	   :let
	   :let*
	   :locally
	   :macrolet
	   :symbol-macrolet

	   :defun
	   :defmacro
	   :defgeneric
	   :defmethod

	   :defparameter
	   :defvar
	   :defconstant
	   :define-symbol-macro

	   :declaim

	   ;; Shadowed functions which take ENVIRONMENT parameters.
	   :macroexpand
	   :macroexpand-1
	   :macro-function
	   :compiler-macro-function
	   :constantp
	   :get-setf-expansion
	   :typep
	   :subtypep)

  (:documentation
   "Package exporting the CLTL2 environments API and shadowing the
    special forms so that it works correctly across
    implementations."))

(in-package :cl-environments-cl)

;;; Shadow CL special forms with macros, which simply expand into the
;;; special forms, in for them to be walked when *MACROEXPAND-HOOK* is
;;; called.

(cl:defmacro add-cl-form-macros (&rest ops)
  "Defines shadowing macros for special forms in the CL package. Each
   element of OPS is a list where the first element is the symbol, for
   which the macro will be defined, and the remaining elements are the
   macro's lambda-list (only used for documentation in SLIME). The
   macro defined expands into the same form with the macro operator
   symbol replaced by the special operator symbol, which has the same
   SYMBOL-NAME as the macro but in the CL package."

  (cl:let ((whole (gensym "WHOLE")))
    (cl:labels ((lambda-list-args (list)
		  (set-difference (flatten list) lambda-list-keywords))

		(shadowing-macro (sym args)
		  (cl:let* ((name (symbol-name sym))
			    (op (intern name :cl)))

		    `(cl:defmacro ,sym (&whole ,whole ,@args)
		       (declare (ignore ,@(lambda-list-args args)))
		       (enclose-form (cons ',op (rest ,whole)))))))

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
  (symbol-macrolet (&rest bindings) &body body)

  (defun name (&rest lambda-list) &body body)
  (defgeneric name (&rest lambda-list) &body options)
  (defmethod name &rest def)

  (defmacro name (&rest lambda-list) &body body)
  (defparameter name value &rest args)
  (defvar name &rest args)
  (defconstant name value &rest args)
  (define-symbol-macro symbol expansion)

  (declaim &rest declaration-specifiers))


;;; Shadow functions which take ENVIRONMENT parameter

(defun macroexpand (form &optional environment)
  (augmented-macroexpand form environment))

(defun macroexpand-1 (form &optional environment)
  (augmented-macroexpand-1 form environment))

(defun macro-function (symbol &optional environment)
  (augmented-macro-function symbol environment))

(defun (setf macro-function) (new-fn symbol &optional (environment nil environment-p))
  ;; Technically Calling (SETF MACRO-FUNCTION) with a non-NIL
  ;; environment parameter is undefined, but we want to preserve the
  ;; actual behaviour on the implementation.

  (if environment-p
      (setf (cl:macro-function symbol environment) (get-base-environment new-fn))
      (setf (cl:macro-function symbol) new-fn)))

(defun compiler-macro-function (symbol &optional environment)
  (cl:compiler-macro-function symbol (get-base-environment environment)))

(defun (setf compiler-macro-function) (new-fn symbol &optional (environment nil environment-p))
  ;; Technically Calling (SETF COMPILER-MACRO-FUNCTION) with a non-NIL
  ;; environment parameter is undefined, but we want to preserve the
  ;; actual behaviour on the implementation.

  (if environment-p
      (setf (cl:compiler-macro-function symbol (get-base-environment environment)) new-fn)
      (setf (cl:compiler-macro-function symbol) new-fn)))

(defun constantp (form &optional environmnet)
  (cl:constantp form (get-base-environment environmnet)))

(defun get-setf-expansion (place &optional environment)
  (augmented-get-setf-expansion place environment))

(defun typep (object type-specifier &optional environment)
  (cl:typep object type-specifier (get-base-environment environment)))

(defun subtypep (type-1 type-2 &optional environment)
  (cl:subtypep type-1 type-2 (get-base-environment environment)))

(defun get-base-environment (environment)
  (typecase environment
    (augmented-environment
     (base-environment environment))

    (otherwise
     environment)))

;;; Re-export all symbols imported from the CL package except symbols
;;; which have been shadowed

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((shadowed (mapcar #'symbol-name (package-shadowing-symbols :cl-environments))))
    (do-external-symbols (sym :cl)
      (export (if (member (symbol-name sym) shadowed :test #'string=)
		  (find-symbol (symbol-name sym) :cl-environments)
		  (list sym))
	      :cl-environments))))
