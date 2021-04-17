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

	   :enable-hook
	   :disable-hook

	   :walk-environment)

  (:import-from :cl-environments.cltl2
		:enclose-form)

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

	   :declaim)

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



;;; Re-export all symbols imported from the CL package except symbols
;;; which have been shadowed

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((shadowed (mapcar #'symbol-name (package-shadowing-symbols :cl-environments))))
    (do-external-symbols (sym :cl)
      (export (if (member (symbol-name sym) shadowed :test #'string=)
		  (find-symbol (symbol-name sym) :cl-environments)
		  (list sym))
	      :cl-environments))))
