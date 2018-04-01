;;;; cl-forms.lisp
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
  (let ((whole (gensym "WHOLE")))
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
  (block name &body body)
  (catch tag &body body)
  (eval-when situation &body body)
  (flet (&rest bindings) &body body)
  (function name-or-lambda-expression)
  (go tag)
  (if test true &optional false)
  (labels (&rest bindings) &body body)
  (let (&rest bindings) &body body)
  (let* (&rest bindings) &body body)
  (load-time-value form &optional read-only-p)
  (locally &body body)
  (macrolet (&rest bindings) &body body)
  (multiple-value-call function arg &rest args)
  (multiple-value-prog1 values-producing-form &body forms-for-effect)
  (progn &body body)
  (progv (&rest vars) (&rest values) &body body)
  (return-from block values)
  (setq &rest symbol-value-pairs)
  (symbol-macrolet (&rest bindings) &body body)
  (tagbody &rest tags-or-forms)
  (the type-specifier form)
  (throw tag value)
  (unwind-protect protected-form &body cleanup-forms)

  (defun function-name lambda-list &body body)
  (defgeneric function-name lambda-list &rest options-and-methods)
  (defmethod name &rest args)
  (defparameter var value &optional doc)
  (defvar var &optional value doc)
  (defconstant sym val &optional doc)
  (defmacro name arglist &body body)
  (define-symbol-macro name expansion)
  (declaim &rest declaration-specifiers))
