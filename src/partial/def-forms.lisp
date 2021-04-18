;;;; def-forms.lisp
;;;;
;;;; Copyright 2021 Alexander Gutev
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

(in-package :cl-environments.cltl2)

;;;; Code-walkers for forms which modify the global environment such
;;;; as global function, variable, macro definition forms.

;;; DEFUN

(defwalker cl:defun (args)
  "Walks DEFUN forms, adds the function to the global environment."

  (match-form (name . def) args
    (let ((env (copy-environment (get-environment *env*))))
      `(,name ,@(walk-fn-def def env t)))))


;;;; Generic Functions

;;; DEFGENERIC

(defwalker cl:defgeneric (args)
  "Walks DEFGENERIC forms, adds the function to the global
   environment."

  (match-form (name lambda-list &rest options) args
    `(,name
      ,lambda-list
      ,@(loop
	   for option in options
	   collect
	     (match option
	       ((list* :method def)
		`(:method ,@(walk-method def)))

	       (_ option))))))



;;; DEFMETHOD

(defwalker cl:defmethod (args)
  "Walks DEFMETHOD forms, adds the function to the global
   environment."

  (match-form (name . def) args
    `(,name ,@(walk-method def))))

(defun walk-method (def)
  "Walks the method definition DEF where DEF is the part of the
   definition following the method's name. Encloses the body in an
   environment augmented with the variable bindings in the method's
   generic function lambda-list."

  (flet ((consume-qualifiers (def)
	   (loop
	      for rest on def
	      for (thing) = rest
	      while (symbolp thing)
	      collect thing into qualifiers
	      finally
		(return (values qualifiers rest)))))

    (multiple-value-bind (qualifiers def)
	(consume-qualifiers def)

      `(,@qualifiers ,@(walk-fn-def def (get-environment *env*) t)))))

;;;; Variable Definitions

(defwalker cl:defparameter (args)
  "Walks DEFPARAMETER forms. Adds the variable binding (of
   type :SPECIAL) to the global environment and walks the init-form."

  (match-form (name init-form . doc) args
    (list* name (enclose-form init-form) doc)))

(defwalker cl:defvar (args)
  "Walks DEFVAR forms. Adds the variable binding (of type :SPECIAL) to
   the global environment and walks the init-form."

  (match-form (name . args) args
    (cons name
	  (destructuring-bind (&optional (init-form nil init-p) &rest doc) args
	    (when init-p
	      (cons (enclose-form init-form) doc))))))


;;;; Constant Definitions

(defwalker cl:defconstant (args)
  "Walks DEFCONSTANT forms. Adds the variable binding (of
   type :CONSTANT) to the global environment and walks the init-form."

  (match-form (name init-form . doc) args
    (list* name (enclose-form init-form) doc)))


;;;; Macros

;;; DEFMACRO

(defwalker cl:defmacro (args)
  "Walks DEFMACRO forms, adds the the macro to the global
   environment."

  (match-form (name . def) args
    (let ((env (copy-environment (get-environment *env*))))
      `(,name ,@(walk-macro-def def env)))))


;;; DEFINE-SYMBOL-MACRO

(defwalker cl:define-symbol-macro (args)
  "Walks DEFINE-SYMBOL-MACRO forms. Adds the symbol macro to the
   global environment."

  (match-form (name form) args
    (list name form)))
