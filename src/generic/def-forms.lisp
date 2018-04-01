;;;; def-forms.lisp
;;;;
;;;; Copyright 2017 Alexander Gutev
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

;;;; Code-walkers for forms which modify the global environment such
;;;; as global function, variable and macro definition forms and
;;;; global declaration (DECLAIM) forms.

;;;; The code-walkers for function and macro definition forms serve
;;;; only to add the function/macro to the global environment. The
;;;; actual code-walking of the lambda lists and bodies is done on the
;;;; macro expansion of the definition forms.

;;; DEFUN

(defwalker cl:defun (args)
  "Walks DEFUN forms, adds the functon to the global environment."

  (walk-global-function args :function)
  args)


;;;; Generic Functions

;;; DEFGENERIC

(defwalker cl:defgeneric (args)
  "Walks DEFGENERIC forms, adds the function to the global
   environment."

  (walk-global-function args :function)
  args)



;;; DEFMETHOD

(defwalker cl:defmethod (args)
  "Walks DEFMETHOD forms. If the global environment does not contain a
   binding for the generic function, a binding is added to the global
   environment. The body is enclosed in an environment containing the
   method arguments."

  (walk-global-function args :function)
  args)


;;;; Variable Definitions

(defwalker cl:defparameter (args env)
  "Walks DEFPARAMETER forms. Encloses the init-form in the
   code-walking macro and adds the variable (with type :SPECIAL) to
   the global environment."
  
  (match-form (name init-form . doc) args
    (add-global-variable name)
    (list* name (walk-form init-form env) doc)))

(defwalker cl:defvar (args env)
  "Walks DEFVAR forms. Encloses the init-form in the code-walking macro
   and adds the variable (with type :SPECIAL) to the global
   environment."
  
  (match-form (name . args) args
    (add-global-variable name)
    (cons name
	  (destructuring-bind (&optional (init-form nil init-p) &rest doc) args
	    (when init-p
	      (cons (walk-form init-form env) doc))))))


;;;; Constant Definitions

(defwalker cl:defconstant (args env)
  "Walks DEFCONSTANT forms. Encloses the init-form in the code-walking
   macro and adds the constant (with type :CONSTANT) to the global
   environment."
  
  (match-form (name init-form . doc) args
    (add-global-variable name :constant)
    (list* name (walk-form init-form env) doc)))


;;;; Macros

;;; DEFMACRO

(defwalker cl:defmacro (args)
  "Walks DEFMACRO forms, adds the the macro to the global
   environment."

  (walk-global-function args :macro)
  args)


;;; DEFINE-SYMBOL-MACRO

(defwalker cl:define-symbol-macro (args env)
  "Walks DEFINE-SYMBOL-MACRO forms. Adds the symbol macro to the
   global environment"
  
  (match-form (name form) args
    (add-global-variable :symbol-macro)
    (list name (walk-form form env))))


;;; Global Declarations (DECLAIM)

(defwalker cl:declaim (args)
  (check-list args
    (loop
       for arg in args
       do
	 (match-form (decl &rest args) arg
	   (walk-declaration decl args *global-environment* t))))
  args)


;;; Utility Functions

(defun walk-global-function (args type)
  (when (consp args)
    (add-global-function (first args) type)))

(defun add-global-function (sym &optional (type :function))
  (ensure-function-type sym *global-environment* :binding-type type :local nil :global t))

(defun add-global-variable (sym &optional (type :special))
  (ensure-variable-type sym *global-environment* :binding-type type :local nil :global t))
