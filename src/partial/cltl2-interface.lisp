;;;; cltl2-interface.lisp
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

;;;; Implements the VARIABLE-INFORMATION, FUNCTION-INFORMATION and
;;;; DECLARATION-INFORMATION FUNCTIONS as specified in Common Lisp the
;;;; Language 2 (CLTL2)

(in-package :cl-environments.cltl2)

(defmacro cltl2-fn (fn &rest args)
  `(,(intern (symbol-name fn)
	     #+ccl :ccl
	     #+cmucl :extensions) ,@args))

(defun variable-information (variable &optional env)
  "Returns information about the variable binding for the symbol
   VARIABLE, in the environment ENV. Returns three values: the first
   value is the binding type nil, :SPECIAL, :LEXICAL, :SYMBOL-MACRO
   or :CONSTANT, the second value is true if there is a local binding
   and the third value is an association list containing declaration
   information."

  (let ((info (variable-binding variable (get-environment env))))
    (multiple-value-bind (type local decl)
	(cltl2-fn variable-information variable env)
      (values type local (append decl info)))))

(defun function-information (function &optional env)
  "Returns information about the function binding for the symbol
   FUNCTION in the environment ENV. Returns three values: the first
   value is the binding type nil, :FUNCTION, :MACRO or :SPECIAL-FORM,
   the second value is true if there is a local binding and the third
   value is an association list containing declaration information."

  (let ((info (function-binding function (get-environment env))))
    (multiple-value-bind (type local decl)
	(cltl2-fn function-information function env)
      (values type local (append decl info)))))

(defun declaration-information (decl-name &optional env)
  "Returns information about the declaration DECL-NAME in the
   environment ENV."

  (let ((ext-env (get-environment env)))
    (if (declaration-function decl-name ext-env)
	(declaration-info decl-name (get-environment env))
	(cltl2-fn declaration-information decl-name env))))


(defmacro define-declaration (decl-name (arg-var &optional (env-var (gensym "ENV"))) &body body)
  "Defines a handler function for the user-defined declaration
   DECL-NAME. ARG-VAR is a symbol bound to the argument list of the
   declaration expression, ENV-VAR is a symbol bound to the lexical
   environment in which the declaration appears. The function should
   return two values: the first value is a keyword identifying whether
   the declaration applies to variable bindings (:VARIABLE), function
   bindings (:FUNCTION) or is a free declaration :DECLARE. If the
   first value is :VARIABLE or :FUNCTION the second must be a list
   where each element is of the form (BINDING-NAME KEY VALUE) where
   BINDING-NAME is the function or variable binding to which the
   declaration applies, and (KEY . VALUE) is the key value pair added
   to the declaration list of the binding. If the first value
   is :DECLARE the second value must be a CONS of the (KEY . VALUE),
   which is added to the declarations list of the lexical
   environment."

  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (declaim (declaration ,decl-name))
     (setf (declaration-function ',decl-name *global-environment*)
	   (lambda (,arg-var ,env-var)
	     (declare (ignorable ,env-var))
	     ,@body))

     #+ccl (ccl:define-declaration ,decl-name (,arg-var)
       (let (,env-var)
	 (declare (ignorable ,env-var))
	 ,@body))

     ',decl-name))
