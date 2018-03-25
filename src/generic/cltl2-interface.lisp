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

(in-package :cl-environments)

(defun variable-information (variable &optional env)
  "Returns information about the variable binding for the symbol
   VARIABLE, in the environment ENV. Returns three values: the first
   value is the binding type nil, :SPECIAL, :LEXICAL, :SYMBOL-MACRO
   or :CONSTANT, the second value is true if there is a local binding
   and the third value is the association list of the declaration
   information."

  (typecase env
    (environment
     (let ((ext-env (get-environment env)))
       (slot-values (type local declarations)
	   (variable-binding variable ext-env)
	 (values type local declarations))))

    (otherwise (variable-information variable (get-environment env)))))

(defun function-information (function &optional env)
  "Returns information about the function binding for the symbol
   FUNCTION in the environment ENV. Returns three values: the first
   value is the binding type nil, :FUNCTION, :MACRO or :SPECIAL-FORM,
   the second value is true if there is a local binding and the third
   value is the association list of the declaration information."

  (typecase env
    (environment
  
     (let ((ext-env (get-environment env)))
       (slot-values (type local declarations)
	   (function-binding function ext-env)
	 (values type local declarations))))

    (otherwise (function-information function (get-environment env)))))


(defun declaration-information (decl-name &optional env)
  "Returns information about the declaration DECL-NAME in the
   environment ENV."

  (typecase env
    (environment 
     (declaration-info decl-name (get-environment env)))

    (otherwise
     (declaration-information decl-name (get-environment env)))))


(defmacro define-declaration (decl-name lambda-list &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (declaim (declaration ,decl-name))
     (setf (declaration-function ,decl-name *global-environment*)
	   (lambda ,lambda-list ,@body))
     ,decl-name))
