;;;; types.lisp
;;;;
;;;; Copyright 2019 Alexander Gutev
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

;;;; Functions for obtaining the types of forms based on the type
;;;; information stored in the environment.

(in-package :cl-environments.tools)


(defun get-return-types (forms env)
  "Determines the return value type of each form in FORMS, in the
   environment ENV. Returns a list where each element is the
   return-value type of the corresponding form in FORMS."

  (mapcar (rcurry #'get-value-type env) forms))

(defun get-value-type (form env &optional (n 0))
  "Determines the type of the N'th value returned by FORM, in the
   environment ENV. If N > 0 and there is no information about the
   type of the N'th value, NIL is returned."

  (match (get-return-type form env)
    ((list* 'values types)
     (nth n types))

    (type
     (if (zerop n)
	 type
	 nil))))

(defun get-return-type (form env)
  "Determines the type of the return value of the form FORM, in the
   environment ENV."

  (flet ((get-ftype (decl)
	   (match (cdr (assoc 'ftype decl))
	     ((list 'function _ (and (not (eq '*)) return-type))
	      return-type)

	     (_ t)))

	 (get-vtype (decl)
	   (aif (assoc 'type decl)
		(cdr it)
		t)))

    (multiple-value-bind (form expanded?) (macroexpand-1 form env)
      (if expanded?
	  (get-return-type form env)

	  (match form
	    ((list 'cl:the type _)
	     type)

	    ((list* op args)
	     (multiple-value-bind (type local decl) (function-information op env)
	       (declare (ignore local))

	       (case type
		 (:function
		  (get-ftype decl))

		 (:special-form
		  (get-special-form-return-type op args env))

		 (otherwise t))))

	    ((satisfies symbolp)
	     (multiple-value-bind (type local decl) (variable-information form env)
	       (declare (ignore local))

	       (case type
		 (:constant
		  `(eql ,(eval form)))

		 ((:lexical :special)
		  (get-vtype decl))

		 (otherwise t))))

	    ((guard value (constantp value env))
	     (constant-type value))

	    (_ t))))))

(defun constant-type (value)
  "Return the type specifier of a constant value.

   If the value is a CHARACTER, NUMBER or SYMBOL an EQL type specifier
   is returned. Otherwise the type specifier returned by TYPE-OF is
   returned.

   VALUE is the constant value."

  (typecase value
    ((or number character symbol) `(eql ,value))
    (otherwise
     (type-of value))))

(defun get-special-form-return-type (operator operands env)
  "Determine the type of value returned by a form in which the
   operator is a special operator."

  (case operator
    (cl:progn
     (get-return-type (lastcar operands) env))

    (cl:quote
     (when (length= operands 1)
       `(eql ,(first operands))))

    (cl:setq
     (when (evenp (length operands))
       (get-return-type (lastcar operands) env)))))
