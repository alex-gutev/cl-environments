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

  (mapcar (rcurry #'get-return-type env) forms))

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

	    ((list* op _)
	     (multiple-value-bind (type local decl) (function-information op env)
	       (declare (ignore local))

	       (case type
		 (:function
		  (get-ftype decl))

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
	     `(eql ,value))

	    (_ t))))))
