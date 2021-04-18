;;;; test.lisp
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

;;;; Master test suite for full code walker implementation

(defpackage :cl-environments.test.cltl2
  (:use :cl-environments-cl
	:cl-environments.util

	:alexandria
	:fiveam)

  (:import-from :cl-environments.util
		:symb)

  (:export :cltl2-test
	   :env-info
	   :info
	   :info=))

(in-package :cl-environments.test.cltl2)

(def-suite cltl2-test
    :description "Tests for the full CLTL2 implementation using code-walker")

(in-suite cltl2-test)

(defun test-cl-environments ()
  (run! 'cltl2-test))


;;; Utilities

(defmacro env-info (form &optional (env (gensym "ENV")))
  "Evaluate FORM in an environment where the variable named by ENV is
   bound to the lexical environment in which the macro appears. All
   return values of FORM are returned in a list."

  (with-gensyms (get-info)
    `(macrolet ((,get-info (&environment ,env)
		  `',(multiple-value-list ,form)))
       (,get-info))))

(defmacro info (type thing)
  "Retrieve information about a binding/declaration from the environment.

   TYPE is the type of binding to retrieve information for, either
   VARIABLE, FUNCTION or DECLARATION.

   THING is the name of the binding (or declaration in the case of
   TYPE being DECLARATION) of which to retrieve the information."

  (with-gensyms (env)
    `(env-info (,(symb type '-information) ',thing ,env) ,env)))

(defun decl= (got expected)
  "Check that the declaration information GOT has all the keys in the
   declaration information EXPECTED. It may have more keys."

  (loop
     for (key . value) in expected
     for assoc = (assoc key got)
     always (and assoc (decl-key= key (cdr assoc) value))))

(defun info= (got expected)
  "Check that the binding information GOT is equal to expected.

   GOT and EXPECTED are expected to be lists of three elements the
   first two being the binding type and whether it is local or global,
   both are compared directly with EQ. The third element is the
   declaration information compared with DECL="

  (destructuring-bind (&optional got-type got-local got-decls) got
    (destructuring-bind (exp-type exp-local exp-decls) expected
      (and (eq got-type exp-type)
	   (eq got-local exp-local)
	   (decl= got-decls exp-decls)))))


;;;; Comparing various declaration keys

(defgeneric decl-key= (key got exp)
  (:documentation
   "Compare the values for a given declaration information key."))

(defmethod decl-key= ((key t) got exp)
  "Compare values with EQUAL."

  (equal got exp))

(defmethod decl-key= ((key (eql 'type)) got exp)
  "Compare TYPE declaration information fields.

   Returns true if the type GOT is a subtype of the expected type
   EXP."

  (subtypep got exp))
