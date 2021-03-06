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
	   :in-lexical-environment
	   :info
	   :info=))

(in-package :cl-environments.test.cltl2)

(def-suite cltl2-test
    :description "Tests for the full CLTL2 implementation using code-walker")

(in-suite cltl2-test)

(defun test-cl-environments ()
  (run! 'cltl2-test))


;;; Utilities

(defmacro in-lexical-environment ((env-var) &body forms)
  "Evaluate forms in the current lexical environment.

   ENV-VAR is the name of the variable to which the lexical
   environment is bound. This binding is visible to FORMS.

   FORMS is the list of forms which are evaluated in an explicit
   PROGN. The forms are evaluated during macroexpansion, and this form
   is substituted by a quoted list containing the all the return
   values of the last form in FORMS."

  (with-gensyms (expand)
    `(macrolet ((,expand (&environment ,env-var)
		  `',(multiple-value-list (progn ,@forms))))
       (,expand))))

(defmacro info (type thing)
  "Retrieve information about a binding/declaration from the environment.

   TYPE is the type of binding to retrieve information for, either
   VARIABLE, FUNCTION or DECLARATION.

   THING is the name of the binding (or declaration in the case of
   TYPE being DECLARATION) of which to retrieve the information."

  (with-gensyms (env)
    `(in-lexical-environment (,env)
       (,(symb type '-information) ',thing ,env))))

(defun decl= (got expected)
  "Check that the declaration information GOT has all the keys in the
   declaration information EXPECTED. It may have more keys."

  (loop
     for (key . value) in expected
     for assoc = (assoc key got)
     always (and assoc (decl-key= key (cdr assoc) value))))

(defun info= (expected got)
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


;;; SUBTYPEP does not work on ABCL for function types. It's not
;;; necessary anyway since ABCL does not provided FUNCTION-INFORMATION
;;; there the result should be exactly as expected.

#-abcl
(defmethod decl-key= ((key (eql 'type)) got exp)
  "Compare TYPE declaration information fields.

   Returns true if the type GOT is a subtype of the expected type
   EXP."

  (subtypep got exp))

#-abcl
(defmethod decl-key= ((key (eql 'ftype)) got exp)
  "Compare FTYPE declaration information fields.

   Returns true if the type GOT is a subtype of the expected type
   EXP."

  (subtypep got exp))
