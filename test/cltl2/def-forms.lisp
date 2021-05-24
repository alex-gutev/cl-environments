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

;;;; Test extracting environment information in definition forms

(defpackage :cl-environments.test.cltl2.definition-forms
  (:use :cl-environments-cl
	:cl-environments.test.cltl2
	:alexandria
	:fiveam))

(in-package :cl-environments.test.cltl2.definition-forms)

(def-suite definition-forms
    :description "Test extraction of environment information in definition forms"
    :in cltl2-test)

(in-suite definition-forms)

(defun test-function (a)
  "Test function to check that DEFUN bodies are walked."

  (declare (ignore a))
  (info variable a))

(defmacro test-macro (form)
  "Test Macro to check that DEFMACRO bodies are walked."

  (declare (ignore form))
  `',(info variable form))

(defgeneric test-generic (a)
  (:method ((a integer))
    (declare (type integer a))
    (1+ a)
    (info variable a)))

(defmethod test-generic ((b string))
  (declare (type string b))
  (concatenate 'string "string" b)
  (info variable b))

(defparameter *test-param* (cl:let ((var))
			   (declare (type null var))
			   (list var)

			   (info variable var))
  "Tests walking DEFPARAMETER")

(defvar *test-var* (cl:let ((var))
		   (declare (type null var))
		   (list var)

		   (info variable var))

  "Tests walking DEFVAR")

(defconstant +a-constant+ 15)

(define-symbol-macro symbol-mac "symbol-macro")

(declaim (declaration my-declaration))

(test defun
  "Test extracting environment information in DEFUN"

  (is (info= '(:lexical t ((ignore . t))) (test-function 1))))

(test defmacro
  "Test extracting environment information in DEFMACRO"

  (is (info= #-ecl '(:lexical t ((ignore . t)))
	     #+ecl '(:lexical t nil)
	     (test-macro 'x))))

(test defgeneric
  "Test extracting environment information in DEFGENERIC"

  (is (info= '(:lexical t ((type . integer))) (test-generic 100))))

(test defmethod
  "Test extracting environment information in DEFMETHOD"

  (is (info= '(:lexical t ((type . string))) (test-generic "hello"))))

(test defparameter
  "Test extracting environment information in DEFPARAMETER"

  (is (info= '(:lexical t ((type . null))) *test-param*)))

(test defvar
  "Test extracting environment information in DEFPARAMETER"

  (is (info= '(:lexical t ((type . null))) *test-var*)))

(test (declaim :compile-at :run-time)
  "Test that global declarations are added to environment"

  ;; Fails on CMUCL, where the native DECLARATION-INFORMATION function
  ;; is used.

  (is (member 'my-declaration
	      (first (info declaration declaration)))))

(test (global-definitions :compile-at :run-time)
  "Test that global definitions added to environment"

  (is (info= '(:function nil nil)
	     (info function test-function)))

  (is (info= '(:macro nil nil)
	     (info function test-macro)))

  (is (info= '(:function nil nil)
	     (info function test-generic)))

  (is (info= '(:special nil nil)
	     (info variable *test-param*)))

  (is (info= '(:special nil nil)
	     (info variable *test-var*)))

  (is (info= '(:constant nil nil)
	     (info variable +a-constant+)))

  (is (info= '(:symbol-macro nil nil)
	     (info variable symbol-mac))))
