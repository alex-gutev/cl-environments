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
	:alexandria
	:fiveam)

  (:export :cltl2-test
	   :env-info
	   :info=))

(in-package :cl-environments.test.cltl2)

(def-suite cltl2-test
    :description "Tests for the full CLTL2 implementation using code-walker")

(in-suite cltl2-test)

(defun test-cl-environments ()
  (run! 'cltl2-test))


;;; Utilities

(defmacro env-info (form &optional (env (gensym "ENV")))
  (with-gensyms (get-info)
    `(macrolet ((,get-info (&environment ,env)
		  `',(multiple-value-list ,form)))
       (,get-info))))

(defun decl= (got expected)
  (loop
     for (key . value) in expected
     for assoc = (assoc key got)
     always (and assoc (equal (cdr assoc) value))))

(defun info= (got expected)
  (destructuring-bind (&optional got-type got-local got-decls) got
    (destructuring-bind (exp-type exp-local exp-decls) expected
      (and (eq got-type exp-type)
	   (eq got-local exp-local)
	   (decl= got-decls exp-decls)))))
