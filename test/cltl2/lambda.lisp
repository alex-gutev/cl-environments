;;;; lambda.lisp
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

;;;; Test that environment information is extracted from LAMBDA forms

(defpackage :cl-environments.test.cltl2.lambda-forms
  (:use :cl-environments-cl
	:cl-environments.test.cltl2
	:fiveam))

(in-package :cl-environments.test.cltl2.lambda-forms)

(def-suite lambda-forms
    :description "Test extraction of environment information from LAMBDA forms"
    :in cltl2-test)

(in-suite lambda-forms)

(test (lambda-arg-information :compile-at :run-time)
  "Test that lambda arguments are added to environment"

  (let ((f (lambda (x)
	     (declare (ignore x))

	     (info variable x))))

    (is (info= '(:lexical t ((ignore . t)))
	       (funcall f 1)))))

(test (lambda-optional-args :compile-at :run-time)
  "Test LAMBDA form with all argument types"

  #+sbcl (declare (optimize (sb-ext:inhibit-warnings 3)))

  (let ((f (lambda (a &optional (b (info variable a)) c &rest d &key (e (info variable d) ep) ((:argf f) (info variable ep)) g &allow-other-keys &aux (h (info variable f)))
	     (declare (ignorable a b c d e ep f g h))
	     (values b e f h))))

    (multiple-value-bind (info-a info-d info-ep info-f) (funcall f 1)
      (is-every info=
	('(:lexical t nil) info-a)
	('(:lexical t nil) info-d)
	('(:lexical t nil) info-ep)
	('(:lexical t nil) info-f)))))

(test (lambda-closure :compile-at :run-time)
  "Test information of variables in LAMBDA closure"

  (let ((x 1)
	(y 2))
    (declare (type integer x y)
	     (ignorable x y))

    (let ((f (lambda (y)
	       (declare (type number y)
			(ignorable y))

	       (values
		(info variable x)
		(info variable y)))))

      (multiple-value-bind (info-x info-y) (funcall f 1)
	(is-every info=
	  ('(:lexical t ((type . integer))) info-x)
	  ('(:lexical t ((type . number))) info-y)))

      (is (info= '(:lexical t ((type . integer)))
		 (info variable y))))))

(test (lambda-with-docstring :compile-at :run-time)
  "Test that LAMBDA's with docstrings are walked correctly"

  (let ((f (lambda (x)
	     "A docstring"

	     (declare (ignore x))
	     (info variable x))))

    (is (info= '(:lexical t ((ignore . t)))
	       (funcall f 1)))))
