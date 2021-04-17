;;;; letstar.lisp
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

;;;; Test that environment information is extracted from LET* forms

(defpackage :cl-environments.test.full.let*-forms
  (:use :cl-environments-cl
	:cl-environments.test.full
	:fiveam))

(in-package :cl-environments.test.full.let*-forms)

(def-suite let*-forms
    :description "Test extraction of environment information from LET* forms"
    :in full-walker-test)

(in-suite let*-forms)

(defvar *global-var* "hello world")

(test let*-binding-types
  (let* ((x 1)
	 (y "hello"))
    (declare (type integer x)
	     (type string y))

    (is (info=
	 (env-info (variable-information 'x env) env)
	 '(:lexical t ((type . integer)))))

    (is (info=
    	 (env-info (variable-information 'y env) env)
    	 '(:lexical t ((type . string)))))

    (is (info=
	 (env-info (variable-information '*global-var* env) env)
	 '(:special nil nil)))

    (is (info=
    	 (env-info (variable-information 'z env) env)
    	 '(nil nil nil)))))

(test let*-binding-special
  (let* ((dvar (+ 1 2))
	 (lvar (+ 2 3))
	 (*global-var* "bye"))
    (declare (special dvar)
	     (type number lvar dvar))

    (is (info=
	 (env-info (variable-information 'dvar env) env)
	 '(:special t ((type . number)))))

    (is (info=
	 (env-info (variable-information 'lvar env) env)
	 '(:lexical t ((type . number)))))

    (is (info=
	 (env-info (variable-information '*global-var* env) env)
	 '(:special t nil)))))

(test let*-binding-dynamic-extent
  (let* ((func (lambda (a b) (+ a b))))
    (declare (dynamic-extent func)
	     (type (function (number number) number) func))

    (is (info=
	 (env-info (variable-information 'func env) env)
	 '(:lexical t ((dynamic-extent . t)
		       (type . (function (number number) number))))))))

(test let*-info-in-init-form
  (let ((outer-var (* 8 7)))
    (declare (type integer outer-var))

    (let* ((a (progn
		(is (info=
		     (env-info (variable-information 'outer-var env) env)
		     '(:lexical t ((type . integer)))))
		1))

	   (b (progn
		(is (info=
		     (env-info (variable-information 'outer-var env) env)
		     '(:lexical t ((type . integer)))))

		(is (info=
		     (env-info (variable-information 'a env) env)
		     '(:lexical t nil)))))

	  (outer-var "string"))

      (declare (special outer-var)
	       (type string outer-var)
	       (dynamic-extent outer-var))

      (is (info=
	   (env-info (variable-information 'outer-var env) env)
	   '(:special t ((dynamic-extent . t) (type . string))))))))
