;;;; labels.lisp
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

;;;; Test that environment information is extracted from LABELS forms.

(defpackage :cl-environments.test.cltl2.labels-forms
  (:use :cl-environments-cl
	:cl-environments.test.cltl2
	:fiveam))

(in-package :cl-environments.test.cltl2.labels-forms)

(def-suite labels-forms
    :description "Test extraction of environment information from LABELS forms"
    :in cltl2-test)

(in-suite labels-forms)

(defun global-fn (a b c)
  (/ (* a b) c))

(declaim (ftype (function (integer integer integer) number) global-fn))

(defmacro test-macro (form)
  form)

(test function-types
  :description "Test extracting function information"

  (labels ((inc (a)
	     (1+ a))

	   (add (a b)
	     (+ a b)))

    (declare (ftype (function (integer) integer) inc)
	     (ftype (function (number number) number) add))

    (declare (dynamic-extent #'inc)
	     (inline add)
	     (notinline global-fn))

    (is (info=
	 (env-info (function-information 'inc env) env)
	 #-cmucl
	 '(:function t ((ftype . (function (integer) integer))
			(dynamic-extent . t)))

	 #+cmucl
	 '(:function t ((ftype . (function (integer) integer))))))

    (is (info=
	 (env-info (function-information 'add env) env)
	 '(:function t ((ftype . (function (number number) number))
			(inline . inline)))))

    (is (info=
	 (env-info (function-information 'global-fn env) env)
	 ;; For some reason CCL doesn't store global declarations.

	 #-ccl '(:function nil ((ftype . (function (integer integer integer) number))
				(inline . notinline)))
	 #+ccl '(:function nil ((inline . notinline)))))

    (is (info=
	 (env-info (function-information 'test-macro env) env)
	 '(:macro nil nil)))

    (is (info=
	 (env-info (function-information 'cl:defun env) env)
	 '(:macro nil nil)))

    (is (info=
	 (env-info (function-information 'cl:if env) env)
	 '(:special-form nil nil)))

    (is (info=
	 (env-info (function-information 'not-a-function env) env)
	 '(nil nil nil)))))

(test shadowing
  :description "Test lexical shadowing of functions"

  (labels ((f2 (a b)
	     (+ a b)))
    (declare (ftype (function (number number) number) f2)
	     (inline f2))

    (labels ((f1 (x)
	       (declare (type integer x)
			(ignore x))

	       (values
		(env-info (variable-information 'x env) env)
		(env-info (function-information 'f2 env) env)
		(env-info (function-information 'global-fn env) env)))

	     (f2 (a b)
	       (declare (ignore a b))

	       (values
		(env-info (variable-information 'a env) env)
		(env-info (variable-information 'b env) env)
		(env-info (function-information 'f1 env) env)))

	     (global-fn (x) x))

      (declare (notinline f2)
	       (ignore #'global-fn))

      (multiple-value-bind (info-x info-f2 info-global-fn)
	  (f1 1)

	(is-every info=
	  (info-x '(:lexical t ((type . integer) (ignore . t))))
	  (info-f2 '(:function t nil))
	  (info-global-fn '(:function t nil))))

      (multiple-value-bind (info-a info-b info-f1)
	  (f2 1 2)

	(is-every info=
	  (info-a '(:lexical t ((ignore . t))))
	  (info-b '(:lexical t ((ignore . t))))
	  (info-f1 '(:function t nil))))

      (is (info=
	   (env-info (function-information 'f2 env) env)
      	   '(:function t ((inline . notinline)))))

      (is (info=
	   (env-info (function-information 'global-fn env) env)
	   #-(or sbcl ccl) '(:function t ((ignore . t)))
	   #+(or sbcl ccl) '(:function t nil))))))
